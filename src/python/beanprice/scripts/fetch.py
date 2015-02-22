"""Fetch prices from the internet and output them in Beancount format.

This script accepts a list of URIs that specify what prices to fetch and from
where. It attempts a list of either a Beancount input filename:

  bean-price file://<filename>

e.g.,

  bean-price file:///path/to/my/file.beancount


Or explicit price sources:

  bean-price price://<source>/<ticker>

e.g.,

  bean-price price://google/TSE:XUS
  bean-price price://yahoo/AAPL

Without a URI prefix, the input is interpreted as a filename:

  bean-price /path/to/my/file.beancount


You can specify a date for the data like this:

  bean-price --date=2015-02-03

If you don't specify a date, it attempts to fetch the current/latest available
data from the source. IMPORTANT: Note that each source may support a different
routine for getting its latest data and for fetching historical/dated data, and
that each of these may differ in their support. For example, Google Finance does
not support fetching historical data for its CURRENCY:* instruments.

If you use a Beancount filename, the corresponding Ledger is loaded and the list
of holdings is computed at the specified date (if no date is specified, the
final list of holdings is used). For each of the holdings, the corresponding
Commodity directives are consulted and their "ticker" metadata fields are used
if present as the ticker symbols whose prices to fetch. You should have
directives like this in your input file:

  2007-07-20 commodity VEA
    ticker: "NYSEARCA:VEA"

So if there is a holding in units of "VEA" at the requested date, this would
fetch a Price entry for "price://google/NYSEARCA:VEA".

Values for ticker fields may be prefixed with their source:

  2007-07-20 commodity VEA
    ticker: "price://google/NYSEARCA:VEA"

The default source if left unspecified is Google Finance ("google"). You may
specify an alternate default price source using the --source option:

  bean-price --source=yahoo ...

Note that this will not override explicitly specified price sources.

Finally, prices are automatically cached at a resolution of one hour. You can
disable the cache with an option:

  bean-price --no-cache

"""
__author__ = "Martin Blais <blais@furius.ca>"

import csv
import collections
import datetime
import io
import functools
from os import path
import shelve
import tempfile
from urllib.request import urlopen
import sys
import urllib
import urllib.parse
import hashlib
import argparse
import logging

from dateutil.parser import parse as parse_datetime

from beancount import loader
from beancount.core import data
from beancount.core import amount
from beancount.ops import holdings
from beanprice.sources import yahoo_finance
from beanprice.sources import google_finance


def memoize_fileobj(function):
    urlcache = shelve.open(path.join(tempfile.gettempdir(),
                                     '{}.db'.format(function.__name__)))
    @functools.wraps(function)
    def memoized(*args, **kw):
        md5 = hashlib.md5()
        md5.update(str(datetime.date.today()).encode('utf-8'))
        md5.update(str(args).encode('utf-8'))
        md5.update(str(sorted(kw.items())).encode('utf-8'))
        hash_ = md5.hexdigest()
        try:
            contents = urlcache[hash_]
        except KeyError:
            fileobj = function(*args, **kw)
            if fileobj:
                contents = fileobj.read()
                urlcache[hash_] = contents
            else:
                contents = None
        return io.BytesIO(contents) if contents else None
    return memoized


def retrying_urlopen(url, timeout=5):
    """Open and download the given URL, retrying if it times out.

    Args:
      url: A string, the URL to fetch.
      timeout: A timeout after which to stop waiting for a respone and return an
        error.
    Returns:
      The contents of the fetched URL.
    """
    while 1:
        response = urlopen(url, timeout=timeout)
        if response:
            break
    if response.getcode() != 200:
        return None
    return response





# SKIP = object()
#
#
# def fetch(instruments, fetchers):
#     """Fetch the latest prices for a list of instruments.
#
#     Args:
#       instruments: A sequence of pairs of (base, quote) strings to fetch.
#       fetchers: A mapping of (base, quote) pairs to explicitly-created fetcher
#         objects. If a (base, quote) pair is not present in the mapping, a
#         GoogleFinancePriceFetcher is created with default settings. This
#         allows you to override the exchange or asset class being used to
#         lookup the value in Google Finance, or specify an alternative source
#         for the data. You may also set the fetcher value to 'SKIP', which
#         tells the price fetcher not to try to fetch a price for this instrument.
#         This is used for oddball user-defined instruments that may appears in
#         one's ledger file, instruments with no price available to fetch on markets.
#     Returns:
#       A list of Price directives instances.
#     """
#     price_entries = []
#     price_map = {}
#     for currency, cost_currency in instruments:
#         # print('------------------------------------------------------------------------------------------------------------------------')
#         # print(currency, cost_currency)
#
#         base_quote = (currency,
#                       cost_currency if cost_currency != currency else None)
#
#         try:
#             fetcher = fetchers[base_quote]
#             if fetcher is SKIP:
#                 continue
#         except KeyError:
#             logging.warn("Using default price fetcher for {}".format(base_quote))
#             fetcher = google_finance.GoogleFinancePriceFetcher(currency,
#                                                                currency,
#                                                                cost_currency)
#
#         price, time = fetcher.get_latest_price()
#         if price is None:
#             logging.error("No price could be found for {}".format(base_quote))
#             continue
#
#         base, quote = fetcher.base, fetcher.quote
#
#         fileloc = data.new_metadata('<{}>'.format(type(fetcher).__name__), 0)
#         price_entries.append(
#             data.Price(fileloc, time.date(), base, amount.Amount(price, quote)))
#
#     return price_entries


# def load_fetch_and_print(filename, fetchers, output, cache=True):
#     """Load a ledger or CSV file and fetch and print prices for it.
#
#     Args:
#       filename: A string, the filename of a ledger or CSV file with holdings.
#       fetchers: A dict of custom fetchers. See fetch() for details.
#       output: A file object to file the entries to.
#       cache: A boolen, true if we're to use the cache.
#     """
#     # Get the list of instruments to price.
#     instruments = get_positions_from_file(filename)
#
#     if cache:
#         orig_urlopen = urllib.request.urlopen
#         urllib.request.urlopen = memoize_fileobj(retrying_urlopen)
#
#     # Fetch the entries.
#     new_entries = fetch(sorted(instruments), fetchers)
#
#     if cache:
#         urllib.request.urlopen = orig_urlopen
#
#     ## FIXME: Find a way to make the format configurable.
#     provider = create_provider_from_format('beancount')
#     for entry in new_entries:
#         output.write(provider.format_entry(entry))


def get_symbol_invert(symbol_with_invert):
    """Extract the inversion token from a symbol if it has one.

    Args:
      symbol_with_invert: A string, a ticker symbol possibly prefixed
        with a token that tells us to invert its value. The token is a
        '^' prefix to the symbol, e.g., '^GBPUSD' is an inverted 'USDGBP'.
    Returns:
      The symbol without the inversion and a boolean, true if inverted.
    """
    if symbol_with_invert.startswith('^'):
        return symbol_with_invert[1:], True
    else:
        return symbol_with_invert, False


# A price fetching job description.
#
# Attributes:
#   source: A string identifier for a data source.
#   symbol: A ticker symbol in the universe of the source.
#   date: A datetime.date object for the date to be fetched, or None
#     with the meaning of fetching the latest price.
#   base: A currency, the base currency for the given symbol. This may be
#     null, in which case we simply issue the price value, not a price directive.
#   quote: A currency, the quote currency for the given syumbol.
#   invert: A boolean, true if we need to invert the currency.
Job = collections.namedtuple('Job', 'source symbol date base quote invert')


def get_jobs_from_file(filename, date, default_source):
    """Given a Beancount input file, extract price fetching jobs.

    Args:
      filename: A string, the name of the file whose prices to fetch.
      date: A datetime.date instance, the date at which to extract the
        holdings for.
      default_source: A string, the name of the default source to use if
        ticker symbols don't specify one.
    Returns:
      A list of Job instances, to be processed.
    """
    entries, errors, options_map = loader.load_file(filename)

    jobs = []
    commodities_list = holdings.get_commodities_at_date(entries, options_map, date=date)
    for currency, cost_currency, quote_currency, ticker in commodities_list:
        # Ignore the commodity if it has no ticker defined on it.
        if ticker is None:
            continue

        # Parse the source out of the ticker if possible.
        if '/' in ticker:
            source, symbol = ticker.split('/', 1)
        else:
            source, symbol = default_source, ticker

        # Select the quote currency if declared, otherwise use the cost
        # currency.
        base = currency
        quote = quote_currency or cost_currency

        # Invert the currencies if the rate is to be inverted.
        symbol, invert = get_symbol_invert(symbol)

        jobs.append(
            Job(source, symbol, date, base, quote, invert))

    return jobs


def process_args(argv, valid_price_sources):
    """Process command-line arguments and return a list of jobs to process.

    Args:
      argv: A list of command-line arguments. This is mainly allowed to be
        specified in explicitly for the benefit of unit tests.
      valid_price_sources: A list of strings, the names of valid price sources.
        The first item is taken to be the default price source.
    Returns:
      A list of Job tuples and a 'do-cache' boolean, true if we should use the
      cache.
    Raises:
      SystemExit: If something could not be parsed.
    """
    default_source = valid_price_sources[0]

    parser = argparse.ArgumentParser(description=__doc__)

    parser.add_argument('uri_list', nargs='*',
                        help='A list of URIs specifying which prices to fetch')

    parse_date = lambda s: parse_datetime(s).date()
    parser.add_argument('--date', action='store', type=parse_date,
                        help="Specify the date for which to fetch the holdings")

    parser.add_argument('--source', action='store',
                        choices=valid_price_sources, default=default_source,
                        help="Specify the default source of data for unspecified tickers.")

    parser.add_argument('--no-cache', dest='do_cache', action='store_false', default=True,
                        help="Disable the price cache")

    args = parser.parse_args(argv)

    # Prepare a list of jobs.
    jobs = []
    for uri in args.uri_list:
        parsed_uri = urllib.parse.urlparse(uri)

        # Parse an explicit price.
        if parsed_uri.scheme == 'price':
            symbol, invert = get_symbol_invert(parsed_uri.path[1:])
            jobs.append(
                Job(parsed_uri.netloc, symbol, args.date, None, None, invert))

        # Parse symbols from a file.
        elif parsed_uri.scheme in ('file', ''):
            if not path.exists(parsed_uri.path):
                parser.error('File does not exist: "{}"'.format(parsed_uri.path))
            jobs.extend(
                get_jobs_from_file(parsed_uri.path, args.date, args.source))
        else:
            parser.error('Invalid scheme "{}"'.format(parsed_uri.scheme))

    # Validate price sources.
    for job in jobs:
        if job.source not in valid_price_sources:
            parser.error('Invalid source "{}"'.format(parsed_uri.netloc))

    return jobs, args.do_cache


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    price_source_modules = [google_finance, yahoo_finance]

    # Parse the arguments.
    price_source_names = [module.name for module in price_source_modules]
    jobs, do_cache = process_args(sys.argv, price_source_names)

    # Process the jobs.
    price_source_map = {module.name: module
                        for mobule in price_source_modules}
    for job in jobs:
        print(job)
