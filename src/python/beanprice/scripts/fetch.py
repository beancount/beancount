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
import re
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


def memoize_recent(function, cache_filename):
    """Memoize recent calls to the given function.

    Args:
      function: A callable object.
      cache_filename: A string, the path to the database file to cache to.
    Returns:
      A memoized version of the function.
    """
    urlcache = shelve.open(cache_filename)
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


def parse_ticker(ticker, default_source='google'):
    """Given a ticker, parse out its components.

    The grammar follows this syntax: (SOURCE/)?(^)?SYMBOL

    For example, parsing 'MSFT' would return ('yahoo', 'MSFT', False)
    with a default_source = 'yahoo'.

    Another example is parsing 'google/^CURRENCY:USDCAD' which would
    ('google', 'CURRENCY:USDCAD', True).

    Args:
      ticker: A string, the ticker to parse.
      default_source: The default value for the source of the ticker to fetch,
        if unspecified.
    Returns:
      A tuple of
        source: A string, the source where to fetch the price from.
        symbol: A string, the unique source-specific symbol to use.
        invert: A boolean, true if we need to invert the price to be fetched.
    """
    match = re.match(r'(?:([a-z]+)/)?(\^)?([A-Z0-9:\-_]+)$', ticker)
    if match is None:
        raise ValueError('Invalid ticker: "{}"'.format(ticker))
    source, invert, symbol = match.group(1, 2, 3)
    invert = bool(invert)
    source = source or default_source
    return source, symbol, invert


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
        source, symbol, invert = parse_ticker(ticker, default_source)

        # Select the quote currency if declared, otherwise use the cost
        # currency.
        base = currency
        quote = quote_currency or cost_currency

        jobs.append(
            Job(source, symbol, date, base, quote, invert))

    return jobs


def process_jobs(jobs, source_map):
    """Run the given jobs using modules from the given source map.

    Args:
      jobs: A list of Job instances.
      source_map: A mapping of source string to a source module object.
    Returns:
      A list of
    """
    for job in jobs:
        logging.info(job)

        source_module = source_map[job.source]
        if job.date is None:
            price, time = source_module.get_latest_price(job.symbol)
        else:
            price, time = source_module.get_historical_price(job.symbol, job.date)
        print(price, time)


#         fileloc = data.new_metadata('<{}>'.format(type(fetcher).__name__), 0)
#         price_entries.append(
#             data.Price(fileloc, time.date(), base, amount.Amount(price, quote)))
#
#     return price_entries


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

    parser.add_argument('uri_list', nargs='+',
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
            source, symbol, invert = parse_ticker(''.join((parsed_uri.netloc,
                                                           parsed_uri.path)))
            jobs.append(
                Job(source, symbol, args.date, None, None, invert))

        # Parse symbols from a file.
        elif parsed_uri.scheme in ('file', ''):
            filename = parsed_uri.path
            if not (path.exists(filename) and path.isfile(filename)):
                parser.error('File does not exist: "{}"'.format(filename))
            jobs.extend(
                get_jobs_from_file(filename, args.date, args.source))
        else:
            parser.error('Invalid scheme "{}"'.format(parsed_uri.scheme))

    # Validate price sources.
    for job in jobs:
        if job.source not in valid_price_sources:
            parser.error('Invalid source "{}"'.format(job.source))

    return jobs, args.do_cache


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    source_modules = [google_finance, yahoo_finance]

    # Parse the arguments.
    source_names = [module.__source_name__ for module in source_modules]
    jobs, do_cache = process_args(sys.argv[1:], price_source_names)

    # Install the cache.
    if do_cache:
        cache_filename = path.join(tempfile.gettempdir(),
                                   path.basename(sys.argv[0]))
        urllib.request.urlopen = memoize_recent(retrying_urlopen)

    # Process the jobs.
    source_map = {module.__source_name__: module for module in source_modules}
    process_jobs(jobs, source_map)

    # price_entries = []
    #     # Invert the currencies if the rate is to be inverted.
    #     if job.invert:
    #         ticker = ticker[2:]
    #         currency, quote_currency = quote_currency, currency
    #     fileloc = data.new_metadata('<fetch-prices>', 0)
    #     price_entries.append(
    #         data.Price(fileloc, price_time.date(), currency, amount.Amount(price, quote_currency)))
    # printer.print_entries(price_entries)
