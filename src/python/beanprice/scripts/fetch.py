"""Read either a Beancount input file or a CSV of positions, and fetch
corresponding latest prices from the internet.
"""


import csv
import datetime
import io
import functools
from os import path
import shelve
import tempfile
from urllib.request import urlopen
import sys
import urllib
import hashlib
import argparse
import logging

from beancount.core import data
from beancount.core import amount
from ledgerhub.ledger.provider import create_provider_from_filename
from ledgerhub.ledger.provider import create_provider_from_format
#from ledgerhub.prices import yahoo_finance
from ledgerhub.prices import google_finance



def read_positions_from_csv(filename):
    """Read the list of positions from an temporary CSV file.

    The temporary file can be created by using --dump.

    Args:
      filename: A string, the name of the CSV file.
    Returns:
      A list of pairs of (currency, cost-currency) strings.
    """
    csv_reader = csv.DictReader(open(filename))
    return [(position['currency'],position['cost_currency'])
            for position in list(csv_reader)]


def read_positions_from_provider(filename):
    """Read the ledger and get the list of positions from it.

    Args:
      filename: A string, the ledger file to load and process.
    Returns:
      A list of pairs of (currency, cost-currency) strings.
    """
    provider = create_provider_from_filename(filename)
    return provider.get_commodities()



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



def get_positions_from_file(filename):
    """Read either a CSV file or a ledger input and return a list of positions.

    Args:
      filename: A string, the name of the file to load.
    Returns:
      A sequence of (base, quote) pairs of strings.
    """
    if path.splitext(filename)[1] == '.csv':
        read_instruments = read_positions_from_csv
    else:
        read_instruments = read_positions_from_provider
    return set(read_instruments(filename))


SKIP = object()


def fetch(instruments, fetchers):
    """Fetch the latest prices for a list of instruments.

    Args:
      instruments: A sequence of pairs of (base, quote) strings to fetch.
      fetchers: A mapping of (base, quote) pairs to explicitly-created fetcher
        objects. If a (base, quote) pair is not present in the mapping, a
        GoogleFinancePriceFetcher is created with default settings. This
        allows you to override the exchange or asset class being used to
        lookup the value in Google Finance, or specify an alternative source
        for the data. You may also set the fetcher value to 'SKIP', which
        tells the price fetcher not to try to fetch a price for this instrument.
        This is used for oddball user-defined instruments that may appears in
        one's ledger file, instruments with no price available to fetch on markets.
    Returns:
      A list of Price directives instances.
    """
    price_entries = []
    price_map = {}
    for currency, cost_currency in instruments:
        # print('------------------------------------------------------------------------------------------------------------------------')
        # print(currency, cost_currency)

        base_quote = (currency,
                      cost_currency if cost_currency != currency else None)

        try:
            fetcher = fetchers[base_quote]
            if fetcher is SKIP:
                continue
        except KeyError:
            logging.warn("Using default price fetcher for {}".format(base_quote))
            fetcher = google_finance.GoogleFinancePriceFetcher(currency,
                                                               currency,
                                                               cost_currency)

        price, time = fetcher.get_latest_price()
        if price is None:
            logging.error("No price could be found for {}".format(base_quote))
            continue

        base, quote = fetcher.base, fetcher.quote

        fileloc = data.new_metadata('<{}>'.format(type(fetcher).__name__), 0)
        price_entries.append(
            data.Price(fileloc, time.date(), base, amount.Amount(price, quote)))

    return price_entries


def load_fetch_and_print(filename, fetchers, output, cache=True):
    """Load a ledger or CSV file and fetch and print prices for it.

    Args:
      filename: A string, the filename of a ledger or CSV file with holdings.
      fetchers: A dict of custom fetchers. See fetch() for details.
      output: A file object to file the entries to.
      cache: A boolen, true if we're to use the cache.
    """
    # Get the list of instruments to price.
    instruments = get_positions_from_file(filename)

    if cache:
        orig_urlopen = urllib.request.urlopen
        urllib.request.urlopen = memoize_fileobj(retrying_urlopen)

    # Fetch the entries.
    new_entries = fetch(sorted(instruments), fetchers)

    if cache:
        urllib.request.urlopen = orig_urlopen

    ## FIXME: Find a way to make the format configurable.
    provider = create_provider_from_format('beancount')
    for entry in new_entries:
        output.write(provider.format_entry(entry))


# Example fetchers configuration:
#
# Fetcher = google_finance.GoogleFinancePriceFetcher
# fetchers = {
#     ('EUR', None): Fetcher('EURUSD', 'EUR', 'USD'),
#     ('USD', None): SKIP,
#     ...
#     }

def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('filename',
                        help='Beancount or CSV Filename with list of positions')
    opts = parser.parse_args()

    fetchers = {}
    load_fetch_and_print(opts.filename, fetchers, sys.stdout)


"""

  bean-price file://<filename>
  bean-price price://<source>/<ticker>
  bean-price --source=<source>
  bean-price --date=...
  bean-price --no-cache  # One hour by default
  bean-price <filename>   # Default is file://

"""
