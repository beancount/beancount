"""Read either a Beancount input file or a CSV of positions, and fetch
corresponding latest prices from the internet.
"""
import csv
from os import path
import re
import shelve
from collections import namedtuple
import tempfile

import dateutil.parser
import bs4

from beancount.ops import prices
from beancount.core import data
from beancount.core.data import FileLocation, Price
from beancount.core.amount import Decimal, Amount
from beancount.parser import printer
from beancount import load
from urllib.request import urlopen



def read_positions_from_csv(filename):
    """Read the list of positions from an temporary CSV file.

    The temporary file can be created by using --dump.

    Args:
      filename: A string, the name of the CSV file.
    Returns:
      A list of position dicts, as per is created by --dump.
    """
    csv_reader = csv.DictReader(open(filename))
    return list(csv_reader)


def write_positions_to_csv(filename, positions):
    """Write the list of positions to a CSV file.

    Args:
      filename: A string, the name of the CSV file to create.
      positions: A list of dicts with field names for positions.
    """
    fieldnames = ['account',
                  'currency',
                  'cost_currency',
                  'number',
                  'cost_number',
                  'price_number',
                  'price_date']
    csv_writer = csv.DictWriter(open(filename, 'w'), fieldnames)
    csv_writer.writeheader()
    for position in positions:
        csv_writer.writerow(position)


def read_positions_from_beancount(filename):
    """Read the beancount ledger and get the list of positions from it.

    Args:
      filename: A string, the Beancount ledger file to load and process.
    Returns:
      A list of position dicts.
    """
    entries, errors, options = load(filename, quiet=True)
    _, positions = prices.get_priced_positions(entries)
    # Fixup for accounts, convert to account names.
    for position in positions:
        position['account'] = position['account'].name
    return positions


urlcache = shelve.open(path.join(tempfile.tempdir, 'urls.db'))

def urlopen_retry(url, timeout=5):
    """Open and download the given URL, retrying if it times out.

    Args:
      url: A string, the URL to fetch.
      timeout: A timeout after which to stop waiting for a respone and return an
        error.
    Returns:
      The contents of the fetched URL.
    """
    try:
        contents = urlcache[url]
    except KeyError:
        while 1:
            response = urlopen(url, timeout=timeout)
            if response:
                break
        if response.getcode() != 200:
            contents = None
        else:
            contents = response.read()
            urlcache[url] = contents
    return contents


def fetch_google_historical(currency, cost_currency):
    """Fetch the given price from Google Finance.

    Args:
      currency: A string, the symbol of the currency/ticker to fetch
        (e.g. GOOG).
      cost_currency: A string, the symbol of the currency that 'currency'
        is expressed in (e.g. USD).
    Returns:
      A list of (date, price) pairs.
    """
    # Convert the symbol to one that Google may accept.
    if cost_currency == 'CAD':
        if re.match('RBF\d\d\d\d', currency):
            #symbol = 'MUTF_CA:{}'.format(currency)
            symbol = currency
        else:
            symbol = 'TSE:{}'.format(currency)
    elif cost_currency == 'AUD':
        symbol = None
    else:
        symbol = currency

    if symbol is None:
        return None

    #url = "http://www.google.com/ig/api?stock={}".format(quote(symbol))
    url = "http://www.google.com/finance/historical?q={}".format(symbol)
    response = urlopen_retry(url)
    if response is None:
        return None

    soup = bs4.BeautifulSoup(response, 'lxml')
    div_prices = soup.find('div', id='prices')
    if div_prices is None:
        return
    table = div_prices.find('table', {'class': 'historical_price'})
    assert table

    headers = [th.contents[0].strip()
               for th in table.find_all('th')]

    RowClass = namedtuple('Row', headers)
    price_list = []
    for tr in table.find_all('tr'):
        if 'class' in tr.attrs and 'bb' in tr['class']:
            continue

        td_contents = [td.contents[0].strip()
                       for td in tr.find_all('td')]
        dt = dateutil.parser.parse(td_contents[0]).date()
        values = [(Decimal(value.replace(',', '')) if value != '-' else None)
                  for value in td_contents[1:]]
        row = RowClass(dt, *values)
        price_list.append((row.Date, row.Close))

    return price_list


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(__doc__)

    parser.add_argument('filename', help='Beancount of CSV Filename')

    parser.add_argument('-d', '--dump', action='store',
                        help="Filename to store updated CSV positions and prices.")

    opts = parser.parse_args()

    # Read the input file.
    if path.splitext(opts.filename)[1] == '.csv':
        read_positions = read_positions_from_csv
    else:
        read_positions = read_positions_from_beancount
    positions = read_positions(opts.filename)

    # Get the list of instruments to price.
    instruments = sorted(set((position['currency'],position['cost_currency'])
                             for position in positions))

    new_entries = []
    price_map = {}
    for currency, cost_currency in instruments:
        # print('------------------------------------------------------------------------------------------------------------------------')
        # print(currency, cost_currency)

        price_list = fetch_google_historical(currency, cost_currency)
        if price_list is None:
            continue

        for date, price in price_list:
            fileloc = FileLocation('<fetch_google_historical>', 0)
            new_entries.append(
                Price(fileloc, date, currency, Amount(price, cost_currency)))

    for entry in new_entries:
        sys.stdout.write(printer.format_entry(entry))

    for position in positions:
        curkey = (position['currency'], position['cost_currency'])
        try:
            date, close = price_map[curkey]
            position['price_date'] = date
            position['price_number'] = close
        except KeyError:
            pass

    # If requested, dump the updated list of positions and prices.
    if opts.dump:
        write_positions_to_csv(opts.dump, positions)


if __name__ == '__main__':
    main()
