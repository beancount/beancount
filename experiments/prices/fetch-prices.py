#!/usr/bin/env python3
"""Fetch prices from Beancount holdings at a particular date.

This script will load a Beancount file, figure out the list of commodities held
at a particular date, extract the ticker symbols for those commodities and fetch
the prices from Google Finance.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import re
import datetime
from pprint import pprint
from urllib import request
from urllib import parse

from beancount.core.number import D
from beancount.core import amount
from beancount.core import getters
from beancount.core import data
from beancount.ops import holdings
from beancount.parser import printer
from beancount import loader


def get_google_finance_latest_price(ticker):
    """Return the latest price found for the symbol.

    Args:
      symbol: An 'ExchangeCode:Symbol' string that is the unambiguous ticker
        for the particular financial instrument to query.
    Returns:
      A pair of a price (a Decimal object) and the actual date of that price
      (a datetime.datetime instance).
    """
    if ':' in ticker:
        exchange_code, symbol = ticker.split(':')
    else:
        exchange_code, symbol = None, ticker

    # Build the query.
    params_dict = {
        'q': symbol,
        'f': 'd,c', # Date,Close
    }
    if exchange_code:
        params_dict['x'] = exchange_code

    # Always reach back 5 days in time because of long weekends.
    if exchange_code in ('MUTF', 'MUTF_CA'):
        params_dict['p'] = '5d'
    else:
        params_dict['p'] = '5d'
        params_dict['i'] = 300 # secs, to get the most recent.

    url = 'http://www.google.com/finance/getprices?{}'.format(
        parse.urlencode(sorted(params_dict.items())))

    # Fetch the data.
    data = request.urlopen(url).read().decode('utf-8')
    data = parse.unquote(data).strip()

    # Process the meta-data.
    metadata = {}
    lines = data.splitlines()
    for index, line in enumerate(lines):
        mo = re.match('([A-Z_+]+)=(.*)$', line)
        if not mo:
            break
        metadata[mo.group(1)] = mo.group(2)
    else:
        # No data was found.
        return None, None

    interval = int(metadata['INTERVAL'])
    data_lines = lines[index:]
    for line in data_lines:
        if re.match('TIMEZONE_OFFSET', line):
            continue
        time_str, price_str = line.split(',')

        mo = re.match('a(\d+)', time_str)
        if mo:
            time_marker = datetime.datetime.fromtimestamp(int(mo.group(1)))
            time = time_marker
        else:
            seconds = int(time_str) * interval
            time = time_marker + datetime.timedelta(seconds=seconds)

        price = D(price_str)

    return (price, time)


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename', help='Beancount input filename')

    # Note: Historical data is not supported yet because Google Finance will not
    # serve us historical currency rates.
    #
    # parser.add_argument('-d', '--date', action='store', type=parse_date,
    #                     help="The date at which to extract the holdings")

    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)

    price_entries = []
    commodities_list = holdings.get_commodities_at_date(entries, options_map)
    for currency, cost_currency, quote_currency, ticker in commodities_list:
        # Ignore the commodity if it has no ticker defined on it.
        if ticker is None:
            continue

        # Select the quote currency if declared, otherwise use the cost
        # currency.
        quote_currency = quote_currency or cost_currency

        # Invert the currencies if the rate is to be inverted.
        if ticker.startswith('1/'):
            ticker = ticker[2:]
            currency, quote_currency = quote_currency, currency

        price, price_time = get_google_finance_latest_price(ticker)

        fileloc = data.new_metadata('<fetch-prices>', 0)
        price_entries.append(
            data.Price(fileloc, price_time.date(), currency, amount.Amount(price, quote_currency)))

    printer.print_entries(price_entries)


if __name__ == '__main__':
    main()
