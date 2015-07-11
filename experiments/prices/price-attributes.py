#!/usr/bin/env python3
"""Print metadata attributes used for the price fetcher.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

from pprint import pprint

from beancount import loader
from beancount.core import getters


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Filename')
    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)

    commodity_map = getters.get_commodity_map(entries, options_map)
    ticker_info = getters.get_values_meta(commodity_map, 'name', 'ticker', 'quote')

    print('Fetching:')
    for currency, (name, ticker, cost_currency) in sorted(ticker_info.items()):
        if ticker:
            print('{:16} {:16} {:16} {}'.format(currency, ticker, cost_currency, name))
    print()

    print('Skipping:')
    for currency, (name, ticker, cost_currency) in sorted(ticker_info.items()):
        if not ticker:
            print('{:16} {:16} {:16} {}'.format(currency, '',
                                                cost_currency or '',
                                                name or ''))


if __name__ == '__main__':
    main()
