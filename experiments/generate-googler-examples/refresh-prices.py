#!/usr/bin/env python3
"""Fetch all the price directives found in the given Beancount file.
"""
import logging
import argparse

from beancount import loader
from beancount.parser import printer
from beancount.core import data

from ledgerhub.prices import google_finance


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Filename')
    args = parser.parse_args()

    entries, _, __ = loader.load_file(args.filename, log_errors=logging.error)
    printer.print_entries(entries)

    for entry in entries:
        if not isinstance(entry, data.Price):
            continue

        currency, cost_currency = entry.currency, entry.amount.currency
        fetcher = google_finance.GoogleFinancePriceFetcher(currency, currency, cost_currency)
        price, price_date = fetcher.get_historical_price(entry.date)
        print("{{{:7.2f} {}/ {}}}".format(price * 2, cost_currency, entry.date))



if __name__ == '__main__':
    main()
