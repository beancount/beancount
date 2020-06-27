#!/usr/bin/env python3
"""Check the timeline of each price for anomalies.

Anomalies in price data entry can be common, e.g., inputting an inverse currency
rate, or importing an invalid price due to a duplicate or invalid stock symbol.
"""
__copyright__ = "Copyright (C) 2019  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import logging
import re

from pprint import pprint

from beancount import loader
from beancount.core import getters
from beancount.core import prices


def is_option(currency):
    return bool(re.match(r'[A-Z]+O-\d\d\d\d\d\d$', currency) or
                re.match(r'[A-Z]{3,}\d{6}[CP]\d+(\.\d+)?', currency))


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Filename')
    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)

    price_map = prices.build_price_map(entries)
    # commodity_map = getters.get_commodity_map(entries, options_map)
    # ticker_info = getters.get_values_meta(commodity_map, 'name', 'ticker', 'quote')
    #pprint(price_map)

    # print('Fetching:')
    diff_threshold = 2.00  # pct, regardless of time.

    for (base, quote), rates in sorted(price_map.items()):
        if is_option(base) or is_option(quote):
            continue
        if not rates:
            continue
        print(base, quote)
        riter = iter(rates)
        prev_date, prev_rate = next(riter)
        for date, rate in riter:
            days = (date - prev_date).days
            diff_pct = (float(rate / prev_rate) - 1.0) / days * 100
            if abs(diff_pct) > diff_threshold:
                print('  {:3d}  {:.5f} {:6.2f}'.format(days, rate, diff_pct))
            prev_date, prev_rate = date, rate


if __name__ == '__main__':
    main()
