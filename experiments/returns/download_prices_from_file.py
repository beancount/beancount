#!/usr/bin/env python3
"""Download required prices from Price directives given in another file.

This is used to fetch missing prices identified from running the compute_returns
script.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"


import datetime
import argparse
import logging

from dateutil import tz

from beancount import loader
from beancount.core import data
from beancount.parser import printer

from beanprice.sources import yahoo


def main():
    """Top-level function."""
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('price_ledger',
                        help="Ledger file containing a list of prices to fetch")
    parser.add_argument('-v', '--verbose', action='store_true')
    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')

    # Load the example file.
    logging.info("Reading ledger: %s", args.price_ledger)
    entries, _, _ = loader.load_file(args.price_ledger)

    source = yahoo.Source()

    new_entries = []
    for price_entry in entries:
        assert isinstance(price_entry, data.Price)

        time = datetime.datetime.combine(price_entry.date, datetime.time(),
                                         tzinfo=tz.tzutc())
        try:
            srcprice = source.get_historical_price(price_entry.currency, time)
            new_entry = price_entry._replace(
                amount=price_entry.amount._replace(number=srcprice.price))
            new_entries.append(new_entry)
        except yahoo.YahooError as exc:
            print("ERROR: {}".format(exc))

    printer.print_entries(new_entries)


if __name__ == '__main__':
    main()
