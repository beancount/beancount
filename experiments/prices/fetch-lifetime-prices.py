#!/usr/bin/env python3
"""A script that will fetch missing prices.
"""
__author__ = "Martin Blais <blais@furius.ca>"

from dateutil.parser import parse as parse_datetime

from beancount.core.number import ZERO
from beancount.core.amount import Amount
from beancount import loader
from beancount.ops import lifetimes
from beancount.parser import printer
from beancount.core import data


def main():
    parse_date = lambda s: parse_datetime(s).date()

    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Ledger filename')
    parser.add_argument('-c', '--compress-days', action='store', type=int, default=60,
                        help="The number of unused days to ignore.")
    parser.add_argument('-m', '--min-date', action='store', type=parse_date, default=None,
                        help="The minimum date to consider")
    args = parser.parse_args()

    # Load the ledger.
    entries, errors, options_map = loader.load_file(args.filename)

    # Build a map of existing price entries.
    price_map = {}
    for entry in entries:
        if isinstance(entry, data.Price):
            key = (entry.date, entry.currency, entry.amount.currency)
            price_map[key] = entry

    # Compute the lifetimes of currencies and compress them.
    lifetimes_map = lifetimes.get_commodity_lifetimes(entries)
    lifetimes_map = lifetimes.compress_lifetimes_days(lifetimes_map, args.compress_days)

    # Create price directives for missing prices.
    prices = []
    for key in lifetimes.required_weekly_prices(lifetimes_map,
                                                entries[-1].date):
        # If the price entry is already in the ledger, ignore it.
        if key in price_map:
            continue
        date, currency, cost_currency = key

        # Ignore entries too early.
        if args.min_date is not None and date < args.min_date:
            continue

        # Ignore entries with an empty cost currency.
        if cost_currency is None:
            continue

        # Create a price directive.
        price = data.Price(data.new_metadata(__file__ ,0),
                           date, currency, Amount(ZERO, cost_currency))
        prices.append(price)

    # For now, just print those out.
    printer.print_entries(prices)


if __name__ == '__main__':
    main()
