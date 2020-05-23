#!/usr/bin/env python3
"""Detect and list lots held at cost which augment an existing one.

We're doing this because we conjecture that we could harmlessly make a
modification to the inventory booking algorithm that only ever merge lots for
lots without cost. This lists all the occurrences where at lot at cost is being
added to.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount import loader
from beancount.core import inventory
from beancount.core import data


def detect_augmenting_lots(transactions):
    """List all augmenting lots at cost, regardless of date."""
    balances = collections.defaultdict(inventory.Inventory)
    for entry in transactions:
        for posting in entry.postings:
            running_balance = balances[posting.account]
            _, booking = running_balance.add_position(posting.position)

            # Detect cases where we're augmenting a found lot held at cost. This is
            # for an experiment: We can pose a conjecture that for lots held at
            # cost, we may never need to do lot merging. This could simplify the
            # booking algorithms.
            pos = posting.position
            if pos.lot.cost and booking == inventory.Booking.AUGMENTED:
                print('Augmenting lot held at cost {:10} {}'.format(pos.number, pos.lot))


def detect_augmenting_lots_with_dates(transactions):
    """List all augmenting lots at cost, but only on matching dates."""
    balances = collections.defaultdict(inventory.Inventory)
    for entry in transactions:
        for posting in entry.postings:
            running_balance = balances[posting.account]
            pos = posting.position
            _, booking = running_balance.add_position(pos)

            # Look for augmenting lots with an explicit date. This is done in
            # order to prepare for booking on a partial pattern for the new
            # booking system.
            lot = pos.lot
            if lot.lot_date:
                if booking != inventory.Booking.REDUCED:
                    print('Augmenting lot with a date {:10} {}'.format(pos.number, lot))


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount input filename')
    args = parser.parse_args()

    entries, _, options_map = loader.load_file(args.filename)
    transactions = [entry
                    for entry in entries
                    if isinstance(entry, data.Transaction)]

    #detect_augmenting_lots(transactions)
    detect_augmenting_lots_with_dates(transactions)


if __name__ == '__main__':
    main()
