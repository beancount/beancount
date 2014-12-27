"""Given a Beancount ledger, compute time intervals where we hold each commodity.

This script computes, for each commodity, which time intervals it is required at.
This can then be used to identify a list of dates at which we need to fetch prices
in order to properly fill the price database.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import collections
import datetime

from beancount import loader
from beancount.core import inventory
from beancount.core import data


ONEDAY = datetime.timedelta(days=1)


def get_time_intervals(entries):
    """Given a list of directives, figure out the life of each commodity.

    Args:
      entries: A list of directives.
    Returns:
      A dict of commodity strings to lists of (start, end) datetime.date pairs.
    """
    lifetimes = collections.defaultdict(list)

    # The current set of active commodities.
    commodities = set()

    # The current balances across all accounts.
    balances = collections.defaultdict(inventory.Inventory)

    for entry in entries:
        # Process only transaction entries.
        if not isinstance(entry, data.Transaction):
            continue

        # Update the balance of affected accounts and check locally whether that
        # triggered a change in the set of commodities.
        commodities_changed = False
        for posting in entry.postings:
            balance = balances[posting.account]
            commodities_before = balance.keys()
            balance.add_position(posting.position)
            commodities_after = balance.keys()
            if commodities_after != commodities_before:
                commodities_changed = True

        # If there was a change in one of the affected account's list of
        # commodities, recompute the total set globally. This should not
        # occur very frequently.
        if commodities_changed:
            new_commodities = get_all_commodities(balances.values())
            if new_commodities != commodities:
                # The new global set of commodities has changed; update our
                # the dictionary of intervals.
                for currency in new_commodities - commodities:
                    lifetimes[currency].append((entry.date, None))

                for currency in commodities - new_commodities:
                    lifetime = lifetimes[currency]
                    begin_date, end_date = lifetime.pop(-1)
                    assert end_date is None
                    lifetime.append((begin_date, entry.date + ONEDAY))

                # Update our current set.
                commodities = new_commodities

                ##print(entry.date, ','.join(commodities))

    return lifetimes


def get_all_commodities(inventories):
    """Given a list of inventories, compute the total set of commodities.

    Args:
      inventories: An iterable of Inventory instances.
    Returns:
      A set of commodities.
    """
    commodities = set()
    for inv in inventories:
        commodities.update(inv.keys())
    return commodities


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount filename to parse')
    args = parser.parse_args()

    # Load the Beancount input file.
    entries, errors, options_map = loader.load_file(args.filename)

    lifetimes = get_time_intervals(entries)
    ccywidth = max(map(len, lifetimes.keys())) if lifetimes else 1
    for currency, lifetime in sorted(lifetimes.items(), key=lambda x: x[1][0][0]):
        print('{:{width}}: {}'.format(currency,
                                      ' / '.join('{}-{}'.format(begin, end or '...')
                                                 for begin, end in lifetime),
                                      width=ccywidth))



if __name__ == '__main__':
    main()
