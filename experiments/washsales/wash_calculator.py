#!/usr/bin/env python3
"""Calculate wash sales.

You specify

- The Beancount input file
- A list of substantially identical stocks
- The calendar year (as an option, by default the previous year is used)

The script will then output a list of events to be analyzed for wash sales. Then
it will compute the wash sales and resulting cost bases.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime
import copy
import io
import re
import sys

from beancount.core.number import ZERO
from beancount.core.number import D
from beancount import loader
from beancount.core import data
from beancount.core import position
from beancount.core import inventory
from beancount.core import account_types
from beancount.parser import printer
from beancount.parser import options
from beancount.reports import table



# Number of days before and after that define the wash period (inclusive).
num_wash_days = (61 - 1) / 2


def book_position(self, entry_date, pos):
    """Find or create a position associated with the given lot, requiring a match
    against the date only if specified.

    Args:
      self: An Inventory that contains the current lots. IMPORTANT: This object is
        mutated in order to reflect the change.
      date: A datetime.date instance for the date of the position.
      pos: An instance of Position to insert.
    Returns:
      A list of booked Position instances, if any.
    """
    # A list of the booked positions.
    booked_pos = []

    # Ignore empty lots.
    if pos.number == ZERO:
        return booked_lots

    if pos.number > ZERO:
        # Always add the dates for augmenting lots. We also create a new lot if
        # the dates don't match.
        if pos.lot.lot_date is None:
            pos.lot = pos.lot._replace(lot_date=entry_date)
        lot = pos.lot

        # Deal with an augmenting lot.
        for pos2 in self:
            if pos2.lot == lot:
                pos2.add(pos.number)
                booked_pos.append(pos2)
                break
        else:
            # Deal with a new, created lot.
            self.append(pos)
            booked_pos.append(pos)

    else:
        pos = copy.copy(pos)
        remove_list = []

        # Deal with a reducing lot.
        lot = pos.lot
        if lot.lot_date:
            # Deal with a reducing lot requesting a specific date.
            for pos2 in self:
                if pos2.lot == lot:
                    booked_pos.append(pos2)
                    change = min(pos2.number, -pos.number)
                    pos.number += change
                    pos2.add(-change)
                    if pos2.number == ZERO:
                        remove_list.append(pos2)
                    if pos.number == ZERO:
                        break
            else:
                raise ValueError("Could not find a matching lot for {}.".format(lot))
        else:
            # Deal with a reducing lot without a date, interpret the date as a
            # wildcard.
            for pos2 in self:
                if (pos2.lot.currency == lot.currency and pos2.lot.cost == lot.cost):
                    booked_pos.append(pos2)
                    change = min(pos2.number, -pos.number)
                    pos.number += change
                    pos2.add(-change)
                    if pos2.number == ZERO:
                        remove_list.append(pos2)
                    if pos.number == ZERO:
                        break
            else:
                raise ValueError("Could not find a matching lot for {}.".format(lot))

        for pos_remove in remove_list:
            self.remove(pos_remove)

    if self.is_mixed():
        raise AssertionError("Invalid booking results in mixed inventory.")

    return booked_pos


def get_trades(entries, options_map, symbols, date_end):
    """Process a series of entries and extract a list of processable trades.

    The returned rows include computed fees and proceeds, ready to be washed.
    The list of trades includes buy and sell types.

    Args:
      entries: A list of directives to be processed.
      options_map: An options dict, as per the parser.
      symbols: A set of currency strings for substantially identical stocks.
      date_end: The cutoff date after which to stop processing transactions.
    Returns:

      XXX

    """
    acc_types = options.get_account_types(options_map)

    # Inventory of lots to accumulate.
    balances = inventory.Inventory()

    # A list of trade information.
    trades = []

    for entry in entries:
        # Skip other directives.
        if not isinstance(entry, data.Transaction):
            continue

        # Skip entries after the relevant period.
        if entry.date > date_end:
            continue

        # Skip entries not relevant to the currency.
        if not any(posting.position.lot.currency in symbols
                   for posting in entry.postings):
            continue

        # Calculate the fee amount and the total price, in order to split the
        # fee later on.
        fee_total = ZERO
        units_total = ZERO
        for posting in entry.postings:
            if account_types.get_account_type(posting.account) == acc_types.expenses:
                fee_total += posting.position.number
            if (account_types.get_account_type(posting.account) == acc_types.assets and
                posting.position.lot.cost is not None):
                units_total += posting.position.number

        # Loop through the postings and create trade entries for them, computing
        # proceeds and fees and all details required to wash sales later on.
        booked = False
        for posting in entry.postings:
            pos = posting.position
            if pos.lot.currency not in symbols:
                continue

            # Check that all sales have the sale price attached to them.
            if pos.number < ZERO:
                assert posting.price or re.search('Split', entry.narration, re.I)

            # Update the shared inventory.
            booked = True
            booked_pos = book_position(balances, entry.date, pos)

            # Determine the dates.
            if pos.number > ZERO:
                txn_type = 'BUY'
                acq_date = entry.date
                adj_acq_date = None
                sell_date = None
                number = pos.number
                cost = pos.lot.cost

                price = ''

                partial_fee = ''
                proceeds = ''
                pnl = ''

            else:
                # This only holds true because we book lot sales individually.
                assert len(booked_pos) <= 1, "Internal error."
                booked_position = booked_pos[0]

                txn_type = 'SELL'
                assert pos.number < ZERO
                acq_date = booked_position.lot.lot_date
                adj_acq_date = None
                sell_date = entry.date
                number = pos.number
                cost = pos.lot.cost
                price = posting.price if posting.price else cost

                partial_fee = fee_total * (number / units_total).quantize(D('0.01'))
                proceeds = -number * price.number - partial_fee
                pnl = proceeds - cost_basis

            cost_basis = -number * cost.number

            trades.append((txn_type,
                           acq_date,
                           adj_acq_date,
                           sell_date,
                           number,
                           pos.lot.currency,
                           cost.number,
                           cost_basis,
                           price.number if price else '',
                           proceeds,
                           partial_fee,
                           pnl))

        if booked:
            printer.print_entry(entry)
            for pos in balances:
                number, rest = str(pos).split(' ', 1)
                print('    {:>16} {}'.format(number, rest))
            print()
            print()

    return trades


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename',
                        help='Beancount input filename to process')

    parser.add_argument('symbols', nargs='+',
                        help='Substantially identical stock symbols.')

    parser.add_argument('-y', '--year', action='store', type=int,
                        default=datetime.date.today().year-1,
                        help="Calendar year to analyze")

    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)

    trades = get_trades(entries, options_map, set(args.symbols), datetime.date(args.year+1, 1, 1))

    field_spec = list(enumerate('type acq_date adj_acq_date sell_date number currency cost cost_basis price proceeds fee pnl'.split()))
    table_ = table.create_table(trades, field_spec)
    table.render_table(table_, sys.stdout, 'text')

    table.render_table(table_, open('/tmp/washsales.csv', 'w'), 'csv')


if __name__ == '__main__':
    main()
