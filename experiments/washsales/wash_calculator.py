#!/usr/bin/env python3
"""Calculate wash sales.

You specify

- The Beancount input file
- A list of substantially identical stocks
- The calendar year (as an option, by default the previous year is used)

The script will then output a list of events to be analyzed for wash sales. Then
it will compute the wash sales and resulting cost bases.
"""

import collections
import datetime
import io

from beancount.core.amount import ZERO
from beancount import loader
from beancount.core import data
from beancount.core import position
from beancount.core import inventory
from beancount.parser import printer



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
      An pair of
        found: An instance of Position, either the position that was found, or a new
          Position instance that was created for this lot.
        created: A boolean, true if the position had to be created.
    """
    # Ignore empty lots.
    if pos.number == ZERO:
        return

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
                break
        else:
            # Deal with a new, created lot.
            self.append(pos)

    else:
        remove_list = []

        # Deal with a reducing lot.
        lot = pos.lot
        if lot.lot_date:
            # Deal with a reducing lot requesting a specific date.
            for pos2 in self:
                if pos2.lot == lot:
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
    symbols = set(args.symbols)

    entries, errors, options_map = loader.load_file(args.filename)

    date_end = datetime.date(args.year+1, 1, 1)

    # Inventory of lots to accumulate.
    balances = inventory.Inventory()

    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        if entry.date > date_end:
            continue

        for posting in entry.postings:
            pos = posting.position
            if pos.lot.currency in symbols:
                book_position(entry.date, pos, balances)

                oss = io.StringIO()
                printer.print_entry(entry, file=oss)
                header = oss.getvalue().splitlines()[0]
                print(header)
                printer.print_entry(posting)
                print()
                for pos in balances:
                    number, rest = str(pos).split(' ', 1)
                    print('    {:>16} {}'.format(number, rest))
                print()


if __name__ == '__main__':
    main()
