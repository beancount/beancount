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
from beancount.parser import printer



# Number of days before and after that define the wash period (inclusive).
num_wash_days = (61 - 1) / 2


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
    inventory_list = []

    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        if entry.date > date_end:
            continue

        for posting in entry.postings:
            pos = posting.position
            if pos.lot.currency in symbols:
                if pos.number > ZERO and pos.lot.lot_date is None:
                    pos.lot = pos.lot._replace(lot_date=entry.date)
                inventory_list.append(pos)

                oss = io.StringIO()
                printer.print_entry(entry, file=oss)
                header = oss.getvalue().splitlines()[0]
                print(header)
                printer.print_entry(posting)
                print()
                for pos in inventory_list:
                    print('    {}'.format(pos))
                print()


if __name__ == '__main__':
    main()
