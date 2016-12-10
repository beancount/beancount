#!/usr/bin/env python3
"""List the lots for some accounts at a particular date.

NOTE: Ideally this should be replaced by a SQL query.
bean-query just has to be improved in order to do this, and the 'booking' branch completed.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import collections
import datetime
import logging
import os
import re
import sys
from os import path

from beancount.parser import printer
from beancount.reports import table
from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core import data
from beancount.core import realization
from beancount.utils import misc_utils
from beancount.utils import date_utils
from beancount import loader


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename',
                        help='Beancount input file')
    parser.add_argument('accounts', nargs='+',
                        help='Account names')

    parser.add_argument('--date', type=date_utils.parse_date_liberally,
                        help="Date")

    parser.add_argument('-o', '--output', action='store',
                        help="Output directory for the CSV files")

    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)

    # Filter out anything after the given date.
    if args.date is None:
        args.date = entries[-1].date
    entries = [entry for entry in entries if entry.date < args.date]

    # Compute the balance in each account and process it.
    real_root = realization.realize(entries)
    rows = []
    fieldspec = list(enumerate(['Vest Date', 'Units', 'Instrument', 'Cost']))
    for account in args.accounts:
        real_acc = realization.get(real_root, account)
        if real_acc is None:
            logging.error("Account '%s' does not exist", account)
            continue
        for position in real_acc.balance:
            rows.append((position.cost.date,
                         position.units.number,
                         position.units.currency,
                         position.cost.number,
                         position.cost.currency))
    rows = sorted(rows)
    tbl = table.create_table(rows, fieldspec)

    table.render_table(tbl, sys.stdout, 'text')
    if args.output:
        with open(path.join(args.output), 'w') as file:
            table.render_table(tbl, file, 'csv')


if __name__ == '__main__':
    main()
