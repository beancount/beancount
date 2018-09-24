#!/usr/bin/env python3
"""Pivot table on monthly expenses, with custom rollups and exemptions.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import logging
import datetime
import collections
import re

from beancount.core.number import Decimal
from beancount.core import data
from beancount.core import prices
from beancount.core import convert
from beancount.core import inventory
from beancount.core import account_types
from beancount.parser import options
from beancount.parser import printer
from beancount import loader

from baskets import table


EXCLUDES = [
    re.compile('Expenses:Taxes'),
    re.compile('Expenses:(Travel|Scuba)'),
    re.compile('Expenses:Transportation:(Car-Rental|Flights|CableCar|Tolls)'),
    re.compile('Expenses:Home:(Broker|Penalty|Moving|Doormen)'),
    re.compile('Expenses:Marriage'),
    re.compile('Expenses:Health:Genomics'),
    re.compile('Expenses:Charity'),
]

EXCLUDE_TAG_PREFIX = 'trip-'

MAPS = [
    (re.compile('Expenses:Online:Media'), 'Expenses:Online:Media'),
    (re.compile('Expenses:Online:Apps'), 'Expenses:Online:Apps'),
    (re.compile('Expenses:Online:Services'), 'Expenses:Online:Services'),
    (re.compile('Expenses:Health:Medical'), 'Expenses:Health:Medical'),
    (re.compile('Expenses:Health:Dental'), 'Expenses:Health:Dental'),
    (re.compile('Expenses:Health:PhysicalTherapy'), 'Expenses:Health:PhysicalTherapy'),
    (re.compile('Expenses:Health:Vision'), 'Expenses:Health:Vision'),
    (re.compile('Expenses:(Financial|PayPal|Ebay)'
                ':(Fees|Commissions)'), 'Expenses:Financial:Fees'),
    (re.compile('Expenses:Clothing'), 'Expenses:Clothing'),
    (re.compile('Expenses:Scuba'), 'Expenses:Scuba'),
    (re.compile('Expenses:Fun'), 'Expenses:Fun'),
    (re.compile('Expenses:Govt-Services'), 'Expenses:Govt-Services'),
    (re.compile('Expenses:Charity'), 'Expenses:Charity'),
    (re.compile('Expenses:Communications:Internet'), 'Expenses:Communications:Internet'),
    (re.compile('Expenses:Transportation:Public'), 'Expenses:Transportation:Public'),
]


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount filename')
    parser.add_argument('--currency', action='store', default='USD')
    parser.add_argument('--quantize', action='store', default='0.')
    parser.add_argument('-o', '--output', action='store', help="Output file")
    args = parser.parse_args()
    Q = Decimal(args.quantize)

    # Read input.
    entries, errors, options_map = loader.load_file(args.filename)
    price_map = prices.build_price_map(entries)
    acctypes = options.get_account_types(options_map)

    # Compute start of period.
    today = datetime.date.today()
    date_min = today - datetime.timedelta(days=2*365)
    date_start = datetime.date(date_min.year, date_min.month, 1)
    month_start = (date_min.year, date_min.month)

    # Compute end of period.
    date_max = datetime.date(today.year, today.month, 1)

    # Accumulate expenses for the period.
    balances = collections.defaultdict(lambda: collections.defaultdict(inventory.Inventory))
    all_months = set()
    for entry in data.filter_txns(entries):
        if entry.date < date_start or entry.date >= date_max:
            continue
        if any(tag.startswith(EXCLUDE_TAG_PREFIX) for tag in entry.tags):
            continue
        month = (entry.date.year, entry.date.month)
        all_months.add(month)
        for posting in entry.postings:
            if account_types.get_account_type(posting.account) != acctypes.expenses:
                continue
            if any(regexp.match(posting.account) for regexp in EXCLUDES):
                continue
            if posting.units.currency != args.currency:
                continue
            account = posting.account
            for regexp, target_account in MAPS:
                if regexp.match(account):
                    account = target_account
                    break
            balances[account][month].add_position(posting)

    # Reduce the final balances to numbers.
    sbalances = collections.defaultdict(dict)
    for account, months in sorted(balances.items()):
        for month, balance in sorted(months.items()):
            year, mth = month
            date = datetime.date(year, mth, 1)
            balance = balance.reduce(convert.get_value, price_map, date)
            balance = balance.reduce(convert.convert_position, args.currency, price_map, date)
            try:
                pos = balance.get_only_position()
            except AssertionError:
                print(balance)
                raise
            total = pos.units.number if pos and pos.units else None
            sbalances[account][month] = total

    # Pivot the table.
    header_months = sorted(all_months)
    header = ['account'] + ['{}-{:02d}'.format(*m) for m in header_months]
    rows = []
    for account in sorted(sbalances.keys()):
        row = [account]
        for month in header_months:
            total = sbalances[account].get(month, None)
            row.append(str(total.quantize(Q)) if total else '')
        rows.append(row)

    # Write out the table.
    tbl = table.Table(header, [str] + [Decimal] * (len(header)-1), rows)
    if args.output:
        with open(args.output, 'w') as outfile:
            table.write_csv(tbl, outfile)
    print(tbl)


if __name__ == '__main__':
    main()
