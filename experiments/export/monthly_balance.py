#!/usr/bin/env python3
"""
Python answer to:
https://gist.github.com/darthcloud/134875df4e624b0cf92a
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import csv
import collections
import sys

from dateutil.parser import parse

from beancount.core.number import ZERO
from beancount.query import query
from beancount import loader


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount input filename')
    parser.add_argument('--from', dest='dfrom', action='store', default='1970-01-01')
    parser.add_argument('--to', dest='dto', action='store', default='2199-01-01')
    args = parser.parse_args()

    # Create a SQL query.
    date_from = parse(args.dfrom).date()
    date_to = parse(args.dto).date()
    sql = """
      SELECT 
        year, month, root(account, 1) as ar, sum(position) as pos
      FROM 
        date > {date_from} AND date < {date_to}
      WHERE 
         account ~ "Expenses" OR
         account ~ "Liabilities:Loans" OR
         account ~ "Income"
      GROUP BY year, month, ar 
      ORDER BY year, month, ar 
      FLATTEN
    """.format(**locals())

    # Load the file and run a query on it.
    entries, _, options_map = loader.load_file(args.filename)
    rtypes, rrows = query.run_query(entries, options_map, sql)

    # Pivot on the year/month + currency
    out = collections.defaultdict(lambda: collections.defaultdict(dict))
    for row in rrows:
        d = out['{}/{:02d}'.format(row.year, row.month)][row.pos.lot.currency]
        d[row.ar] = row.pos.number
        
    # Write this out to a CSV file.
    wr = csv.writer(sys.stdout)
    for month, currencies in sorted(out.items()):
        for currency, accounts in sorted(currencies.items()):
            exp = accounts.get('Expenses', ZERO)
            loans = accounts.get('Liabilities:Loans', ZERO)
            inc = accounts.get('Income', ZERO)
            wr.writerow((month, exp, loans, -inc, exp + loans - inc, currency))


if __name__ == '__main__':
    main()
