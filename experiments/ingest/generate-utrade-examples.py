#!/usr/bin/env python3
"""Generate some random transactions for a fake trading account.
Outputs a CSV file, that looks realistic.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import datetime
import csv
import sys
import random

from dateutil.parser import parse as parse_time

from beancount.core.number import D
from beancount.core import inventory


PROBAB = [
    ('Transfer', 0.02),
    ('Buy', 0.05),
    ('Dividend', 0.1),
    ('Sell', 0.02),
    ]


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('-s', '--start', action='store', type=parse_time,
                        help="Start date")
    parser.add_argument('-e', '--end', action='store', type=parse_time,
                        help="Enddate")
    args = parser.parse_args()

    if args.end is None:
        args.end = datetime.datetime.today()

    if args.start is None:
        args.start = args.end - datetime.timedelta(days=90)

    start = args.start.date()
    end = args.end.date()

    rows = ['DATE,TYPE,REF #,DESCRIPTION,FEES,AMOUNT,BALANCE'.split(',')]

    balance = D('{:.2f}'.format(random.randint(30000, 50000)))

    date = start
    oneday = datetime.timedelta(days=1)
    while date < end:
        date += oneday
        r = random.random()

        s = 0
        txn = None
        for ttype, p in PROBAB:
            if s < random.random() < s + p:
                break
            s += p
        else:
            continue

        row = None
        if ttype == 'Transfer':
            xid = random.randint(10000000, 100000000-1)
            amount = D('{:.2f}'.format(random.randint(3000, 15000)))
            balance += amount
            row = (date, 'XFER', xid, "CLIENT REQUESTED ELECTRONIC FUNDING", D('0'), amount, balance)
        elif ttype == 'Buy':



        if row:
            rows.append(row)

    wr = csv.writer(sys.stdout)
    wr.writerows(rows)


if __name__ == '__main__':
    main()
