#!/usr/bin/env python3
"""A quick one-off script to generate some random transactions for a fake trading account.
Outputs a CSV file, that looks realistic.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import collections
import datetime
import csv
import sys
import random
import bisect
from os import path

from dateutil.parser import parse as parse_time

from beancount.core.number import D
from beancount.core.number import Decimal
from beancount.core.number import ZERO
from beancount.utils.bisect_key import bisect_left_with_key
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory


PROBAB = [
    ('XFER', 0.01),
    ('BUY', 0.05),
    ('DIV', 0.08),
    ('SELL', 0.02),
    ]

STOCKS = ['HOOL', 'MSFX', 'BAPL', 'CSKO']


def split_rows(rows, file_period):
    date_first = rows[0][0]
    date_last = rows[-1][0]
    date_end = date_first + file_period
    while date_end < date_last:
        index_end = bisect_left_with_key(rows, date_end, key=lambda row: row.date)
        date_begin = date_end - file_period
        index_begin = bisect_left_with_key(rows, date_begin, key=lambda row: row.date)
        yield date_end, rows[index_begin:index_end]
        report_interval = datetime.timedelta(days=random.randint(60, 80))
        date_end += report_interval


Row = collections.namedtuple('Row', 'date type xid desc fees amount balance')



def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('output', action='store',
                        help="Ouptut directory")

    parser.add_argument('-s', '--start', action='store', type=parse_time,
                        help="Start date")
    parser.add_argument('-e', '--end', action='store', type=parse_time,
                        help="Enddate")

    args = parser.parse_args()

    if args.end is None:
        args.end = datetime.datetime.today()

    if args.start is None:
        args.start = args.end - datetime.timedelta(days=24*30)

    start = args.start.date()
    end = args.end.date()

    rows = []

    PENNY = D('0.01')
    initial_balance = D(random.uniform(30000, 50000)).quantize(PENNY)

    date = start
    oneday = datetime.timedelta(days=1)
    balance = initial_balance
    inv = inventory.Inventory()
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
        xid = random.randint(10000000, 100000000-1)
        fees = ZERO
        number = ZERO
        code = ttype
        if ttype == 'XFER':
            desc = "CLIENT REQUESTED ELECTRONIC FUNDING"
            number = D(random.uniform(3000, 15000)).quantize(PENNY)

        elif ttype == 'BUY':
            core = 'TRD'
            stock = random.choice(STOCKS)
            approx_number = D(random.uniform(2000, 10000))
            price = D(random.uniform(5, 100)).quantize(PENNY)
            shares = D(int(approx_number / price))
            number = -shares * price
            desc = "BOUGHT +{} {} @{}".format(stock, shares, price)
            inv.add_amount(amount.Amount(shares, stock),
                           position.Cost(price, 'USD', date, None))
            if shares * price > balance:
                continue

            fees = D('7.95')
            number -= fees

        elif ttype == 'SELL':
            if inv.is_empty():
                continue
            core = 'TRD'
            pos = random.choice(inv)
            shares = (D(random.uniform(0.3, 0.7)) * pos.units.number).quantize(ZERO)
            price = (pos.cost.number * D(random.normalvariate(1.0, 0.1))).quantize(PENNY)
            number = price * shares

            fees = D('7.95')
            number -= fees

        elif ttype == 'DIV':
            if inv.is_empty():
                continue
            pos = random.choice(inv)
            desc = "ORDINARY DIVIDEND~{}".format(pos.units.currency)
            number = (D(random.uniform(0.01/12, 0.04/12)) *
                      pos.units.number *
                      pos.cost.number).quantize(PENNY)

        else:
            continue

        balance += number
        row = Row(date, ttype, xid, desc, fees, number, balance)
        rows.append(row)

    header = 'DATE,TYPE,REF #,DESCRIPTION,FEES,AMOUNT,BALANCE'.split(',')
    for date_end, report_rows in split_rows(rows, datetime.timedelta(days=90)):
        filename = path.join(args.output, "UTrade{:%Y%m%d}.csv".format(date_end))
        with open(filename, 'w') as file:
            wr = csv.writer(file)
            wr.writerow(header)
            wr.writerows(report_rows)


if __name__ == '__main__':
    main()
