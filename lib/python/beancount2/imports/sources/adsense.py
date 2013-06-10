#!/usr/bin/env python
"""
Interpret the Google AdSense CSV file and output transactions in a format
suitable for Ledger.
"""

import re, time, codecs
from datetime import date
from decimal import Decimal
from itertools import count
from collections import namedtuple

from beancount2.core.data import to_decimal
from beancount2.core.data import Transaction, Posting
from beancount2.utils import csv_tuple_reader


CONFIG = {
    'FILE'          : 'Account for filing',
    'cash_currency' : 'USD',
    'cash'          : 'Main account holding the funds',
    'income'        : 'Income account',
    'transfer'      : 'Default account where money gets transferred to',
}


#### FIXME: This is incomplete, I need to finish porting this one (this is the last one).

def import_file(filename, config):
    """Import a Google AdSense file."""

    f = codecs.open(fn, "rb", encoding='utf-16')
    rows = list(parse_csv_file(f, delimiter='\t'))
    i = 0
    for x in rows:
        i += 1
        date_ = datetime.datetime.strptime(x.date, '%m/%d/%y').date()

        ispayment = re.search('payment.*issued', x.description, re.I)

        if not ispayment:
            print('%s * %s | %s' % (date_, payee, x.description))
            print('  %-50s    %s %s' % (acc_asset, x.amount, currency))
            print('  %-50s    %s %s' % (acc_income, '', ''))
            print()
        else:
            print('%s ! %s | %s' % (date_, payee, x.description))
            print('  %-50s    %s %s' % (acc_income, x.amount, currency))
            print('  %-50s    %s %s' % (acc_deposit, '', ''))
            print()

    balance = to_decimal(x.account_balance)
    print('@check %s  %-50s  %s %s' % (date_, acc_asset, balance, currency))


# Use the generic one from utils.

# def parse_csv_file(f, **kw):
#     """
#     Parse a CSV file and return a list of rows as named_tuple objects.
#     We assume that the first row is a title row.
#     """
#     import csv
#     reader = csv.reader(f, **kw)
#     ireader = iter(reader)

#     cols = []
#     dummycount = count().next
#     for x in ireader.next():
#         cx = x.strip().lower().replace(' ', '_')
#         if not cx:
#             cx = 'dummy_%d' % dummycount()
#         cols.append(cx)

#     Row = namedtuple('Row', cols)
#     return (Row(*x) for x in ireader)
