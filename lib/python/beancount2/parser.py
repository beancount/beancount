#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import datetime
from cdecimal import Decimal
from collections import namedtuple

from beancount2 import _parser


CURRENCIES = {}

class Currency:
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return '<%s>' % self.name
    __repr__ = __str__

class Builder(object):

    def __init__(self):
        self.tags = []

    def get_result(self):
        """Return a list of the transactions.."""
        return None ## FIXME: todo

    def begintag(self, tag):
        self.tags.append(tag)

    def endtag(self, tag):
        self.tags.remove(tag)


    def DATE(self, year, month, day):
        return datetime.date(year, month, day)

    def ACCOUNT(self, s):
        return s

    def CURRENCY(self, s):
        try:
            return CURRENCIES[s]
        except KeyError:
            ccy = CURRENCIES[s] = Currency(s)
            return ccy

    def STRING(self, s):
        return s

    def NUMBER(self, s):
        return Decimal(s)

    def amount(self, number, currency):
        return (number, currency)

    def amount_lot(self, amount, lot):
        return (amount, lot)

    def handle_list(self, object_list, object):
        if object_list is None:
            object_list = []
        object_list.append(object)
        return object_list

    def open(self, date, account, account_id, currencies):
        return Open(date, account, account_id, currencies)

    def close(self, date, account):
        return Close(date, account)

    def pad(self, date, account, account_pad):
        return Pad(date, account, account_pad)

    def check(self, date, account, amount):
        return Check(date, account, amount)

    def transaction(self, date, txn, payee, description, postings):
        pass #print('transaction', date, chr(txn), payee, description)

    def posting(self, account, amount_lot, amount, istotal, optflag):
        return Posting(account, amount_lot, amount, istotal, optflag)


Open = namedtuple('Open', 'date account account_id currencies')
Close = namedtuple('Close', 'date account')
Pad = namedtuple('Pad', 'date account account_pad')
Check = namedtuple('Check', 'date account amount')
Posting = namedtuple('Posting', 'account amount_lot amount istotal optflag')
Transaction = namedtuple('Transaction', 'date txn payee description postings')


def parse(filename):
    """Parse a beancount input file and return a list of transactions."""
    builder = Builder()
    _parser.parse(filename, builder)
    return builder.get_result()
