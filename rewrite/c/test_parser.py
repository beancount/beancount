#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import _beancount
import datetime
from cdecimal import Decimal
from collections import namedtuple


CURRENCIES = {}

class Currency:
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return '<%s>' % self.name
    __repr__ = __str__

class Builder:
    def __init__(self):
        pass

    def parseDate(self, year, month, day):
        return datetime.date(year, month, day)

    def parseAccount(self, s):
        return s

    def parseCurrency(self, s):
        try:
            return CURRENCIES[s]
        except KeyError:
            ccy = CURRENCIES[s] = Currency(s)
            return ccy

    def parseString(self, s):
        return s[1:-1]

    def parseNumber(self, s):
        return Decimal(s)

    def buildAmount(self, number, currency):
        return (number, currency)

    def buildTransaction(self, date, txn, payee, description):
        pass #print('transaction', date, chr(txn), payee, description)

    def buildList(self, object_list, object):
        if object_list is None:
            object_list = []
        object_list.append(object)
        return object_list

    def buildOpen(self, date, account, accound_id, currencies):
        return Open(date, account, accound_id, currencies)


Open = namedtuple('Open', 'date account account_id currencies')


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(__doc__.strip())
    parser.add_argument('filename', help='Filename')
    opts = parser.parse_args()

    builder = Builder()
    _beancount.parse(opts.filename, builder)


if __name__ == '__main__':
    main()
