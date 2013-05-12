#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import datetime
from cdecimal import Decimal
from collections import namedtuple

from beancount2 import _parser
from beancount2.inventory import Amount


# All possible types of entries.
Open        = namedtuple('Open'        , 'date account account_id currencies')
Close       = namedtuple('Close'       , 'date account')
Pad         = namedtuple('Pad'         , 'date account account_pad')
Check       = namedtuple('Check'       , 'date account amount')
Transaction = namedtuple('Transaction' , 'date flag payee description tags postings')
Event       = namedtuple('Event'       , 'date type description')
Note        = namedtuple('Note'        , 'date comment')
Price       = namedtuple('Price'       , 'date currency amount')

# Basic data types.
Account = namedtuple('Account', 'name')
Posting = namedtuple('Posting', 'account amount_lot amount istotal optflag')


class Builder(object):

    def __init__(self):
        # A stack of the current active tags.
        self.tags = []

        # The result from running the parser.
        self.result = None

        # Temporary accounts map.
        self.accounts = {}

    def store_result(self, result):
        """Start rule stores the final result here."""
        self.result = result


    def begintag(self, tag):
        self.tags.append(tag)

    def endtag(self, tag):
        self.tags.remove(tag)


    def DATE(self, year, month, day):
        return datetime.date(year, month, day)

    def ACCOUNT(self, s):
        try:
            account = self.accounts[s]
        except KeyError:
            account = self.accounts[s] = Account(s)
        return account

    def CURRENCY(self, s):
        return s

    def STRING(self, s):
        return s

    def TAG(self, s):
        return s

    def NUMBER(self, s):
        return Decimal(s)

    def amount(self, number, currency):
        return Amount(number, currency)

    def lot(self, amount, lot_date):
        return Lot(amount, lot_date)

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

    def event(self, date, event_type, description):
        return Event(date, event_type, description)

    def price(self, date, currency, amount):
        return Price(date, currency, amount)

    def note(self, date, comment):
        return Note(date, comment)

    def transaction(self, date, flag, payee, description, tags, postings):
        ctags = []
        if tags is not None:
            ctags.extend(tags)
        if self.tags:
            ctags.extend(self.tags)

        return Transaction(date, chr(flag), payee, description, ctags, postings)

    def posting(self, account, amount_lot, amount, istotal, optflag):
        return Posting(account, amount_lot, amount, istotal, optflag)





def parse(filename):
    """Parse a beancount input file and return a list of transactions."""
    builder = Builder()
    _parser.parse(filename, builder)
    return builder.result




class LexOnlyBuilder(object):

    def DATE(self, year, month, day): pass
    def ACCOUNT(self, s):             pass
    def CURRENCY(self, s):            pass
    def STRING(self, s):              pass
    def TAG(self, s):                 pass
    def NUMBER(self, s):              pass

def dump_lexer(filename, fileobj):
    """Parse a beancount input file and return a list of transactions."""
    _parser.lexer_init(filename, LexOnlyBuilder())
    while 1:
        x = _parser.lexer_next()
        if x is None:
            break
        token, text, lineno = x
        print('{:12} {:6d} {}'.format(token, lineno, repr(text)))
