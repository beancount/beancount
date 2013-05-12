#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import datetime
from cdecimal import Decimal
from collections import namedtuple

from beancount2 import _parser
from beancount2.inventory import Amount, Lot, Position


# The location in a source file where the directive was found.
FileLocation = namedtuple('FileLocation', 'filename lineno')

# All possible types of entries.
Open        = namedtuple('Open'        , 'fileloc date account account_id currencies')
Close       = namedtuple('Close'       , 'fileloc date account')
Pad         = namedtuple('Pad'         , 'fileloc date account account_pad')
Check       = namedtuple('Check'       , 'fileloc date account amount')
Transaction = namedtuple('Transaction' , 'fileloc date flag payee description tags postings')
Event       = namedtuple('Event'       , 'fileloc date type description')
Note        = namedtuple('Note'        , 'fileloc date comment')
Price       = namedtuple('Price'       , 'fileloc date currency amount')

# Basic data types.
Account = namedtuple('Account', 'name')
Posting = namedtuple('Posting', 'account position price istotal optflag')


class ParserError(RuntimeError):
    "A parsing error. Formats the file location into the message string."

    def __init__(self, fileloc, message):
        RuntimeError.__init__(self, '{:s}:{:d}: {}'.format(
            fileloc.filename, fileloc.lineno, message))


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

    def lot_cost_date(self, cost, lot_date):
        return (cost, lot_date)

    def position(self, amount, lot_cost_date):
        cost, lot_date = lot_cost_date if lot_cost_date else (None, None)
        lot = Lot(amount.currency, cost, lot_date)
        return Position(lot, amount.number)

    def handle_list(self, object_list, object):
        if object_list is None:
            object_list = []
        object_list.append(object)
        return object_list

    def open(self, filename, lineno, date, account, account_id, currencies):
        fileloc = FileLocation(filename, lineno)
        return Open(fileloc, date, account, account_id, currencies)

    def close(self, filename, lineno, date, account):
        fileloc = FileLocation(filename, lineno)
        return Close(fileloc, date, account)

    def pad(self, filename, lineno, date, account, account_pad):
        fileloc = FileLocation(filename, lineno)
        return Pad(fileloc, date, account, account_pad)

    def check(self, filename, lineno, date, account, amount):
        fileloc = FileLocation(filename, lineno)
        return Check(fileloc, date, account, amount)

    def event(self, filename, lineno, date, event_type, description):
        fileloc = FileLocation(filename, lineno)
        return Event(fileloc, date, event_type, description)

    def price(self, filename, lineno, date, currency, amount):
        fileloc = FileLocation(filename, lineno)
        return Price(fileloc, date, currency, amount)

    def note(self, filename, lineno, date, comment):
        fileloc = FileLocation(filename, lineno)
        return Note(fileloc, date, comment)

    def posting(self, account, position, price, istotal, optflag):
        return Posting(account, position, price, istotal, optflag)

    def transaction(self, filename, lineno, date, flag, payee, description, tags, postings):
        fileloc = FileLocation(filename, lineno)

        # Detect when a transaction does not have at least two legs.
        if postings is None or len(postings) < 2:
            raise ParserError("Invalid number of postings", fileloc)

        # Merge the tags from the stach with the explicit tags of this transaction
        ctags = set()
        if tags is not None:
            ctags.update(tags)
        if self.tags:
            ctags.update(self.tags)

        return Transaction(fileloc, date, chr(flag), payee, description, ctags, postings)

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
