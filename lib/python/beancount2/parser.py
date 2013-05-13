#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import datetime
import copy
import logging
from cdecimal import Decimal
from collections import namedtuple

from beancount2 import _parser
from beancount2.inventory import Amount, mult_amount
from beancount2.inventory import Lot, Position, Inventory


__sanity_checks__ = False

ZERO = Decimal()

# The location in a source file where the directive was found.
FileLocation = namedtuple('FileLocation', 'filename lineno')

# All possible types of entries.
Open        = namedtuple('Open'        , 'fileloc date account account_id currencies')
Close       = namedtuple('Close'       , 'fileloc date account')
Pad         = namedtuple('Pad'         , 'fileloc date account account_pad')
Check       = namedtuple('Check'       , 'fileloc date account amount')
Transaction = namedtuple('Transaction' , 'fileloc date flag payee narration tags postings')
Event       = namedtuple('Event'       , 'fileloc date type description')
Note        = namedtuple('Note'        , 'fileloc date comment')
Price       = namedtuple('Price'       , 'fileloc date currency amount')

# Basic data types.
Account = namedtuple('Account', 'name')
Posting = namedtuple('Posting', 'account position price flag')


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
        self.entries = None

        # Temporary accounts map.
        self.accounts = {}

    def store_result(self, entries):
        """Start rule stores the final result here."""
        self.entries = entries


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

    def posting(self, account, position, price, istotal, flag):
        # If the price is specified for the entire amount, compute the effective
        # price here and forget about that detail of the input syntax.
        if istotal:
            price = Amount(price.number / position.number, price.currency)
        return Posting(account, position, price, flag)

    def transaction(self, filename, lineno, date, flag, payee, narration, tags, postings):
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

        # Balance incomplete auto-postings.
        # print('{}:{}: {}'.format(fileloc.filename, fileloc.lineno, narration))
        postings, inserted = balance_incomplete_postings(fileloc, postings)

        # Create the transaction.
        transaction = Transaction(fileloc, date, chr(flag), payee, narration, ctags, postings)

        # Check that the balance actually is empty.
        if __sanity_checks__:
            residual = compute_residual(postings)
            assert residual.is_small(SMALL_EPSILON), residual

        return transaction


def parse(filename):
    """Parse a beancount input file and return a list of transactions."""
    builder = Builder()
    _parser.parse(filename, builder)
    return copy.copy(builder.entries)



# FIXME: does this belong here? Not sure.


SMALL_EPSILON = Decimal('0.005') # FIXME: This should probably be a little smaller.

def compute_residual(postings):
    """Compute the residual of a set of complete postings.
    This is used to cross-check a balanced transaction."""

    inventory = Inventory()
    for posting in postings:
        position = posting.position
        lot = position.lot

        # It the position has a cost, use that to balance this posting.
        if lot.cost:
            amount = mult_amount(lot.cost, position.number)

        # If there is a price, use that to balance this posting.
        elif posting.price:
            amount = mult_amount(posting.price, position.number)

        # Otherwise, just use the amount itself.
        else:
            amount = position.get_amount()
        inventory.add(amount)

    return inventory


def balance_incomplete_postings(fileloc, postings):
    """Replace and complete postings that have no amount specified on them.
    Return a new list of balanced postings, with the incomplete postings
    replaced with completed ones.
    (The 'postings' parameter may be modified or destroyed for performance;
    don't reuse it.)"""

    # The list of postings without and with an explicit position.
    auto_postings_indices = []

    # Currencies seen in complete postings.
    currencies = set()

    # An inventory to accumulate the residual balance.
    inventory = Inventory()

    for i, posting in enumerate(postings):
        position = posting.position

        if position is None:
            auto_postings_indices.append(i)
        else:
            lot = position.lot
            currencies.add(lot.currency)

            # It the position has a cost, use that to balance this posting.
            if lot.cost:
                amount = mult_amount(lot.cost, position.number)

            # If there is a price, use that to balance this posting.
            elif posting.price:
                amount = mult_amount(posting.price, position.number)

            # Otherwise, just use the amount itself.
            else:
                amount = position.get_amount()
            inventory.add(amount)

    # If there are auto-postings, fill them in.
    if auto_postings_indices:
        inserted_autopostings = True

        # If there are too many such postings, we can't do anything, barf.
        if len(auto_postings_indices) > 1:
            raise ParserError(fileloc, "Too many auto-postings; cannot fill in.")

        index = auto_postings_indices[0]
        old_posting = postings[index]
        assert old_posting.price is None

        residual_positions = inventory.get_positions()

        # If there are no residual positions, we want to still insert a posting
        # but with a zero position, so that the posting shows up anyhow. We
        # insert one such posting for each currency seen in the complete
        # postings.
        new_postings = []
        if not residual_positions:
            for currency in currencies:
                position = Position(Lot(currency, None, None), ZERO)
                new_postings.append(
                    Posting(old_posting.account, position, None, old_posting.flag))
        else:
            # Convert all the residual positions in inventory into a posting for
            # each position.
            for position in residual_positions:
                position.number = -position.number
                new_postings.append(
                    Posting(old_posting.account, position, None, old_posting.flag))

        postings[index:index+1] = new_postings

    else:
        inserted_autopostings = False

        # Detect complete sets of postings that have residual balance.
        if not inventory.is_small(SMALL_EPSILON):
            raise ParserError(fileloc, "Transaction does not balance: {}.".format(inventory))

    return postings, inserted_autopostings





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
