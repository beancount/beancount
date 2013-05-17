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
from beancount2 import render_tree
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
Account = namedtuple('Account', 'name type')
Posting = namedtuple('Posting', 'account position price flag')


class ParserError(RuntimeError):
    "A parsing error. Formats the file location into the message string."

    def __init__(self, fileloc, message):
        RuntimeError.__init__(self, '{:s}:{:d}: {}'.format(
            fileloc.filename, fileloc.lineno, message))


def account_parent_name(name):
    """Return the name of the parent account of the given account."""
    components = name.split(':')
    components.pop(-1)
    return ':'.join(components)

def account_leaf_name(name):
    return name.split(':')[-1]


class AccountTree:
    """A container for a hierarchy of accounts."""

    def __init__(self):
        # The root note of all accounts.
        self.root = Account('', None, [])

        # A mapping of (name, Account).
        self.accounts_map = {'': self.root}

    def dump(self, out_file):
        string = render_tree.render(self.root,
                                    lambda x: account_leaf_name(x.name),
                                    lambda x: x.children)
        print(string, file=out_file)

    def get(self, name):
        return self.accounts_map[name]

    def get_names(self):
        return sorted(self.accounts_map)

    def get_or_create(self, name):
        try:
            account = self.accounts_map[name]
        except KeyError:
            parent = self.get_or_create(account_parent_name(name))
            account = Account(name, parent, [])
            parent.children.append(account)
            self.accounts_map[name] = account
        return account



# FIXME: it would be nice to be able to just provide a list of entries in order
# to create a Ledger, and for the subtree of accounts to just recreate itself
# automatically from that list of entries. Is the tree of accounts part of a
# Realization?
#
# Or maybe just the 'children' part of an account is part of the Realization?
#
# Maybe just 'entries' is the basic unit, and 'Ledger' *is* the realization?

class Ledger:
    """A class that contains a particular list of entries and an
    associated account tree. Note: the account tree is redundant and
    could be recalculated from the list of entries."""

    def __init__(self, entries, accounts):

        # A list of sorted entries in this ledger.
        assert isinstance(entries, list)
        entries.sort(key=lambda x: x.date)
        self.entries = entries

        # A tree of accounts.
        assert isinstance(accounts, AccountTree)
        self.accounts = accounts


class Builder(object):
    """A builder used by the lexer and grammer parser as callbacks to create
    the data objects corresponding to rules parsed from the input file."""

    def __init__(self):
        # A stack of the current active tags.
        self.tags = []

        # The result from running the parser, a list of entries.
        self.entries = None

        # # Temporary accounts map.
        # self.account_tree = AccountTree()

    def store_result(self, entries):
        """Start rule stores the final result here."""
        self.entries = entries


    def begintag(self, tag):
        self.tags.append(tag)

    def endtag(self, tag):
        self.tags.remove(tag)


    def DATE(self, year, month, day):
        return datetime.date(year, month, day)

    def ACCOUNT(self, account_name):
        # return self.account_tree.get_or_create(account_name)
        return Account(account_name, account_name.split(':')[0])

    def CURRENCY(self, currency_name):
        return currency_name

    def STRING(self, string):
        return string

    def TAG(self, tag):
        return tag

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
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts."""
    builder = Builder()
    _parser.parse(filename, builder)
    return builder.entries



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
