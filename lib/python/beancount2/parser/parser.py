#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import datetime
import copy
import logging
import tempfile
from collections import namedtuple
from os import path
import textwrap

from beancount2.parser import _parser
from beancount2.utils import tree_utils
from beancount2.data import *
from beancount2.inventory import Position, Inventory
from beancount2 import balance


# Options.
__sanity_checks__ = False


class ParserError(RuntimeError):
    """A parsing error. Formats the file location into the message string."""

    # FIXME: remove this formatting, not needed.
    def __init__(self, fileloc, message):
        RuntimeError.__init__(self, '{:s}:{:d}: {}'.format(
            fileloc.filename, fileloc.lineno, message))


class Builder(object):
    """A builder used by the lexer and grammer parser as callbacks to create
    the data objects corresponding to rules parsed from the input file."""

    def __init__(self):
        # A stack of the current active tags.
        self.tags = []

        # The result from running the parser, a list of entries.
        self.entries = []

        # A mapping of all the accounts created.
        self.accounts = {}

        # Errors that occurred during parsing.
        self.errors = []

        # Accumulated and unprocessed options.
        self.options = []

    def store_result(self, entries):
        """Start rule stores the final result here."""
        if entries:
            self.entries = entries


    def pushtag(self, tag):
        self.tags.append(tag)

    def poptag(self, tag):
        self.tags.remove(tag)

    def option(self, filename, lineno, key, value):
        fileloc = FileLocation(filename, lineno)
        assert key not in self.options
        self.options.append(Option(fileloc, key, value))


    def DATE(self, year, month, day):
        return datetime.date(year, month, day)

    def ACCOUNT(self, account_name):
        try:
            account = self.accounts[account_name]
        except KeyError:
            account = Account(account_name, account_type(account_name))
            self.accounts[account_name] = account
        return account

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

    def check(self, filename, lineno, date, account, position):
        fileloc = FileLocation(filename, lineno)
        return Check(fileloc, date, account, position)

    def event(self, filename, lineno, date, event_type, description):
        fileloc = FileLocation(filename, lineno)
        return Event(fileloc, date, event_type, description)

    def price(self, filename, lineno, date, currency, amount):
        fileloc = FileLocation(filename, lineno)
        return Price(fileloc, date, currency, amount)

    def note(self, filename, lineno, date, account, comment):
        fileloc = FileLocation(filename, lineno)
        return Note(fileloc, date, account, comment)

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
            # FIXME: Don't raise, log an error and skip instead...
            raise ParserError(fileloc, "Invalid number of postings")

        # Merge the tags from the stach with the explicit tags of this transaction
        ctags = set()
        if tags is not None:
            ctags.update(tags)
        if self.tags:
            ctags.update(self.tags)

        # Balance incomplete auto-postings.
        # print('{}:{}: {}'.format(fileloc.filename, fileloc.lineno, narration))
        postings, inserted, errors = balance.balance_incomplete_postings(fileloc, postings)
        if errors:
            self.errors.extend(errors)

        # Create the transaction.
        transaction = Transaction(fileloc, date, chr(flag), payee, narration, ctags, postings)

        # Check that the balance actually is empty.
        if __sanity_checks__:
            residual = balance.compute_residual(postings)
            assert residual.is_small(balance.SMALL_EPSILON), residual

        return transaction


# The result from parsing a set of entries.
# I want this to remain as simple as possible.
# The list of entries is sorted by date, and the order of appearance in the input file.
FileContents = namedtuple('FileContents', 'entries accounts parse_errors options')

# A parsed option directive.
Option = namedtuple('Option', 'fileloc key value')


# Sort with the checks at the BEGINNING of the day.
SORT_ORDER = {Open: -2, Check: -1, Close: 1}

# Sort with the checks at the END of the day.
#SORT_ORDER = {Open: -2, Check: 1, Close: 2}


def entry_sortkey(entry):
    """Sort-key for entries. We sort by date, except that checks
    should be placed in front of every list of entries of that same day,
    in order to balance linearly."""
    return (entry.date, SORT_ORDER.get(type(entry), 0), entry.fileloc.lineno)


# A list of option names that can be specified multiple times.
# FIXME: This shall be removed, we should use an argparse parser to ensure that
# we offer the same set of options for the command-line as we do in the file.
LIST_OPTIONS = ['documents', 'currency']

def parse_options(options):
    """Parse and validate the options into a dictionary."""
    options_dict = {}
    for option in options:
        key = option.key
        if key in LIST_OPTIONS:
            options_dict.setdefault(key, []).append(option.value)
        else:
            assert key not in options_dict
            options_dict[key] = option.value
    return options_dict


def parse(filename):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts."""
    builder = Builder()
    _parser.parse(path.abspath(filename), builder)
    entries = sorted(builder.entries, key=entry_sortkey)
    accounts = sorted(builder.accounts.values(), key=account_sortkey)
    options = parse_options(builder.options)
    return FileContents(entries, accounts, builder.errors, options)


def parse_string(input_string):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts."""
    with tempfile.NamedTemporaryFile('w') as tmp_file:
        tmp_file.write(input_string)
        tmp_file.flush()
        return parse(tmp_file.name)





class LexOnlyBuilder(object):
    """A builder used only for getting the lexer to pass."""

    def DATE(self, year, month, day): pass
    def ACCOUNT(self, s):             pass
    def CURRENCY(self, s):            pass
    def STRING(self, s):              pass
    def TAG(self, s):                 pass
    def NUMBER(self, s):              pass


def dump_lexer(filename):
    """Parse a beancount input file and return a list of transactions."""
    _parser.lexer_init(filename, LexOnlyBuilder())
    while 1:
        x = _parser.lexer_next()
        if x is None:
            break
        token, text, lineno = x
        print('{:12} {:6d} {}'.format(token, lineno, repr(text)))


def dump_lexer_string(input_string):
    """Parse a beancount input file and return a list of transactions."""
    with tempfile.NamedTemporaryFile('w') as tmp_file:
        tmp_file.write(input_string)
        tmp_file.flush()
        return dump_lexer(tmp_file.name)

