#!/usr/bin/env python3
"""
Basic test to invoke the beancount parser.
"""
import collections
import datetime
import functools
import inspect
import textwrap
import copy
import tempfile
from collections import namedtuple
from os import path

from beancount.parser import _parser
from beancount.core.account import account_from_name
from beancount.core import data
from beancount.core.amount import ZERO, Decimal, Amount
from beancount.core.position import Lot, Position
from beancount.core.data import Transaction, Check, Open, Close, Pad, Event, Price, Note, Document, FileLocation, Posting
from beancount.core.account import account_from_name
from beancount.core.balance import balance_incomplete_postings
from beancount.core.balance import compute_residual, SMALL_EPSILON


# Options.
__sanity_checks__ = False


# A set of default options.
#
# FIXME: This shall be removed, we should use an argparse parser to ensure that
# we offer the same set of options for the command-line as we do in the file.
DEFAULT_OPTIONS = {

    # The title of this ledger / input file. This shows up at the top of every
    # page.
    "title" : "Beancount",

    # Root names of every account. This can be used to customize your category
    # names, so that if you prefer "Revenue" over "Income" or "Capital" over
    # "Equity", you can set them here. The account names in your input files
    # must match, and the parser will validate these.
    "name_assets"      : "Assets",
    "name_liabilities" : "Liabilities",
    "name_equity"      : "Equity",
    "name_income"      : "Income",
    "name_expenses"    : "Expenses",

    # Leaf name of the equity account used for summarizing previous transactions
    # into opening balances.
    "account_previous_balances" : "Opening-Balances",

    # Leaf name of the equity account used for transferring previous retained
    # earnings from income and expenses accrued before the beginning of the
    # exercise into the balance sheet.
    "account_previous_earnings" : "Earnings:Previous",

    # Leaf name of the equity account used for inserting conversions that will
    # zero out remaining amounts due to transfers before the opening date. This
    # will essentially "fixup" the basic accounting equation due to the errors
    # that priced conversions introduce.
    "account_previous_conversions" : "Conversions:Previous",

    # Leaf name of the equity account used for transferring current retained
    # earnings from income and expenses accrued during the current exercise into
    # the balance sheet. This is most often called "Net Income".
    "account_current_earnings" : "Earnings:Current",

    # Leaf name of the equity account used for inserting conversions that will
    # zero out remaining amounts due to transfers during the exercise period.
    "account_current_conversions" : "Conversions:Current",

    # Leaf name of the subaccounts created for unrealized capital gains.
    "account_unrealized" : "Unrealized",

    # A list of directory roots, relative to the CWD, which should be searched
    # for document files. For the document files to be automatically found they
    # must have the following filename format: YYYY-MM-DD.(.*)
    "documents" : [],

    # A list of currencies that we single out during reporting and create
    # dedicated columns for. This is used to indicate the main currencies that
    # you work with in real life. (Refrain from listing all the possible
    # currencies here, this is not what it is made for; just list the very
    # principal currencies you use daily only.)
    #
    # Because our system is agnostic to any unit definition that occurs in the
    # input file, we use this to display these values in table cells without
    # their associated unit strings. This allows you to import the numbers in a
    # spreadsheet (e.g, "101.00 USD" does not get parsed by a spreadsheet
    # import, but "101.00" does).
    "operating_currency" : [],
}


def get_previous_accounts(options):
    """Return Account objects for the opening, earnings, and conversion accounts."""

    equity = options['name_equity']

    account_previous_earnings = account_from_name(
        '{}:{}'.format(equity, options['account_previous_earnings']))

    account_previous_balances = account_from_name(
        '{}:{}'.format(equity, options['account_previous_balances']))

    account_previous_conversions = account_from_name(
        '{}:{}'.format(equity, options['account_previous_conversions']))

    return (account_previous_earnings,
            account_previous_balances,
            account_previous_conversions)


def get_current_accounts(options):
    """Return Account objects for the opening, earnings, and conversion accounts."""

    equity = options['name_equity']

    account_current_earnings = account_from_name(
        '{}:{}'.format(equity, options['account_current_earnings']))

    account_current_conversions = account_from_name(
        '{}:{}'.format(equity, options['account_current_conversions']))

    return (account_current_earnings,
            account_current_conversions)


ParserError = collections.namedtuple('ParserError', 'fileloc message entry')
ParserSyntaxError = collections.namedtuple('ParserError', 'fileloc message entry')


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
        self.options = copy.deepcopy(DEFAULT_OPTIONS)

    def store_result(self, entries):
        """Start rule stores the final result here."""
        if entries:
            self.entries = entries


    def pushtag(self, tag):
        self.tags.append(tag)

    def poptag(self, tag):
        self.tags.remove(tag)

    def option(self, filename, lineno, key, value):
        if key not in self.options:
            fileloc = FileLocation(filename, lineno)
            self.errors.append(
                ParserError(fileloc, "Invalid option: '{}'".format(key), None))
        else:
            option = self.options[key]
            if isinstance(option, list):
                option.append(value)
            else:
                self.options[key] = value


    def DATE(self, year, month, day):
        return datetime.date(year, month, day)

    def ACCOUNT(self, account_name):
        try:
            account = self.accounts[account_name]
        except KeyError:
            account = account_from_name(account_name)
            self.accounts[account_name] = account
        return account

    def CURRENCY(self, currency_name):
        return currency_name

    def STRING(self, string):
        return string

    def TAG(self, tag):
        return tag

    def LINK(self, link):
        return link

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
        if object is not None:
            object_list.append(object)
        return object_list

    def error(self, message, filename, lineno):
        fileloc = FileLocation(filename, lineno)
        self.errors.append(ParserSyntaxError(fileloc, message, None))

    def open(self, filename, lineno, date, account, currencies):
        fileloc = FileLocation(filename, lineno)
        return Open(fileloc, date, account, currencies)

    def close(self, filename, lineno, date, account):
        fileloc = FileLocation(filename, lineno)
        return Close(fileloc, date, account)

    def pad(self, filename, lineno, date, account, account_pad):
        fileloc = FileLocation(filename, lineno)
        return Pad(fileloc, date, account, account_pad)

    def check(self, filename, lineno, date, account, amount):
        # Note: No errors by default. We replace the failing ones in the routine
        # that does the verification that these have succeeded or failed.
        errdiff = None
        fileloc = FileLocation(filename, lineno)
        return Check(fileloc, date, account, amount, errdiff)

    def event(self, filename, lineno, date, event_type, description):
        fileloc = FileLocation(filename, lineno)
        return Event(fileloc, date, event_type, description)

    def price(self, filename, lineno, date, currency, amount):
        fileloc = FileLocation(filename, lineno)
        return Price(fileloc, date, currency, amount)

    def note(self, filename, lineno, date, account, comment):
        fileloc = FileLocation(filename, lineno)
        return Note(fileloc, date, account, comment)

    def document(self, filename, lineno, date, account, document_filename):
        fileloc = FileLocation(filename, lineno)
        if not path.isabs(document_filename):
          document_filename = path.abspath(path.join(path.dirname(filename),
                                                     document_filename))

        return Document(fileloc, date, account, document_filename)

    def posting(self, account, position, price, istotal, flag):
        # If the price is specified for the entire amount, compute the effective
        # price here and forget about that detail of the input syntax.
        if istotal:
            price = Amount(ZERO if position.number == ZERO else price.number / position.number, price.currency)
        return Posting(None, account, position, price, chr(flag) if flag else None)

    def transaction(self, filename, lineno, date, flag, payee, narration, tags, links, postings):
        fileloc = FileLocation(filename, lineno)

        # Detect when a transaction does not have at least two legs.
        if postings is None or len(postings) < 2:
            self.errors.append(
                ParserError(fileloc, "Invalid number of postings: {}".format(postings), None))
            return None

        # Merge the tags from the stach with the explicit tags of this transaction
        ctags = set()
        if tags is not None:
            ctags.update(tags)
        if self.tags:
            ctags.update(self.tags)
        if not ctags:
            ctags = None
        else:
            ctags = frozenset(ctags)

        # Create the transaction. Note: we need to parent the postings.
        entry = Transaction(fileloc, date, chr(flag), payee, narration, ctags, links, postings)

        # Balance incomplete auto-postings.
        balance_incomplete_postings(entry)

        # Check that the balance actually is empty.
        if __sanity_checks__:
            residual = compute_residual(entry.postings)
            assert residual.is_small(SMALL_EPSILON), residual

        return entry


# A parsed option directive.
Option = namedtuple('Option', 'fileloc key value')

# A tuple that contains the names of the root accounts. This is a subset of options.
AccountTypes = namedtuple('AccountTypes', "assets liabilities equity income expenses")

def get_account_types(options):
    """Extract the account type names from the parser's options."""
    return AccountTypes(
        *(options["name_{}".format(x)]
          for x in "assets liabilities equity income expenses".split()))


def parse(filename, **kw):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts."""
    builder = Builder()
    _parser.parse(path.abspath(filename), builder, **kw)
    entries = sorted(builder.entries, key=data.entry_sortkey)
    # accounts = sorted(builder.accounts.values(), key=account_sortkey)
    return (entries, builder.errors, builder.options)


def parse_string(input_string, **kw):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts."""
    with tempfile.NamedTemporaryFile('w') as tmp_file:
        tmp_file.write(input_string)
        tmp_file.flush()
        return parse(tmp_file.name, **kw)


def parsedoc(fun):
    """Decorator that parses the function's docstring as an argument."""
    import sys
    filename = inspect.getfile(fun)
    lines, lineno = inspect.getsourcelines(fun)

    # decorator line + function definition line (I realize this is largely
    # imperfect, but it's only for reporting in our tests) - empty first line
    # stripped away.
    lineno += 1

    @functools.wraps(fun)
    def newfun(self):
        entries, errors, options = parse_string(textwrap.dedent(fun.__doc__),
                                                report_filename=filename,
                                                report_firstline=lineno)
        return fun(self, entries, errors, options)
    return newfun



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
