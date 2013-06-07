"""
Basic data structures used to represent the Ledger entries.
"""
import io
import os
import datetime
from collections import namedtuple, defaultdict

from beancount2 import utils

# Attempt to import a fast Decimal implementation if you can.
try:
    from cdecimal import Decimal
except ImportError:
    from decimal import Decimal


# Constants.
ZERO = Decimal()


# Lookup for ordering a list of currencies: we want the majors first, then the
# cross-currencies, and then all the rest of the stuff a user might define
# (shorter strings first).
CURRENCY_ORDER = {
    # Majors
    'USD': 0,
    'EUR': 1,
    'JPY': 2,
    # Commonwealth
    'CAD': 3,
    'GBP': 4,
    'AUD': 5,
    'NZD': 6,
    'CHF': 7,
    # All the rest...
}


DISPLAY_QUANTIZE = Decimal('.01')

# An 'Amount' is a representation of an amount of a particular units.
class Amount:

    __slots__ = ('number', 'currency')

    def __init__(self, number, currency):
        self.number = Decimal(number) if isinstance(number, str) else number
        self.currency = currency

    def __str__(self):
        number = self.number
        if number == number.quantize(DISPLAY_QUANTIZE):
            return "{:.2f} {}".format(number, self.currency)
        else:
            return "{:f} {}".format(number, self.currency)
    __repr__ = __str__

    def __eq__(self, other):
        return (self.number, self.currency) == other

    def __hash__(self):
        return hash((self.number, self.currency))


# Note: We don't implement operators here in favour of the more explicit functional style.

def amount_sortkey(amount):
    """Sort by currency first."""
    return (amount.currency, amount.number)

def amount_mult(amount, number):
    """Multiply the given amount by a number."""
    return Amount(amount.number * number, amount.currency)

def amount_sub(amount1, amount2):
    """Multiply the given amount by a number."""
    assert amount1.currency == amount2.currency
    return Amount(amount1.number - amount2.number, amount1.currency)

# def neg_amount(amount):
#     return Amount(-amount.number, amount.currency)




# Lots are representation of a commodity with an optional associated cost and optional acquisition date.
# (There are considered immutable and shared between many objects; this makes everything faster.)
Lot = namedtuple('Lot', 'currency cost lot_date')


# A type used to represent an account read in.
Account = namedtuple('Account', 'name type')

def account_from_name(account_name):
    "Create a new account solely from its name."
    return Account(account_name, account_type(account_name))

def account_parent_name(name):
    """Return the name of the parent account of the given account."""
    components = name.split(':')
    components.pop(-1)
    return ':'.join(components)

def account_leaf_name(name):
    """Get the name of the leaf of this account."""
    return name.split(':')[-1]

def account_sortkey(account):
    """Sort a list of accounts, taking into account the type of account.
    Assets, Liabilities, Equity, Income and Expenses, in this order, then
    in the order of the account's name."""
    return (TYPES_ORDER[account.type], account.name)

def account_names_sortkey(account_name):
    """Sort a list of accounts, taking into account the type of account.
    Assets, Liabilities, Equity, Income and Expenses, in this order, then
    in the order of the account's name."""
    account_type = account_name.split(':')[0]
    return (TYPES_ORDER[account_type], account_name)

# FIXME: This may not be hard-coded, needs to be read from options.
TYPES_ORDER = dict((x,i) for (i,x) in enumerate('Assets Liabilities Equity Income Expenses'.split()))

def account_type(name):
    """Return the type of this account's name."""
    return name.split(':')[0]

def is_account_root(account_name):
    """Return true if the account name is one of the five root accounts."""
    return ':' not in account_name

def is_balance_sheet_account(account):
    return account.type in ('Assets', 'Liabilities', 'Equity')
    # FIXME: Remove these hardcoded strings; use the options instead.

def is_balance_sheet_account_name(account_name, options):
    return account_type(account_name) in (
        options[x] for x in ('name_assets',
                             'name_liabilities',
                             'name_equity'))

('Assets', 'Liabilities', 'Equity')
    # FIXME: Remove these hardcoded strings; use the options instead.

def is_income_statement_account(account):
    return account.type in ('Income', 'Expenses')
    # FIXME: Remove these hardcoded strings; use the options instead.


# The location in a source file where the directive was read from.
FileLocation = namedtuple('FileLocation', 'filename lineno')

def render_fileloc(fileloc):
    """Render the fileloc for errors in a way that it will be both detected by
    Emacs and align and rendered nicely."""
    return '{}:{:8}'.format(fileloc.filename, '{}:'.format(fileloc.lineno))

def print_errors(errors):
    # Report all the realization errors.
    for error in errors:
        print('{} {}'.format(render_fileloc(error.fileloc), error.message))


# All possible types of entries. See the documentation for these.
Open        = namedtuple('Open'        , 'fileloc date account currencies')
Close       = namedtuple('Close'       , 'fileloc date account')
Pad         = namedtuple('Pad'         , 'fileloc date account account_pad')
Check       = namedtuple('Check'       , 'fileloc date account amount errdiff')
Transaction = namedtuple('Transaction' , 'fileloc date flag payee narration tags links postings')
Note        = namedtuple('Note'        , 'fileloc date account comment')
Event       = namedtuple('Event'       , 'fileloc date type description')
Price       = namedtuple('Price'       , 'fileloc date currency amount')

# Postings are contained in Transaction entries.
# Note: a posting may only list within a single entry, and that's what the entry
# field should be set to.
Posting = namedtuple('Posting', 'entry account position price flag')


NoneType = type(None)

def sanity_check_types(entry):
    """Check that the entry and its postings has all correct data types."""
    from beancount2.core.inventory import Position
    assert isinstance(entry, (Transaction, Open, Close, Pad, Check, Note, Event, Price))
    assert isinstance(entry.fileloc, FileLocation)
    assert isinstance(entry.date, datetime.date)
    if isinstance(entry, Transaction):
        assert isinstance(entry.flag, (NoneType, str))
        assert isinstance(entry.payee, (NoneType, str))
        assert isinstance(entry.narration, (NoneType, str))
        assert isinstance(entry.tags, (NoneType, set, frozenset))
        assert isinstance(entry.links, (NoneType, set, frozenset))
        assert isinstance(entry.postings, list)
        for posting in entry.postings:
            assert isinstance(posting, Posting)
            assert posting.entry is entry
            assert isinstance(posting.account, Account)
            assert isinstance(posting.position, Position)
            assert isinstance(posting.price, (Amount, NoneType))
            assert isinstance(posting.flag, (str, NoneType))
        

def reparent_posting(posting, entry):
    "Create a new posting entry that has the parent field set."
    return Posting(entry,
                   posting.account, posting.position, posting.price, posting.flag)

def posting_has_conversion(posting):
    """Return true if this position involves a conversion. A conversion is when
    there is a price attached to the amount but no cost. This is used on
    transactions to convert between units."""
    return (posting.position.lot.cost is None and
            posting.price is not None)

def transaction_has_conversion(transaction):
    """Given a Transaction entry, return true if at least one of
    the postings has a price conversion (without an associated
    cost). These are the source of non-zero conversion balances."""
    assert isinstance(transaction, Transaction)
    for posting in transaction.postings:
        if posting_has_conversion(posting):
            return True
    return False




# Sort with the checks at the BEGINNING of the day.
SORT_ORDER = {Open: -2, Check: -1, Close: 1}

# Sort with the checks at the END of the day.
#SORT_ORDER = {Open: -2, Check: 1, Close: 2}


def entry_sortkey(entry):
    """Sort-key for entries. We sort by date, except that checks
    should be placed in front of every list of entries of that same day,
    in order to balance linearly."""
    return (entry.date, SORT_ORDER.get(type(entry), 0), entry.fileloc.lineno)


def posting_sortkey(entry):
    """Sort-key for entries or postings. We sort by date, except that checks
    should be placed in front of every list of entries of that same day,
    in order to balance linearly."""
    if isinstance(entry, Posting):
        entry = entry.entry
    return (entry.date, SORT_ORDER.get(type(entry), 0), entry.fileloc.lineno)



# Special flags
FLAG_OKAY      = '*' # Transactions that have been checked.
FLAG_WARNING   = '!' # Mark by the user as something to be looked at later on.
FLAG_PADDING   = 'P' # Transactions created from padding directives.
FLAG_SUMMARIZE = 'S' # Transactions created due to summarization.
FLAG_TRANSFER  = 'T' # Transactions created due to balance transfers.


class GetAccounts:
    """Gather the list of accounts from the list of entries.
    (This runs much, much faster than the corresponding generic routine.)
    """
    def __call__(self, entries):
        accounts = {}
        for entry in entries:
            for account in getattr(self, entry.__class__.__name__)(entry):
                accounts[account.name] = account
        return accounts

    def Transaction(_, entry):
        for posting in entry.postings:
            yield posting.account

    def Pad(_, entry):
        return (entry.account, entry.account_pad)

    def _one(_, entry):
        return (entry.account,)

    def _zero(_, entry):
        return ()

    Open = Close = Check = Note = _one
    Event = Price = _zero

def gather_accounts(entries):
    return GetAccounts()(entries)


#
# Common operations on lists of entries.
#

def get_min_max_dates(entries):
    """Return the minimum and amximum dates in the list of entries."""
    if entries:
        return (entries[0].date, entries[-1].date)
    else:
        return (None, None)


def get_active_years(entries):
    """Yield all the years that have at least one entry in them."""
    prev_year = None
    for entry in entries:
        year = entry.date.year
        if year != prev_year:
            prev_year = year
            yield year


def get_account_open_close(entries):
    """Fetch the open/close entries for each of the accounts."""

    open_closes_map = defaultdict(lambda: [None, None])
    for entry in utils.filter_type(entries, (Open, Close)):
        index = 0 if isinstance(entry, Open) else 1
        open_closes_map[entry.account][index] = entry

    return open_closes_map


#
# Conversion to text.
#

class EntryPrinter:
    "Multi-method for printing an entry."

    @classmethod
    def __call__(cls, obj):
        oss = io.StringIO()
        getattr(cls, obj.__class__.__name__)(cls, obj, oss)
        return oss.getvalue()

    def Transaction(_, entry, oss):
        # Compute the string for the payee and narration line.
        strings = []
        if entry.payee:
            strings.append('"{}" |'.format(entry.payee))
            format_string(entry.payee)
        if entry.narration:
            strings.append('"{}"'.format(entry.narration))

        if entry.tags:
            for tag in entry.tags:
                strings.append('#{}'.format(tag))
        if entry.links:
            for link in entry.links:
                strings.append('^{}'.format(link))

        oss.write('{e.date} {e.flag} {}\n'.format(' '.join(strings), e=entry))

        for posting in entry.postings:
            flag = '{} '.format(posting.flag) if posting.flag else ''
            assert posting.account is not None
            position = str(posting.position) if posting.position else ''
            oss.write('  {}{:64} {:>16} {:>16}\n'.format(flag, posting.account.name, position, posting.price or ''))

    def Check(_, entry, oss):
        oss.write('{e.date} check {e.account.name} {e.amount}\n'.format(e=entry))

    def Note(_, entry, oss):
        oss.write('{e.date} note {e.account.name} {e.comment}\n'.format(e=entry))

    def Pad(_, entry, oss):   raise NotImplementedError
    def Open(_, entry, oss):  raise NotImplementedError
    def Close(_, entry, oss): raise NotImplementedError
    def Event(_, entry, oss): raise NotImplementedError
    def Price(_, entry, oss): raise NotImplementedError


def format_string(string):
    return '"%s"' % string if string is not None else ''


def format_entry(entry):
    return EntryPrinter()(entry)
