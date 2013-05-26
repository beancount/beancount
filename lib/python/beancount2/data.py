"""
Basic data structures used to represent the Ledger entries.
"""
from collections import namedtuple

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


# An 'Amount' is a representation of an amount of a particular units.
class Amount:

    __slots__ = ('number', 'currency')

    def __init__(self, number, currency):
        self.number = Decimal(number) if isinstance(number, str) else number
        self.currency = currency

    def __str__(amount):
        return "{:.2f} {}".format(amount.number, amount.currency)
    __repr__ = __str__

    def __eq__(self, other):
        return (self.number, self.currency) == other

    def __hash__(self):
        return hash((self.number, self.currency))


def amount_sortkey(amount):
    """Sort by currency first."""
    return (amount.currency, amount.number)

def amount_mult(amount, number):
    """Multiply the given amount by a number."""
    return Amount(amount.number * number, amount.currency)

# def sub_amount(amount1, amount2):
#     """Multiply the given amount by a number."""
#     assert amount1.currency == amount2.currency
#     return Amount(amount1.number - amount2.number, amount1.currency)

# def neg_amount(amount):
#     return Amount(-amount.number, amount.currency)




# Lots are representation of a commodity with an optional associated cost and optional acquisition date.
Lot = namedtuple('Lot', 'currency cost lot_date')


# A type used to represent an account read in.
Account = namedtuple('Account', 'name type')

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

TYPES_ORDER = dict((x,i) for (i,x) in enumerate('Assets Liabilities Equity Income Expenses'.split()))

def account_type(name):
    """Return the type of this account's name."""
    return name.split(':')[0]

def is_balance_sheet_account(account):
    return account.type in ('Assets', 'Liabilities', 'Equity')

def is_income_statement_account(account):
    return account.type in ('Income', 'Expenses')


# The location in a source file where the directive was read from.
FileLocation = namedtuple('FileLocation', 'filename lineno')

def render_fileloc(fileloc):
    """Render the fileloc for errors in a way that it will be both detected by
    Emacs and align and rendered nicely."""
    return '{}:{:8}'.format(fileloc.filename, '{}:'.format(fileloc.lineno))


# All possible types of entries. See the documentation for these.
Open        = namedtuple('Open'        , 'fileloc date account account_id currencies')
Close       = namedtuple('Close'       , 'fileloc date account')
Pad         = namedtuple('Pad'         , 'fileloc date account account_pad')
Check       = namedtuple('Check'       , 'fileloc date account position')
Transaction = namedtuple('Transaction' , 'fileloc date flag payee narration tags postings')
Event       = namedtuple('Event'       , 'fileloc date type description')
Note        = namedtuple('Note'        , 'fileloc date account comment')
Price       = namedtuple('Price'       , 'fileloc date currency amount')

# Postings are contained in Transaction entries.
Posting = namedtuple('Posting', 'account position price flag')


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
