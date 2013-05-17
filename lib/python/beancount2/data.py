"""
Basic data structures used to represent the Ledger entries.
"""
from collections import namedtuple
from cdecimal import Decimal


# Constants.
ZERO = Decimal()


# An 'Amount' is a representation of an amount of a particular units.
Amount = namedtuple('Amount', 'number currency')

def AmountS(number_string, currency):
    """Create an Amount instance with a string. For convenience."""
    return Amount(Decimal(number_string), currency)

def mult_amount(amount, number):
    """Multiply the given amount by a number."""
    return Amount(amount.number * number, amount.currency)


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

def account_type(name):
    """Return the type of this account's name."""
    return name.split(':')[0]


# The location in a source file where the directive was read from.
FileLocation = namedtuple('FileLocation', 'filename lineno')

# All possible types of entries. See the documentation for these.
Open        = namedtuple('Open'        , 'fileloc date account account_id currencies')
Close       = namedtuple('Close'       , 'fileloc date account')
Pad         = namedtuple('Pad'         , 'fileloc date account account_pad')
Check       = namedtuple('Check'       , 'fileloc date account amount')
Transaction = namedtuple('Transaction' , 'fileloc date flag payee narration tags postings')
Event       = namedtuple('Event'       , 'fileloc date type description')
Note        = namedtuple('Note'        , 'fileloc date comment')
Price       = namedtuple('Price'       , 'fileloc date currency amount')

# Postings are contained in Transaction entries.
Posting = namedtuple('Posting', 'account position price flag')
