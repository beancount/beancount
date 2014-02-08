"""Account object.

These account objects are rather simple and dumb; they do not contain the list
of their associated postings. This is achieved by building a realization; see
realization.py for details.
"""
import re
from collections import namedtuple


# A type used to represent an account read in.
Account = namedtuple('Account', 'name type')

# Default values for root accounts.
DEFAULT_ASSETS      = "Assets"
DEFAULT_LIABILITIES = "Liabilities"
DEFAULT_EQUITY      = "Equity"
DEFAULT_INCOME      = "Income"
DEFAULT_EXPENSES    = "Expenses"


def update_default_valid_account_names():
    """Set the globals to the default account names."""
    global TYPES_ORDER
    TYPES_ORDER = dict((x,i) for (i,x) in enumerate((DEFAULT_ASSETS,
                                                     DEFAULT_LIABILITIES,
                                                     DEFAULT_EQUITY,
                                                     DEFAULT_INCOME,
                                                     DEFAULT_EXPENSES)))

def update_valid_account_names(account_types):
    """Update the globals used to validate root account names.

    Args:
      account_types: an instance of AccountTypes, the account names.
    """
    global TYPES_ORDER
    TYPES_ORDER = dict((x,i) for (i,x) in enumerate(account_types))

# FIXME: This is the only place where we have globals for this.
# However, we need to thread the account_types all over the myriad
# functions which call functions in this package which use this,
# and I (blais) haven't decided whether the extra validation is
# really worth it yet. Maybe we can just do without and make an
# assumption. To review later.
TYPES_ORDER = None
update_default_valid_account_names()


def account_from_name(account_name):
    "Create a new account solely from its name."
    assert isinstance(account_name, str)
    atype = account_name_type(account_name)
    assert atype in TYPES_ORDER, "Invalid account type: {}".format(atype)
    return Account(account_name, atype)

def account_name_parent(name):
    """Return the name of the parent account of the given account."""
    assert isinstance(name, str)
    if not name:
        return None
    components = name.split(':')
    components.pop(-1)
    return ':'.join(components)

def account_name_leaf(name):
    """Get the name of the leaf of this account."""
    return name.split(':')[-1] if name else None

def account_sortkey(account):
    """Sort a list of accounts, taking into account the type of account.
    Assets, Liabilities, Equity, Income and Expenses, in this order, then
    in the order of the account's name."""
    return (TYPES_ORDER[account.type], account.name)

def account_name_sortkey(account_name):
    """Sort a list of accounts, taking into account the type of account.
    Assets, Liabilities, Equity, Income and Expenses, in this order, then
    in the order of the account's name."""
    type_ = account_name_type(account_name)
    return (TYPES_ORDER[type_], account_name)

def account_name_type(name):
    """Return the type of this account's name."""
    assert isinstance(name, str)
    atype = name.split(':')[0]
    assert atype in TYPES_ORDER, (name, atype, TYPES_ORDER)
    return atype

def is_account_name(string):
    """Return true if the given string is an account name."""
    return bool(re.match(
        '([A-Z][A-Za-z0-9\-]+)(:[A-Z][A-Za-z0-9\-]+)+$', string))

def is_account_name_root(account_name):
    """Return true if the account name is one of the root accounts."""
    return account_name in TYPES_ORDER

def is_balance_sheet_account(account, options):
    return account.type in (options[x] for x in ('name_assets',
                                                 'name_liabilities',
                                                 'name_equity'))

def is_income_statement_account(account, options):
    return account.type in (options[x] for x in ('name_income',
                                                 'name_expenses'))

def accountify_dict(string_dict):
    """Convert the dictionary items that have values which are account names into
    Account instances. This is a simple core convenience designed to be used by the
    importers, so that configurations can be specified in terms of strings, like this:

       {'asset': 'Assets:US:Checking', <---- See how this is just a string.
        ...}

    """
    return {key: account_from_name(value)
            if isinstance(value, str) and is_account_name(value) else value
            for key, value in string_dict.items()}
