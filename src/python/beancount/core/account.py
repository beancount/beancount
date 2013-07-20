"""Account object.

These account objects are rather simple and dumb; they do not contain the list
of their associated postings. This is achieved by building a realization; see
realization.py for details.
"""
import re
from collections import namedtuple


# A type used to represent an account read in.
Account = namedtuple('Account', 'name type')

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

# FIXME: This may not be hard-coded, needs to be read from options.
TYPES_ORDER = dict((x,i) for (i,x) in enumerate('Assets Liabilities Equity Income Expenses'.split()))

def account_name_type(name):
    """Return the type of this account's name."""
    assert isinstance(name, str)
    atype = name.split(':')[0]
    assert atype in TYPES_ORDER
    return atype

def is_account_name(string):
    """Return true if the given string is an account name."""
    return re.match('(Asset|Liabilities|Equity|Income|Expenses)', string)

def is_account_name_root(account_name):
    """Return true if the account name is one of the root accounts."""
    return ':' not in account_name

def is_balance_sheet_account(account):
    return account.type in ('Assets', 'Liabilities', 'Equity')
    # FIXME: Remove these hardcoded strings; use the options instead.

def is_balance_sheet_account_name(account_name, options):
    return account_name_type(account_name) in (
        options[x] for x in ('name_assets',
                             'name_liabilities',
                             'name_equity'))

def is_income_statement_account(account):
    return account.type in ('Income', 'Expenses')
    # FIXME: Remove these hardcoded strings; use the options instead.


def accountify_dict(string_dict):
    """Convert the dictionary itms that have values which are account names into
    Account instances. This is a simple core convenience designed to be used by the
    importers, so that configurations can be specified in terms of strings, like this:

       {'asset': 'Assets:US:Checking', <---- See how this is just a string.
        ...}

    """
    return {key: account_from_name(value) if is_account_name(value) else value
            for key, value in string_dict.items()}
