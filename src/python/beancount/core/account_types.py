"""Definition for global account types.

This is where we keep the global account types value and definition.

Note that it's unfortunate that we're using glboals and side-effect here, but
this is the best solution in the short-term, the account types are used
in too many places to pass around that state everywhere. Maybe we change
this later on.
"""
import re
from collections import namedtuple

from beancount.core import account


# A tuple that contains the names of the root accounts. This is a subset of options.
# Attributes:
#   assets: a str, the name of the prefix for the Asset subaccounts.
#   liabilities: a str, the name of the prefix for the Liabilities subaccounts.
#   equity: a str, the name of the prefix for the Equity subaccounts.
#   income: a str, the name of the prefix for the Income subaccounts.
#   expenses: a str, the name of the prefix for the Expenses subaccounts.
AccountTypes = namedtuple('AccountTypes', "assets liabilities equity income expenses")

# The global value of account types.
ACCOUNT_TYPES = None
TYPES_ORDER = None

# Default values for root accounts.
DEFAULT_ACCOUNT_TYPES = AccountTypes("Assets",
                                     "Liabilities",
                                     "Equity",
                                     "Income",
                                     "Expenses")


def update_valid_account_names(account_types=DEFAULT_ACCOUNT_TYPES):
    """Set the globals for the root account names. Calling this function without a
    parameter initializes the default names for the five root accounts for
    assets, liabilities, equity, income and expenses. This is used betwee
    parsing runs.

    Args:
      account_types: None to reset to default, or an instance of AccountTypes to
        set to specific values.
    """
    assert isinstance(account_types, (AccountTypes, type(None)))
    global ACCOUNT_TYPES, TYPES_ORDER
    ACCOUNT_TYPES = account_types
    TYPES_ORDER = dict((x, i) for (i, x) in enumerate(account_types))


update_valid_account_names(DEFAULT_ACCOUNT_TYPES)


def account_name_sortkey(account_name):
    """Sort a list of accounts, taking into account the type of account.
    Assets, Liabilities, Equity, Income and Expenses, in this order, then
    in the order of the account's name.

    Args:
      account_name: A string, the name of the account to sort.
    Returns:
      A tuple which is to be used as the sort key for lists of accounts.
    """
    assert isinstance(account_name, str)
    type_ = account_name_type(account_name)
    return (TYPES_ORDER[type_], account_name)


def account_name_type(account_name):
    """Return the type of this account's name.

    Args:
      account_name: A string, the name of the account whose type is to return.
    Returns:
      A string, the type of the account in 'account_name'.
    """
    assert isinstance(account_name, str)
    atype = account_name.split(account.sep)[0]
    assert atype in ACCOUNT_TYPES, (
        account_name, atype, ACCOUNT_TYPES)
    return atype


def is_account_name(string):
    """Return true if the given string is an account name.

    Args:
      string: A string, to be checked for account name pattern.
    Returns:
      A boolean, true if the string has the form of an account's name.
    """
    return (isinstance(string, str) and
            bool(re.match(
                '([A-Z][A-Za-z0-9\-]+)(:[A-Z][A-Za-z0-9\-]+)+$', string)))


def is_account_name_root(account_name):
    """Return true if the account name is one of the root accounts.

    Args:
      account_name: A string, the name of the account to check for.
    Returns:
      A boolean, true if the name is the name of a root account (same
      as an account type).
    """
    assert isinstance(account_name, str)
    return account_name in ACCOUNT_TYPES


def is_balance_sheet_account(account_name, options):
    """Return true if the given account is a balance sheet account.
    Assets, liabilities and equity accounts are balance sheet accounts.

    Args:
      account_name: A string, an account name.
      options: The options dictionary of a file.
    Returns:
      A boolean, true if the account is a balance sheet account.
    """
    assert isinstance(account_name, str)
    account_type = account_name_type(account_name)
    # FIXME: Use account_types.ACCOUNT_TYPES instead of options?
    return account_type in (options[x] for x in ('name_assets',
                                                 'name_liabilities',
                                                 'name_equity'))


def is_income_statement_account(account_name, options):
    """Return true if the given account is an income statement account.
    Income and expense accounts are income statement accounts.

    Args:
      account_name: A string, an account name.
      options: The options dictionary of a file.
    Returns:
      A boolean, true if the account is an income statement account.
    """
    assert isinstance(account_name, str)
    account_type = account_name_type(account_name)
    # FIXME: Use account_types.ACCOUNT_TYPES instead of options?
    return account_type in (options[x] for x in ('name_income',
                                                 'name_expenses'))


def is_equity_account(account_name, options):
    """Return true if the given account is an equity account.

    Args:
      account_name: A string, an account name.
      options: The options dictionary of a file.
    Returns:
      A boolean, true if the account is an equityaccount.
    """
    assert isinstance(account_name, str)
    account_type = account_name_type(account_name)
    # FIXME: Use account_types.ACCOUNT_TYPES instead of options?
    return account_type == options['name_equity']
