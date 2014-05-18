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
    global ACCOUNT_TYPES
    ACCOUNT_TYPES = account_types


update_valid_account_names(DEFAULT_ACCOUNT_TYPES)


def account_sortkey_fun(account_types):
    """Return a function that can be used to extract a key to sort account names.

    Args:
      account_types: An instance of AccountTypes, a tuple of account type names.
    Returns:
      A function object to use as the optional 'key' argument to the sort
      function. It accepts a single argument, the account name to sort and
      produces a sortable key.
    """
    assert isinstance(account_types, AccountTypes)
    return lambda account_name: (account_types.index(account_name_type(account_name)),
                                 account_name)


def account_name_type(account_name):
    """Return the type of this account's name.

    Warning: No check is made on the validity of the account type. This merely
    returns the root account of the corresponding account name.

    Args:
      account_name: A string, the name of the account whose type is to return.
    Returns:
      A string, the type of the account in 'account_name'.

    """
    assert isinstance(account_name, str), account_name
    return account_name.split(account.sep)[0]


def is_valid_account_name(string):
    """Return true if the given string is an account name.

    Args:
      string: A string, to be checked for account name pattern.
    Returns:
      A boolean, true if the string has the form of an account's name.
    """
    return (isinstance(string, str) and
            bool(re.match(
                '([A-Z][A-Za-z0-9\-]+)(:[A-Z][A-Za-z0-9\-]+)+$', string)))


def is_root_account(account_name, account_types=None):
    """Return true if the account name is a root account.
    This function does not verify whether the account root is a valid
    one, just that it is a root account or not.

    Args:
      account_name: A string, the name of the account to check for.
      account_types: An optional instance of the current account_types;
        if provided, we check against these values. If not provided, we
        merely check that name pattern is that of an account component with
        no separator.
    Returns:
      A boolean, true if the account is root account.
    """
    assert isinstance(account_name, str)
    if account_types is not None:
        assert isinstance(account_types, AccountTypes)
        return account_name in account_types
    else:
        return (account_name and
                bool(re.match('([A-Z][A-Za-z0-9\-]+)$', account_name)))


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
