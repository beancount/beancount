"""Definition for global account types.

This is where we keep the global account types value and definition.

Note that it's unfortunate that we're using globals and side-effect here, but
this is the best solution in the short-term, the account types are used
in too many places to pass around that state everywhere. Maybe we change
this later on.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import re
from collections import namedtuple

from beancount.core import account


# A tuple that contains the names of the root accounts.
# Attributes:
#   assets: a str, the name of the prefix for the Asset subaccounts.
#   liabilities: a str, the name of the prefix for the Liabilities subaccounts.
#   equity: a str, the name of the prefix for the Equity subaccounts.
#   income: a str, the name of the prefix for the Income subaccounts.
#   expenses: a str, the name of the prefix for the Expenses subaccounts.
AccountTypes = namedtuple('AccountTypes', "assets liabilities equity income expenses")

# Default values for root accounts.
DEFAULT_ACCOUNT_TYPES = AccountTypes("Assets",
                                     "Liabilities",
                                     "Equity",
                                     "Income",
                                     "Expenses")


def get_account_sort_key(account_types, account_name):
    """Return a tuple that can be used to order/sort account names.

    Args:
      account_types: An instance of AccountTypes, a tuple of account type names.
    Returns:
      A function object to use as the optional 'key' argument to the sort
      function. It accepts a single argument, the account name to sort and
      produces a sortable key.
    """
    return (account_types.index(get_account_type(account_name)), account_name)


def get_account_type(account_name):
    """Return the type of this account's name.

    Warning: No check is made on the validity of the account type. This merely
    returns the root account of the corresponding account name.

    Args:
      account_name: A string, the name of the account whose type is to return.
    Returns:
      A string, the type of the account in 'account_name'.

    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    return account.split(account_name)[0]


def is_account_type(account_type, account_name):
    """Return the type of this account's name.

    Warning: No check is made on the validity of the account type. This merely
    returns the root account of the corresponding account name.

    Args:
      account_type: A string, the prefix type of the account.
      account_name: A string, the name of the account whose type is to return.
    Returns:
      A boolean, true if the account is of the given type.
    """
    return bool(re.match('^{}{}'.format(account_type, account.sep), account_name))


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
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    if account_types is not None:
        assert isinstance(account_types, AccountTypes), (
            "Account types has invalid type: {}".format(account_types))
        return account_name in account_types
    else:
        return (account_name and
                bool(re.match(r'([A-Z][A-Za-z0-9\-]+)$', account_name)))



def is_balance_sheet_account(account_name, account_types):
    """Return true if the given account is a balance sheet account.
    Assets, liabilities and equity accounts are balance sheet accounts.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is a balance sheet account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(account_types, AccountTypes), (
        "Account types has invalid type: {}".format(account_types))
    account_type = get_account_type(account_name)
    return account_type in (account_types.assets,
                            account_types.liabilities,
                            account_types.equity)


def is_income_statement_account(account_name, account_types):
    """Return true if the given account is an income statement account.
    Income and expense accounts are income statement accounts.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is an income statement account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(account_types, AccountTypes), (
        "Account types has invalid type: {}".format(account_types))
    account_type = get_account_type(account_name)
    return account_type in (account_types.income,
                            account_types.expenses)


def is_equity_account(account_name, account_types):
    """Return true if the given account is an equity account.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is an equityaccount.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(account_types, AccountTypes), (
        "Account types has invalid type: {}".format(account_types))
    account_type = get_account_type(account_name)
    return account_type == account_types.equity


def get_account_sign(account_name, account_types=None):
    """Return the sign of the normal balance of a particular account.

    Args:
      account_name: A string, the name of the account whose sign is to return.
      account_types: An optional instance of the current account_types.
    Returns:
      +1 or -1, depending on the account's type.
    """
    if account_types is None:
        account_types = DEFAULT_ACCOUNT_TYPES
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    account_type = get_account_type(account_name)
    return (+1
            if account_type in (account_types.assets,
                                account_types.expenses)
            else -1)
