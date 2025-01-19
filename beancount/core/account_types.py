"""Definition for global account types.

This is where we keep the global account types value and definition.

Note that it's unfortunate that we're using globals and side-effect here, but
this is the best solution in the short-term, the account types are used
in too many places to pass around that state everywhere. Maybe we change
this later on.
"""

from __future__ import annotations

__copyright__ = "Copyright (C) 2014-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import re
from typing import TYPE_CHECKING
from typing import NamedTuple

from beancount.core import account

if TYPE_CHECKING:
    from beancount.core.account import Account


class AccountTypes(NamedTuple):
    """A tuple that contains the names of the root accounts."""

    assets: str  # the name of the prefix for the Asset subaccounts.
    liabilities: str  # the name of the prefix for the Liabilities subaccounts.
    equity: str  # the name of the prefix for the Equity subaccounts.
    income: str  # the name of the prefix for the Income subaccounts.
    expenses: str  # the name of the prefix for the Expenses subaccounts.


# Default values for root accounts.
DEFAULT_ACCOUNT_TYPES = AccountTypes(
    "Assets", "Liabilities", "Equity", "Income", "Expenses"
)


def get_account_type(account_name: Account) -> str:
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


def get_account_sort_key(
    account_types: AccountTypes, account_name: Account
) -> tuple[int, Account]:
    """Return a tuple that can be used to order/sort account names.

    Args:
      account_types: An instance of AccountTypes, a tuple of account type names.
    Returns:
      An object to use as the 'key' argument to the sort function.
    """
    return (account_types.index(get_account_type(account_name)), account_name)


def is_account_type(account_type: str, account_name: Account) -> bool:
    """Return the type of this account's name.

    Warning: No check is made on the validity of the account type. This merely
    returns the root account of the corresponding account name.

    Args:
      account_type: A string, the prefix type of the account.
      account_name: A string, the name of the account whose type is to return.
    Returns:
      A boolean, true if the account is of the given type.
    """
    return bool(re.match("^{}{}".format(account_type, account.sep), account_name))


def is_root_account(account_name: Account) -> bool:
    """Return true if the account name is a root account.

    This function does not verify whether the account root is a valid
    one, just that it is a root account or not.

    Args:
      account_name: A string, the name of the account to check for.
    Returns:
      A boolean, true if the account is root account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    return bool(account_name) and bool(re.match(r"([A-Z][A-Za-z0-9\-]+)$", account_name))


def is_balance_sheet_account(account_name: Account, account_types: AccountTypes) -> bool:
    """Return true if the given account is a balance sheet account.
    Assets, liabilities and equity accounts are balance sheet accounts.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is a balance sheet account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(
        account_types, AccountTypes
    ), "Account types has invalid type: {}".format(account_types)
    account_type = get_account_type(account_name)
    return account_type in (
        account_types.assets,
        account_types.liabilities,
        account_types.equity,
    )


def is_income_statement_account(account_name: Account, account_types: AccountTypes) -> bool:
    """Return true if the given account is an income statement account.
    Income and expense accounts are income statement accounts.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is an income statement account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(
        account_types, AccountTypes
    ), "Account types has invalid type: {}".format(account_types)
    account_type = get_account_type(account_name)
    return account_type in (account_types.income, account_types.expenses)


def is_equity_account(account_name: Account, account_types: AccountTypes) -> bool:
    """Return true if the given account is an equity account.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is an equity account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(
        account_types, AccountTypes
    ), "Account types has invalid type: {}".format(account_types)
    account_type = get_account_type(account_name)
    return account_type == account_types.equity


def is_inverted_account(account_name: Account, account_types: AccountTypes) -> bool:
    """Return true if the given account has inverted signs.

    An inverted sign is the inverse as you'd expect in an external report, i.e.,
    with all positive signs expected.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account has an inverted sign.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(
        account_types, AccountTypes
    ), "Account types has invalid type: {}".format(account_types)
    account_type = get_account_type(account_name)
    return account_type in (
        account_types.liabilities,
        account_types.income,
        account_types.equity,
    )


def get_account_sign(
    account_name: Account, account_types: AccountTypes | None = None
) -> int:
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
    return +1 if account_type in (account_types.assets, account_types.expenses) else -1
