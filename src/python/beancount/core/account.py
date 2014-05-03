"""Account object.

These account objects are rather simple and dumb; they do not contain the list
of their associated postings. This is achieved by building a realization; see
realization.py for details.
"""
import re

from beancount.core import account_types


# Component separator for account names.
sep = ':'


def join(*components):
    """Join the names with the account separator.

    Args:
      *components: Strings, the components of an account name.
    Returns:
      A string, joined in a single account name.
    """
    return sep.join(components)


def account_name_parent(account_name):
    """Return the name of the parent account of the given account.

    Args:
      account_name: A string, the name of the account whose parent to return.
    Returns:
      A string, the name of the parent account of this account.
    """
    assert isinstance(account_name, str), account_name
    if not account_name:
        return None
    components = account_name.split(sep)
    components.pop(-1)
    return sep.join(components)


def account_name_leaf(account_name):
    """Get the name of the leaf of this account.

    Args:
      account_name: A string, the name of the account whose leaf name to return.
    Returns:
      A string, the name of the leaf of the account.
    """
    assert isinstance(account_name, str)
    return account_name.split(sep)[-1] if account_name else None











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
    return (account_types.TYPES_ORDER[type_], account_name)


def account_name_type(account_name):
    """Return the type of this account's name.

    Args:
      account_name: A string, the name of the account whose type is to return.
    Returns:
      A string, the type of the account in 'account_name'.
    """
    assert isinstance(account_name, str)
    atype = account_name.split(sep)[0]
    assert atype in account_types.ACCOUNT_TYPES, (
        account_name, atype, account_types.ACCOUNT_TYPES)
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
    return account_name in account_types.ACCOUNT_TYPES


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


# FIXME: This needs a bit of review, we can very likely do everything more
# consistently and simpler by just using strings with methods instead of Account
# types. In other words, we can get rid of the Account type and just deal with
# strings. Everything else should be simpler. Do this at some point.
