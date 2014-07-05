"""Account object.

These account objects are rather simple and dumb; they do not contain the list
of their associated postings. This is achieved by building a realization; see
realization.py for details.
"""
import re
import os
from os import path


# Component separator for account names.
sep = ':'


def is_valid(string):
    """Return true if the given string is a valid account name.
    This does not check for the root account types, just the general syntax.

    Args:
      string: A string, to be checked for account name pattern.
    Returns:
      A boolean, true if the string has the form of an account's name.
    """
    return (isinstance(string, str) and
            bool(re.match('([A-Z][A-Za-z0-9\-]+)({}[A-Z][A-Za-z0-9\-]+)+$'.format(sep),
                          string)))


def join(*components):
    """Join the names with the account separator.

    Args:
      *components: Strings, the components of an account name.
    Returns:
      A string, joined in a single account name.
    """
    return sep.join(components)


def parent(account_name):
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


def leaf(account_name):
    """Get the name of the leaf of this account.

    Args:
      account_name: A string, the name of the account whose leaf name to return.
    Returns:
      A string, the name of the leaf of the account.
    """
    assert isinstance(account_name, str)
    return account_name.split(sep)[-1] if account_name else None


def sans_root(account_name):
    """Get the name of the account without the root.

    For example, an in put of 'Assets:BofA:Checking' will produce 'BofA:Checking'.

    Args:
      account_name: A string, the name of the account whose leaf name to return.
    Returns:
      A string, the name of the non-root portion of this account name.
    """
    assert isinstance(account_name, str)
    components = account_name.split(sep)[1:]
    return join(*components) if account_name else None


def has_component(account, component):
    """Return true if one of the account contains a given component.

    Args:
      account: A string, an account name.
      component: A string, a component of an account name. For instance,
        'Food' in 'Expenses:Food:Restaurant'. All components are considered.
    Returns:
      A boolean, true if the component is in the account. Note that a component
      name must be whole, that is 'NY' is not in Expenses:Taxes:StateNY'.
    """
    return re.search('(^|:){}(:|$)'.format(component), account)


def commonprefix(accounts):
    """Return the common prefix of a list of account names.

    Args:
      accounts: A sequence of account name strings.
    Returns:
      A string, the common parent account. If none, returns an empty string.
    """
    accounts_lists = [account_.split(sep)
                      for account_ in accounts]
    # Note: the os.path.commonprefix() function just happens to work here.
    # Inspect its code, and even the special case of no common prefix
    # works well with str.join() below.
    common_list = path.commonprefix(accounts_lists)
    return sep.join(common_list)


def walk(root_directory):
    """A version of os.walk() which yields directories that are valid account names.

    This only yields directories that are accounts... it skips the other ones.
    For convenience, it also yields you the account's name.

    Args:
      root_directory: A string, the name of the root of the hierarchy to be walked.
    Yields:
      Tuples of (root, account-name, dirs, files), similar to os.walk().
    """
    for root, dirs, files in os.walk(root_directory):
        relroot = root[len(root_directory)+1:]
        account_name = relroot.replace(os.sep, sep)
        if is_valid(account_name):
            yield (root, account_name, dirs, files)
