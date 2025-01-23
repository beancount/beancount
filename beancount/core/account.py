"""Functions that operate on account strings.

These account objects are rather simple and dumb; they do not contain the list
of their associated postings. This is achieved by building a realization; see
realization.py for details.
"""

from __future__ import annotations

__copyright__ = "Copyright (C) 2013-2020, 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"

import os
import re
import unicodedata
from os import path
from typing import Any
from typing import Callable
from typing import Iterable
from typing import Iterator

import regex

# Public type for accounts.
Account = str


# Component separator for account names.
sep = ":"


# Regular expression string that matches valid account name components.
# Categories are:
#   Lu: Uppercase letters.
#   L: All letters.
#   Nd: Decimal numbers.
ACC_COMP_TYPE_RE = r"[\p{Lu}][\p{L}\p{Nd}\-]*"
ACC_COMP_NAME_RE = r"[\p{Lu}\p{Nd}][\p{L}\p{Nd}\-]*"

# Regular expression string that matches a valid account. {5672c7270e1e}
ACCOUNT_RE = r"(?:{})(?:{}{})+".format(ACC_COMP_TYPE_RE, sep, ACC_COMP_NAME_RE)


# A dummy object which stands for the account type. Values in custom directives
# use this to disambiguate between string objects and account names.
TYPE = "<AccountDummy>"


def is_valid_root(string: Account) -> bool:
    """Return true if the given string is a valid root account name.
    This just check for valid syntax.

    Args:
      string: A string, to be checked for account name pattern.
    Returns:
      A boolean, true if the string has the form of a root account's name.
    """
    return isinstance(string, str) and bool(regex.fullmatch(ACC_COMP_TYPE_RE, string))


def is_valid_leaf(string: Account) -> bool:
    """Return true if the given string is a valid leaf account name.
    This just check for valid syntax.

    Args:
      string: A string, to be checked for account name pattern.
    Returns:
      A boolean, true if the string has the form of a leaf account's name.
    """
    return isinstance(string, str) and all(
        regex.fullmatch(ACC_COMP_NAME_RE, p) for p in string.split(":")
    )


def is_valid(string: Account) -> bool:
    """Return true if the given string is a valid account name.
    This does not check for the root account types, just the general syntax.

    Args:
      string: A string, to be checked for account name pattern.
    Returns:
      A boolean, true if the string has the form of an account's name.
    """
    return isinstance(string, str) and bool(regex.fullmatch(ACCOUNT_RE, string))


def join(*components: str) -> Account:
    """Join the names with the account separator.

    Args:
      *components: Strings, the components of an account name.
    Returns:
      A string, joined in a single account name.
    """
    return sep.join(components)


def split(account_name: Account) -> list[str]:
    """Split an account's name into its components.

    Args:
      account_name: A string, an account name.
    Returns:
      A list of strings, the components of the account name (without the separators).
    """
    return account_name.split(sep)


def parent(account_name: Account) -> Account | None:
    """Return the name of the parent account of the given account or None if at the root.

    Args:
      account_name: A string, the name of the account whose parent to return.
    Returns:
      A string, the name of the parent account of this account or None if at the root.
    """
    assert isinstance(account_name, str), account_name
    if not account_name:
        return None
    components = account_name.split(sep)
    components.pop(-1)
    return sep.join(components)


def leaf(account_name: Account) -> Account | None:
    """Get the name of the leaf of this account.

    Args:
      account_name: A string, the name of the account whose leaf name to return.
    Returns:
      A string, the name of the leaf of the account.
    """
    assert isinstance(account_name, str)
    return account_name.split(sep)[-1] if account_name else None


def sans_root(account_name: Account) -> Account | None:
    """Get the name of the account without the root.

    For example, an input of 'Assets:BofA:Checking' will produce 'BofA:Checking'.

    Args:
      account_name: A string, the name of the account whose leaf name to return.
    Returns:
      A string, the name of the non-root portion of this account name or None if
      the account is empty.
    """
    assert isinstance(account_name, str)
    components = account_name.split(sep)[1:]
    return join(*components) if account_name else None


def root(num_components: int, account_name: Account) -> Account:
    """Return the first few components of an account's name.

    Args:
      num_components: An integer, the number of components to return.
      account_name: A string, an account name.
    Returns:
      A string, the account root up to 'num_components' components.
    """
    return join(*(split(account_name)[:num_components]))


def has_component(account_name: Account, component: str) -> bool:
    """Return true if one of the account contains a given component.

    Args:
      account_name: A string, an account name.
      component: A string, a component of an account name. For instance,
        ``Food`` in ``Expenses:Food:Restaurant``. All components are considered.
    Returns:
      Boolean: true if the component is in the account. Note that a component
      name must be whole, that is ``NY`` is not in ``Expenses:Taxes:StateNY``.
    """
    return bool(re.search("(^|:){}(:|$)".format(component), account_name))


def commonprefix(accounts: Iterable[Account]) -> Account:
    """Return the common prefix of a list of account names.

    Args:
      accounts: A sequence of account name strings.
    Returns:
      A string, the common parent account. If none, returns an empty string.
    """
    accounts_lists = [account_.split(sep) for account_ in accounts]
    # Note: the os.path.commonprefix() function just happens to work here.
    # Inspect its code, and even the special case of no common prefix
    # works well with str.join() below.
    common_list = path.commonprefix(accounts_lists)
    return sep.join(common_list)


def walk(
    root_directory: str, followlinks: bool = True
) -> Iterator[tuple[str, Account, list[str], list[str]]]:
    """A version of os.walk() which yields directories that are valid account names.

    This only yields directories that are accounts... it skips the other ones.
    For convenience, it also yields you the account's name.

    Args:
      root_directory: A string, the name of the root of the hierarchy to be walked.
      followlinks: A boolean, whether os.walk() will follow links.
    Yields:
      Tuples of (root, account-name, dirs, files), similar to os.walk().
    """
    for root, dirs, files in os.walk(root_directory, followlinks=followlinks):
        dirs.sort()
        files.sort()
        relroot = root[len(root_directory) + 1 :]
        account_name = relroot.replace(os.sep, sep)
        # The regex module does not handle Unicode characters in decomposed
        # form. Python uses the normal form for representing string. However,
        # some filesystems use the canonical decomposition form.
        # See https://docs.python.org/3/library/unicodedata.html#unicodedata.normalize
        account_name = unicodedata.normalize("NFKC", account_name)
        if is_valid(account_name):
            yield root, account_name, dirs, files


def parent_matcher(account_name: Account) -> Callable[[str], Any]:
    """Build a predicate that returns whether an account is under the given one.

    Args:
      account_name: The name of the parent account we want to check for.
    Returns:
      A callable, which, when called, will return true if the given account is a
      child of ``account_name``.
    """
    pattern = re.compile(r"{}($|{})".format(re.escape(account_name), sep))
    return lambda s: bool(pattern.match(s))


def parents(account_name: Account) -> Iterator[Account]:
    """A generator of the names of the parents of this account, including this account.

    Args:
      account_name: The name of the account we want to start iterating from.
    Returns:
      A generator of account name strings.
    """
    current_account: str | None = account_name
    while current_account:
        yield current_account
        current_account = parent(current_account)


class AccountTransformer:
    """Account name transformer.

    This is used to support Win... huh, filesystems and platforms which do not
    support colon characters.

    Attributes:
      rsep: A character string, the new separator to use in link names.
    """

    def __init__(self, rsep: str | None = None):
        self.rsep = rsep

    def render(self, account_name: Account) -> str:
        "Convert the account name to a transformed account name."
        return account_name if self.rsep is None else account_name.replace(sep, self.rsep)

    def parse(self, transformed_name: str) -> Account:
        "Convert the transform account name to an account name."
        return (
            transformed_name
            if self.rsep is None
            else transformed_name.replace(self.rsep, sep)
        )
