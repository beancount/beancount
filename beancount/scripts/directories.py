"""Check that document directories mirror a list of accounts correctly."""

__copyright__ = "Copyright (C) 2013-2014, 2016-2017, 2024  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core import account
from beancount.core import getters


class ValidateDirectoryError(Exception):
    """A directory validation error."""


def validate_directory(accounts, document_dir):
    """Check a directory hierarchy against a list of valid accounts.

    Walk the directory hierarchy, and for all directories with names matching
    that of accounts (with ":" replaced with "/"), check that they refer to an
    account name declared in the given list.

    Args:
      account: A set or dict of account names.
      document_dir: A string, the root directory to walk and validate.
    Returns:
      An errors for each invalid directory name found.
    """
    # Generate all parent accounts in the account_set we're checking against, so
    # that parent directories with no corresponding account don't warn.
    accounts_with_parents = set(accounts)
    for account_ in accounts:
        while True:
            parent = account.parent(account_)
            if not parent:
                break
            if parent in accounts_with_parents:
                break
            accounts_with_parents.add(parent)
            account_ = parent

    errors = []
    for directory, account_name, _, _ in account.walk(document_dir):
        if account_name not in accounts_with_parents:
            errors.append(
                ValidateDirectoryError(
                    "Invalid directory '{}': no corresponding account '{}'".format(
                        directory, account_name
                    )
                )
            )
    return errors


def validate_directories(entries, document_dirs):
    """Validate a directory hierarchy against a ledger's account names.

    Read a ledger's list of account names and check that all the capitalized
    subdirectory names under the given roots match the account names.

    Args:
      entries: A list of directives.
      document_dirs: A list of string, the directory roots to walk and validate.
    """

    # Get the list of accounts declared in the ledge.
    accounts = getters.get_accounts(entries)

    # For each of the roots, validate the hierarchy of directories.
    for document_dir in document_dirs:
        errors = validate_directory(accounts, document_dir)
        for error in errors:
            print("ERROR: {}".format(error))
