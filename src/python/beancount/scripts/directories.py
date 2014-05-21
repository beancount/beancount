"""Check that the auto-doc directories matches a list of accounts.
"""
import argparse

from beancount import load
from beancount.parser import documents
from beancount.core import getters
from beancount.core import account


class ValidateDirectoryError(Exception):
    """A directory validation error."""


def validate_directories(accounts_set, document_dir):
    """Check a directory hierarchy against a list of valid accounts.

    Walk the directory hierarchy, and for all directories with names matching
    that of accounts (with ":" replaced with "/"), check that they refer to an
    account name declared in the given list.

    Args:
      account_set: A set or dict of account names.
      document_dir: A string, the root directory to walk and validate.
    Returns:
      An errors for each invalid directory name found.
    """
    # Generate all parent accounts in the account_set we're checking against, so
    # that parent directories with no corresponding account don't warn.
    accounts_set_with_parents = accounts_set.copy()
    for account_ in accounts_set:
        while True:
            parent = account.account_name_parent(account_)
            if not parent:
                break
            if parent in accounts_set_with_parents:
                break
            accounts_set_with_parents.add(parent)
            account_ = parent

    errors = []
    for directory, account_name, _, _ in documents.walk_accounts(document_dir):
        if account_name not in accounts_set_with_parents:
            errors.append(ValidateDirectoryError(
                "Invalid directory '{}': no corresponding account '{}'".format(
                    directory, account_name)))
    return errors


def main():
    parser = argparse.ArgumentParser(__doc__)

    parser.add_argument('filename',
                        help='Beancount input filename')

    parser.add_argument('document_dirs', nargs='+',
                        help="Root directories of documents")

    opts = parser.parse_args()

    # Load up the ledger file, print errors.
    entries, _, _ = load(opts.filename, do_print_errors=True)

    # Get the list of accounts declared in the ledge.
    accounts = getters.get_accounts(entries)
    accounts_set = set(accounts)

    # For each of the roots, validate the hierarchy of directories.
    for document_dir in opts.document_dirs:
        errors = validate_directories(accounts_set, document_dir)
        for error in errors:
            print("ERROR: {}".format(error))


if __name__ == '__main__':
    main()
