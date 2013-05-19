"""
Sanity checks.
"""
# FIXME: TODO -- see TODO file, many to implement.

from collections import namedtuple

from beancount2.data import Account, Open, Close, Transaction
from beancount2 import utils


# An error from one of the checks.
CheckError = namedtuple('CheckError', 'fileloc message')


def check_open_close(entries, accounts):
    """Some entries may not be present more than once for each account or date.
    Open and Close are unique per account, for instance. Check is unique
    for each date. There are more. Return a list of errors on non-unique
    entries.
    """

    open_map = {}
    close_map = {}
    check_errors = []

    def check_one(entry, account):
        """Check a single entry."""
        open = open_map.get(account)
        if open is None or entry.date < open.date:
            check_errors.append(CheckError(entry.fileloc,
                                           "Entry before account {} opened.".format(account.name)))

        close = close_map.get(account)
        if close is not None and entry.date > close.date:
            check_errors.append(CheckError(entry.fileloc,
                                           "Entry after account {} closed.".format(account.name)))

    # Check all entries for missing open directives and references to accounts
    # which haven't been opened.
    for entry in entries:
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                check_one(entry, posting.account)

        elif isinstance(entry, Open):
            account = entry.account
            if account in open_map:
                check_errors.append(CheckError(entry.fileloc,
                                               "Duplicate open entry for {}.".format(account.name)))
            else:
                open_map[account] = entry

        elif isinstance(entry, Close):
            account = entry.account
            if account in close_map:
                check_errors.append(CheckError(entry.fileloc,
                                               "Duplicate close entry for {}.".format(account.name)))
            else:
                close_map[account] = entry

        elif hasattr(entry, 'account'):
            check_one(entry, entry.account)

    # Check to make sure that all accounts parsed have a corresponding open directive.
    for account in accounts:
        if account not in open_map:
            check_errors.append(CheckError(entry.fileloc,
                                           "No open directive for account {}.".format(account.name)))

    return check_errors, open_map, close_map


def check_unused_accounts(entries, accounts):
    """Find the list of accounts referred to by non-open entries,
    and check that against the total list of accounts. Accounts which are only
    referred to by open entries are probably unused."""

    # Find all the accounts referenced by entries which are not Open, and the
    # open directives for error reporting below.
    open_map = {}
    referenced_accounts = set()
    for entry in entries:
        if isinstance(entry, Open):
            open_map[entry.account] = entry
            continue
        referenced_accounts.update(get_ntuple_accounts(entry))

    # Unreferenced accounts are unused accounts.
    unused_accounts = set(accounts) - referenced_accounts

    # Create a list of suitable errors, with the location of the spurious Open
    # directives.
    return [CheckError(open_map[account].fileloc,
                       "Unused account {}.".format(account.name))
            for account in unused_accounts]


def get_ntuple_accounts(ntuple):
    """Return all the accounts referred to by this namedtuple instance.
    This function also works recursively on its members, and so it
    can be used for Transaction instances."""
    for attribute in ntuple:
        if isinstance(attribute, Account):
            yield attribute
        elif isinstance(attribute, list):
            for sub_attribute in attribute:
                for account in get_ntuple_accounts(sub_attribute):
                    yield account


def get_account_open_close(entries, accounts):
    """Fetch the open/close entries for each of the accounts."""

    open_closes_map = {account: [None, None]
                       for account in accounts}
    for entry in utils.filter_type(entries, (Open, Close)):
        index = 0 if isinstance(entry, Open) else 1
        open_closes_map[entry.account][index] = entry

    return open_closes_map


def check(entries, accounts):
    """Perform all the standard checks on parsed contents."""

    # Check for unused accounts.
    unused_errors = check_unused_accounts(entries, accounts)

    # Validate open/close directives and accounts referred outside of those.
    check_errors, _, _ = check_open_close(entries, accounts)

    errors = unused_errors + check_errors
    return errors
