"""
Sanity checks.
"""
# FIXME: TODO -- see TODO file, many to implement.

from collections import namedtuple

from beancount2.data import Open, Close
from beancount2 import utils
from beancount2.data import Transaction


# An error from one of the checks.
CheckError = namedtuple('CheckError', 'fileloc message')


def check_unique_entries(entries):
    """Some entries may not be present more than once for each account or date.
    Open and Close are unique per account, for instance. Check is unique
    for each date. There are more. Return a list of errors on non-unique
    entries.
    """
    # Check for uniqueness of open and close entries per account.
    # for entry in entries:

    
    # FIXME: continue here


    # Check for uniqueness by date of checks





def check_open_close(entries, accounts):
    """Ensure that each of the accounts seen do not have entries referring to
    them outside of their open and, if present, close directives. It also
    checks that each of the given accounts from the list has at least an
    open directive. This check returns a list of CheckError instances."""

    # Get a mapping of the open/close directives available for each account.
    extents = get_account_open_close(entries, accounts)

    check_errors = []

    def check_one(entry, account):
        open, close = extents[account]
        if open is not None and entry.date < open.date:
            check_errors.append(CheckError(entry.fileloc, "Entry before account opened."))
            print()
            print(account)
            print(entry.date, open.date)
            print(open)
            print(entry)
        if close is not None and entry.date > close.date:
            check_errors.append(CheckError(entry.fileloc, "Entry after account closed."))

    for entry in entries:
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                check_one(entry, posting.account)
        elif hasattr(entry, 'account'):
            check_one(entry, entry.account)

    return check_errors


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
    errors = check_open_close(entries, accounts)
    for error in errors:
        print('{fileloc.filename}:{fileloc.lineno}: {error.message}'.format(fileloc=error.fileloc, error=error))
