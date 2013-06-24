"""Getter functions that operate on lists of entries.
"""
from collections import defaultdict

from beancount import utils


# FIXME: Ideally this would live under ops.


#
# Common operations on lists of entries.
#

class GetAccounts:
    """Gather the list of accounts from the list of entries.
    (This runs much, much faster than the corresponding generic routine.)
    """
    def __call__(self, entries):
        accounts = {}
        for entry in entries:
            for account in getattr(self, entry.__class__.__name__)(entry):
                accounts[account.name] = account
        return accounts

    def Transaction(_, entry):
        for posting in entry.postings:
            yield posting.account

    def Pad(_, entry):
        return (entry.account, entry.account_pad)

    def _one(_, entry):
        return (entry.account,)

    def _zero(_, entry):
        return ()

    Open = Close = Check = Note = Document = _one
    Event = Price = _zero

def get_accounts(entries):
    return GetAccounts()(entries)


def get_all_tags(entries):
    "Return a list of all the tags seen in the given entries."
    all_tags = set()
    for entry in utils.filter_type(entries, data.Transaction):
        if entry.tags:
            all_tags.update(entry.tags)
    return all_tags


def get_all_payees(entries):
    "Return a list of all the unique payees seen in the given entries."
    all_payees = set()
    for entry in utils.filter_type(entries, data.Transaction):
        all_payees.add(entry.payee)
    all_payees.discard(None)
    return all_payees


def get_min_max_dates(entries):
    """Return the minimum and amximum dates in the list of entries."""
    if entries:
        return (entries[0].date, entries[-1].date)
    else:
        return (None, None)


def get_active_years(entries):
    """Yield all the years that have at least one entry in them."""
    prev_year = None
    for entry in entries:
        year = entry.date.year
        if year != prev_year:
            prev_year = year
            yield year


def get_account_open_close(entries):
    """Fetch the open/close entries for each of the accounts."""

    open_closes_map = defaultdict(lambda: [None, None])
    for entry in utils.filter_type(entries, (Open, Close)):
        index = 0 if isinstance(entry, Open) else 1
        open_closes_map[entry.account][index] = entry

    return open_closes_map


def get_currency_for_account(account, entries):
    """Find the single currency used in the given account.
    This assumes that there is exactly one currency.
    May return None."""

    for entry in utils.filter_type(entries, Open):
        if entry.account.name == account.name:
            found = entry
            break
    else:
        return None

    assert len(entry.currencies) == 1, (account, entry.currencies)
    return entry.currencies[0]
