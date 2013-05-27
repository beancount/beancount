"""Summarization of entries.

This code is used to summarize a sequence of entries (e.g. during a time period)
into a few "opening balance" entries. This is when computing a balance sheet for
a specific time period: we don't want to see the entries from before some period
of time, so we fold them into a single transaction per account that has the sum
total amount of that account.
"""
import datetime
from collections import defaultdict

from beancount2 import inventory
from beancount2.data import Transaction, Open, Close, Check
from beancount2.data import FileLocation, Posting
from beancount2.data import FLAG_SUMMARIZE, FLAG_TRANSFER


def summarize(entries, date, opening_account):
    """Summarize all the entries before date.

    This function replaces the transactions up to (and not including) the given
    date with a single opening balance transaction, for each account. It returns
    new entries, all of the ones before the given date having been replaced by a
    few summarization entries. (You can then "realize" that as a second step if
    desired.)

    The function returns two lists:

    - A list of Open entries and transactions that establish the new balance
      right before the given date;
    - A list of all the entries that follow.

    You can simply contatenate the two lists to obtain the final balance sheet
    entries. Using the first list only allows you to draw up the beginning
    balance sheet if desired.
    """

    # Compute balances at date.
    index, balances = sum_to_date(entries, date)

    # We need to insert the entries with a date previous to subsequent checks,
    # to maintain the invariant that all Check and Open directive show up before
    # any transaction.
    summarize_date = date - datetime.timedelta(days=1)

    # Create summarization / opening balance entries.
    summarizing_entries = create_entries_from_balances(
        balances, summarize_date, opening_account, True,
        '<summarize>', FLAG_SUMMARIZE,
        "Opening balance for '{account.name}' as of {date} (Summarization)")

    # Gather the list of active open entries at date.
    open_entries = open_at_date(entries, date)

    # Return the list of active open entries encountered before the date, the
    # summarized entires, and the rest.
    before_entries = open_entries + summarizing_entries

    # We will preserve the entries after the date.
    after_entries = [] if index is None else entries[index:]

    return (before_entries, after_entries)


def transfer(entries, date, account_pred, transfer_account):
    """For all accounts that match the 'account_pred' predicate, create new
    entries to transfer the balance at the given date 'date' from the account
    to the transfer account. Return a new list of entries, with the new
    transfer entries added in.

    This is used to transfer balances from income and expenses from a previous
    period to a "retained earnings" account. This is accomplished by creating
    new entries (like every other kind of safe manipulation of entry lists).

    (Note that inserting transfers breaks any Checks that are in the touched
    accounts. This is basically to be used only on accounts without checks, such
    as Income or Expense.)
    """

    # Compute balances at date.
    index, balances = sum_to_date(entries, date)
    if index is None:
        index = len(entries)

    # Filter out to keep only the accounts we want.
    transfer_balances = {account: balance
                         for account, balance in balances.items()
                         if account_pred(account)}

    # Create transfer entries.
    transfer_entries = create_entries_from_balances(
        transfer_balances, date, transfer_account, False,
        '<summarize>', FLAG_TRANSFER,
        "Transfer balance for '{account.name}' as of {date} (Transfer Balance)")

    # Skip Check and Open directives (from other accounts), to maintain the
    # invariant that they always show up before any transaction (the invariant
    # is a bit too strong, we could require it only per-account, but that's a
    # good thing, so we skip them here).
    for index in range(index, len(entries)):
        entry = entries[index]
        if not isinstance(entry, (Check, Open)):
            break

    # Split the new entries in a new list.
    return (entries[:index] + transfer_entries + entries[index:])


def create_entries_from_balances(balances, date, other_account, direction,
                                 filename, flag, narration):
    """"Create a list of new entries to transfer the amounts in the 'balances' dict
    to/from other_account. If 'direction' is True, the new entries transfer TO
    the balances account from the other account; otherwise the new entries
    transfer FROM the balances into the other account."""

    new_entries = []
    for account, balance in sorted(balances.items()):

        # Don't create new entries where there is no balance.
        if not balance:
            continue

        fileloc = FileLocation(filename, 0)
        narration = narration.format(account=account, date=date)

        if not direction:
            balance = -balance

        postings = []
        for position in balance.get_positions():
            postings.append(Posting(account, position, None, None))
            postings.append(Posting(other_account, -position, None, None))

        new_entry = Transaction(
            fileloc, date, flag, None, narration, set(), postings)

        new_entries.append(new_entry)

    return new_entries


def sum_to_date(entries, date):
    """Sum up the balances per account for all entries striclty before 'date'.
    Return the index in the list of entries (or None, if all were before the
    date) and a dict of accounts to balance inventory.
    """

    # Sum up the balances up to the data of transfer.
    balances = defaultdict(inventory.Inventory)

    for index, entry in enumerate(entries):
        if entry.date >= date:
            break

        if isinstance(entry, Transaction):
            for posting in entry.postings:
                balance = balances[posting.account]
                balance.add_position(posting.position, False)

    else:
        index = None

    return index, balances


def open_at_date(entries, date):
    """Gather the list of active (i.e., non-closed) entries at date."""

    open_entries = {}

    for index, entry in enumerate(entries):
        if entry.date >= date:
            break

        if isinstance(entry, Open):
            assert entry.account not in open_entries
            open_entries[entry.account] = (index, entry)

        elif isinstance(entry, Close):
            assert entry.account in open_entries
            del open_entries[entry.account]

    return [entry for (index, entry) in sorted(open_entries.values())]
