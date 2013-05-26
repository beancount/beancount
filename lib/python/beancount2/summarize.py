"""Summarization of entries.

This code is used to summarize a sequence of entries (e.g. during a time period)
into a few "opening balance" entries. This is when computing a balance sheet for
a specific time period: we don't want to see the entries from before some period
of time, so we fold them into a single transaction per account that has the sum
total amount of that account.
"""
from collections import defaultdict

from beancount2 import inventory
from beancount2.data import Transaction, Open, Close, FileLocation, Posting
from beancount2.data import FLAG_SUMMARIZE


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

    # Sum up the balances up to the data of summarization.
    balances = defaultdict(inventory.Inventory)

    # If an account is opened, keep its open directive to include it in the
    # summarized list of entries.
    open_entries = {}

    for index, entry in enumerate(entries):
        if entry.date >= date:
            break

        if isinstance(entry, Transaction):
            for posting in entry.postings:
                balance = balances[posting.account]
                balance.add_position(posting.position, False)

        elif isinstance(entry, Open):
            assert entry.account not in open_entries
            open_entries[entry.account] = entry

        elif isinstance(entry, Close):
            assert entry.account in open_entries
            del open_entries[entry.account]

    after_entries = entries[index:]

    # Create summarization / opening balance entries.
    summarizing_entries = []
    for account, balance in sorted(balances.items()):

        fileloc = FileLocation('<summarize>', 0)
        narration = "Opening balance for '{}' as of {} (Summarization)".format(
            account.name, date)

        postings = []
        for position in balance.get_positions():
            postings.append(Posting(account, position, None, None))
            postings.append(Posting(opening_account, -position, None, None))

        summarize_entry = Transaction(
            fileloc, date, FLAG_SUMMARIZE, None, narration, set(), postings)

        summarizing_entries.append(summarize_entry)

    # Return the list of active open entries encountered before the date, the
    # summarized entires, and the rest.
    before_entries = list(open_entries.values()) + summarizing_entries

    return (before_entries, after_entries)
