"""Summarization of entries.

This code is used to summarize a sequence of entries (e.g. during a time period)
into a few "opening balance" entries. This is when computing a balance sheet for
a specific time period: we don't want to see the entries from before some period
of time, so we fold them into a single transaction per account that has the sum
total amount of that account.
"""
import datetime
import logging
from collections import defaultdict

from beancount.core import inventory
from beancount.core.data import Transaction, Open, Close
from beancount.core.data import FileLocation, Posting
from beancount.core import flags
from beancount.core.account import is_income_statement_account


def clamp(entries, begin_date, end_date,
          options,
          account_previous_earnings,
          account_previous_balances,
          account_previous_conversions):
    """Filter entries to include only those between begin and end dates.

    This routine performs the standard procedure required to produce reports
    only for entries between two dates. That is, it

    - transfers the income and expense balances to a "previous retained
      earnings" account,
    - summarizes all the entries before the begin date to an "opening balances"
      account
    - truncates the entries to only those before the end date.

    A new list of entries is returned, and the index that points to the first
    original transaction (this is used to create the opening balances report,
    i.e., the balance sheet with only the summarized entries).
    """

    income_statement_account_pred = (
        lambda account: is_income_statement_account(account, options))

    # Transfer income and expenses before the period to equity.
    entries = transfer_balances(entries, begin_date,
                                income_statement_account_pred, account_previous_earnings)

    # Summarize all the previous balances.
    entries, index = summarize(entries, begin_date, account_previous_balances)

    # Truncate the entries after this.
    entries = truncate(entries, end_date)

    # Insert conversion entries.
    entries = conversions(entries, account_previous_conversions, begin_date)

    return entries, index


def close(entries,
          options,
          account_current_earnings,
          account_current_conversions):
    """Transfer net income to equity and insert a final conversion entry."""

    income_statement_account_pred = (
        lambda account: is_income_statement_account(account, options))

    # Transfer the balances as net-income.
    entries = transfer_balances(entries, None,
                                income_statement_account_pred, account_current_earnings)

    # Insert final conversion entries.
    entries = conversions(entries, account_current_conversions, None)

    return entries


def transfer_balances(entries, date, account_pred, transfer_account):
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

    # We need to insert the entries at the end of the previous day.
    if date:
        transfer_date = date - datetime.timedelta(days=1)
    else:
        transfer_date = entries[-1].date

    # Create transfer entries.
    transfer_entries = create_entries_from_balances(
        transfer_balances, transfer_date, transfer_account, False,
        '<summarize>', flags.FLAG_TRANSFER,
        "Transfer balance for '{account.name}' as of {date} (Transfer Balance)")

    # Split the new entries in a new list.
    return (entries[:index] + transfer_entries + entries[index:])


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
        '<summarize>', flags.FLAG_SUMMARIZE,
        "Opening balance for '{account.name}' as of {date} (Summarization)")

    # Gather the list of active open entries at date.
    open_entries = open_at_date(entries, date)

    # We will preserve the entries after the date.
    after_entries = [] if index is None else entries[index:]

    # Return a new list of entries and the index that points after the entries
    # were inserted.
    return ((open_entries + summarizing_entries + after_entries),
            len(open_entries) + len(summarizing_entries))


def conversions(entries, account, date=None):
    """Insert a conversion entry at date 'date' at the given account.

    Args:
      entries: A list of entries.
      account: The Account object to book against.
      date: The date at which to insert the conversion entry. The new
            entry will be inserted as the last entry and the date just previous
            to this date.
    Returns:
      A modified list of entries.
    """

    # Compute the balance at the given date.
    balance = inventory.Inventory()
    for index, entry in enumerate(entries):
        if not (date is None or entry.date < date):
            break
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                balance.add_position(posting.position, allow_negative=True)

    # Early exit if there is nothing to do.
    if balance.is_empty():
        return entries

    new_entries = list(entries)

    last_date = entries[-1].date - datetime.timedelta(days=1)

    fileloc = FileLocation('<conversions>', -1)
    narration = 'Conversion for {}'.format(balance)
    conversion_entry = Transaction(fileloc, last_date, flags.FLAG_CONVERSIONS, None, narration, None, None, [])
    for position in balance.get_cost().get_positions():
        # FIXME: Set the cost to zero here to maintain the balance invariant.
        # (This is the only single place we cheap on the balance rule in the
        # entire system and this is necessary; see documentation on
        # Conversions.)
        conversion_entry.postings.append(
            Posting(conversion_entry, account, -position, None, None))

    new_entries.insert(index, conversion_entry)

    return new_entries


def truncate(entries, date):
    """Filter out all the entries at and after date. Returns a new list of entries."""
    # FIXME: Do a bisect here, the list is sorted.
    new_entries = []
    for entry in entries:
        if entry.date >= date:
            break # Stop here, we're assuming the entries are sorted.
        new_entries.append(entry)
    return new_entries


def create_entries_from_balances(balances, date, other_account, direction,
                                 filename, flag, narration_template):
    """"Create a list of new entries to transfer the amounts in the 'balances' dict
    to/from other_account. If 'direction' is True, the new entries transfer TO
    the balances account from the other account; otherwise the new entries
    transfer FROM the balances into the other account.

    IMPORTANT! The other leg of the transfers have to be carried out AT COST in
    order for the sum total of all the resulting entries to reflect the correct
    final positions held.
    """

    new_entries = []
    for account, balance in sorted(balances.items()):

        # Don't create new entries where there is no balance.
        if not balance:
            continue

        fileloc = FileLocation(filename, 0)
        narration = narration_template.format(account=account, date=date)

        if not direction:
            balance = -balance


        postings = []
        new_entry = Transaction(
            fileloc, date, flag, None, narration, None, None, postings)

        for position in balance.get_positions():
            postings.append(Posting(new_entry, account, position, None, None))
            cost = position.get_cost_position()
            postings.append(Posting(new_entry, other_account, -cost, None, None))

        new_entries.append(new_entry)

    return new_entries


def sum_to_date(entries, date=None):
    """Sum up the balances per account for all entries striclty before 'date'.
    Return the index in the list of entries (or None, if all were before the
    date) and a dict of accounts to balance inventory.
    """

    # Sum up the balances up to the data of transfer.
    balances = defaultdict(inventory.Inventory)

    for index, entry in enumerate(entries):
        if date and entry.date >= date:
            break

        if isinstance(entry, Transaction):
            for posting in entry.postings:
                balance = balances[posting.account]
                try:
                    balance.add_position(posting.position, False)
                except ValueError as e:
                    logging.error("Error during realization: {}".format(e))

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
            assert entry.account not in open_entries, entry.account
            open_entries[entry.account] = (index, entry)

        elif isinstance(entry, Close):
            assert entry.account in open_entries
            del open_entries[entry.account]

    return [entry for (index, entry) in sorted(open_entries.values())]


def compute_total_balance(entries):
    """Sum up all the positions in the transactions in the list of entries and
    return an inventory of it."""

    balance = inventory.Inventory()
    for entry in entries:
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                balance.add_position(posting.position, allow_negative=True)
    return balance


def compute_balance_for_prefix(entries, account_prefix):
    """Compute the balance of all the accounts that match the given prefix."""

    balance = inventory.Inventory()
    for entry in entries:
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                if posting.account.name.startswith(account_prefix):
                    balance.add_position(posting.position, allow_negative=True)
    return balance
