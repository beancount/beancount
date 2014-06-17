"""Summarization of entries.

This code is used to summarize a sequence of entries (e.g. during a time period)
into a few "opening balance" entries. This is when computing a balance sheet for
a specific time period: we don't want to see the entries from before some period
of time, so we fold them into a single transaction per account that has the sum
total amount of that account.
"""
import datetime
import collections

from beancount.core import amount
from beancount.core import inventory
from beancount.core.data import Transaction, Open, Close
from beancount.core.data import FileLocation, Posting
from beancount.core import data
from beancount.core import flags
from beancount.core import realization
from beancount.core.account_types import is_income_statement_account
from beancount.ops import prices
from beancount.ops import balance
from beancount.utils import bisect_key


# The imaginary currency used to convert all units for conversions at a
# degenerate rate of zero.
TRANSFER_CURRENCY = 'NOTHING'


def clamp(entries, begin_date, end_date,
          account_types,
          account_previous_earnings,
          account_previous_balances,
          account_previous_conversions):
    """Filter entries to include only those between begin and end dates.

    Args:
      entries: A list of directive tuples.
      begin_date: A datetime.date instance, the beginning of the period.
      end_date: A datetime.date instance, one day beyond the end of the period.
      account_types: An instance of AccountTypes.
      account_previous_earnings: A string, the name of the account to transfer
        previous earnings from the income statement accounts to the balance
        sheet.
      account_previous_balances: A string, the name of the account in equity
        to transfer previous balances from, in order to initialize account
        balances at the beginning of the period. This is typically called an
        opening balances account.
      account_previous_conversions: A string, tne name of the equity account to
        book currency conversions against.

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
        lambda account: is_income_statement_account(account, account_types))

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
          account_types,
          account_current_earnings,
          account_current_conversions):
    """Transfer net income to equity and insert a final conversion entry."""

    income_statement_account_pred = (
        lambda account: is_income_statement_account(account, account_types))

    # Transfer the balances as net-income.
    entries = transfer_balances(entries, None,
                                income_statement_account_pred, account_current_earnings)

    # Insert final conversion entries.
    entries = conversions(entries, account_current_conversions, None)

    return entries










def transfer_balances(entries, date, account_pred, transfer_account):
    """Synthesize transactions to transfer balances from some accounts at a given date.

    For all accounts that match the 'account_pred' predicate, create new entries
    to transfer the balance at the given date from the account to the transfer
    account. This is used to transfer balances from income and expenses from a
    previous period to a "retained earnings" account. This is accomplished by
    creating new entries.

    Note that inserting transfers breaks any following balance checks that are
    in the tranferred accounts. For this reason, all balance assertion entries
    following the cutoff date for those accounts are removed from the list in
    output.

    Args:
      entries: A list of directives.
      date: A datetime.date instance, the date at which to make the transfer.
      account_pred: A predicate function that, given an account string, returns
        true if the account is meant to be transferred.
      transfer_account: A string, the name of the source account to be used on
        the transfer entries to receive balances at the given date.
    Returns:
      A new list of entries, with the new transfer entries added in.
    """
    # Don't bother doing anything if there are no entries.
    if not entries:
        return entries

    # Compute balances at date.
    balances, index = balance_by_account(entries, date)

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
        data.FileLocation('<transfer_balances>', 0), flags.FLAG_TRANSFER,
        "Transfer balance for '{account}' (Transfer balance)")

    # Remove balance assertions that occur after a transfer on an account that
    # has been transferred away; they would break.
    after_entries = [entry
                     for entry in entries[index:]
                     if not (isinstance(entry, balance.Balance) and
                             entry.account in transfer_balances)]

    # Split the new entries in a new list.
    return (entries[:index] + transfer_entries + after_entries)


def summarize(entries, date, opening_account):
    """Summarize all entries before a date by replacing then with summarization entries.

    This function replaces the transactions up to (and not including) the given
    date with a opening balance transactions, one for each account. It returns
    new entries, all of the transactions before the given date having been
    replaced by a few summarization entries, one for each account.

    Notes:
    - Open entries are preserved for active accounts.
    - The last relevant price entry for each (base, quote) pair is preserved.
    - All other entries before the cutoff date are culled.

    Args:
      entries: A list of directives.
      date: A datetime.date instance, the cutoff date before which to summararize.
      opening_account: A string, the name of the source account to book summarization
        entries against.
    Returns:
      The function returns a list of new entries and the integer index at which
      the entries on or after the cutoff date begin.
    """
    # Compute balances at date.
    balances, index = balance_by_account(entries, date)

    # We need to insert the entries with a date previous to subsequent checks,
    # to maintain ensure the open directives show up before any transaction.
    summarize_date = date - datetime.timedelta(days=1)

    # Create summarization / opening balance entries.
    summarizing_entries = create_entries_from_balances(
        balances, summarize_date, opening_account, True,
        data.FileLocation('<summarize>', 0), flags.FLAG_SUMMARIZE,
        "Opening balance for '{account}' (Summarization)")

    # Insert the last price entry for each commodity from before the date.
    price_entries = prices.get_last_price_entries(entries, date)

    # Gather the list of active open entries at date.
    open_entries = open_at_date(entries, date)

    # Compute entries before hte date and preserve the entries after the date.
    before_entries = open_entries + price_entries + summarizing_entries
    after_entries = entries[index:]

    # Return a new list of entries and the index that points after the entries
    # were inserted.
    return (before_entries + after_entries), len(before_entries)


def conversions(entries, conversion_account, date=None, transfer_currency=TRANSFER_CURRENCY):
    """Insert a conversion entry at date 'date' at the given account.

    Args:
      entries: A list of entries.
      conversion_account: The Account object to book against.
      date: The date before which to insert the conversion entry. The new
        entry will be inserted as the last entry of the date just previous
        to this date.
      transfer_currency: A string, the transfer currency to use for zero prices
        on the conversion entry.
    Returns:
      A modified list of entries.
    """
    # Compute the balance at the given date.
    conversion_balance = realization.compute_entries_balance(entries, date=date)

    # Early exit if there is nothing to do.
    if conversion_balance.is_empty():
        return entries

    # Calculate the index and the date for the new entry. We want to store it as
    # the last transaction of the day before.
    if date is not None:
        index = bisect_key.bisect_left_with_key(entries, date, key=lambda entry: entry.date)
        last_date = date - datetime.timedelta(days=1)
    else:
        index = len(entries)
        last_date = entries[-1].date

    fileloc = FileLocation('<conversions>', -1)
    narration = 'Conversion for {}'.format(conversion_balance)
    conversion_entry = Transaction(fileloc, last_date, flags.FLAG_CONVERSIONS,
                                   None, narration, None, None, [])
    for position in conversion_balance.get_cost().get_positions():
        # FIXME: Set the cost to zero here to maintain the balance invariant.
        # (This is the only single place we cheap on the balance rule in the
        # entire system and this is necessary; see documentation on
        # Conversions.)
        price = amount.Amount(amount.ZERO, transfer_currency)
        conversion_entry.postings.append(
            Posting(conversion_entry, conversion_account, -position, price, None))

    # Make a copy of the list of entries and insert the new transaction into it.
    new_entries = list(entries)
    new_entries.insert(index, conversion_entry)

    return new_entries


def truncate(entries, date):
    """Filter out all the entries at and after date. Returns a new list of entries.

    Args:
      entries: A sorted list of directives.
      date: A datetime.date instance.
    Returns:
      A truncated list of directives.
    """
    index = bisect_key.bisect_left_with_key(entries, date, key=lambda entry: entry.date)
    return entries[index:]


def create_entries_from_balances(balances, date, source_account, direction,
                                 fileloc, flag, narration_template):
    """"Create a list of entries from a dict of balances.

    This method creates a list of new entries to transfer the amounts in the
    'balances' dict to/from another account specified in 'source_account'.

    The balancing posting is created with the equivalent at cost. In other
    words, if you attempt to balance 10 GOOG {500 USD}, this will synthesize a
    posting with this position on one leg, and with 5000 USD on the
    'source_account' leg.

    Args:
      balances: A dict of account name strings to Inventory instances.
      date: A datetime.date object, the date at which to create the transaction.
      source_account: A string, the name of the account to pull the balances
        from. This is the magician's hat to pull the rabbit from.
      direction: If 'direction' is True, the new entries transfer TO the
        balances account from the source account; otherwise the new entries
        transfer FROM the balances into the source account.
      fileloc: An instance of data.FileLocation to use to indicate the source of
        the transactions
      flag: A string, the flag to use for the transactinos.
      narration_template: A format string for creating the narration. It is
        formatted with 'account' and 'date' replacement variables.
    Returns:
      A list of newly synthesizes Transaction entries.
    """
    new_entries = []
    for account, account_balance in sorted(balances.items()):

        # Don't create new entries where there is no balance.
        if account_balance.is_empty():
            continue

        narration = narration_template.format(account=account, date=date)

        if not direction:
            account_balance = -account_balance

        postings = []
        new_entry = Transaction(
            fileloc, date, flag, None, narration, None, None, postings)

        for position in account_balance.get_positions():
            postings.append(Posting(new_entry, account, position, None, None))
            cost = position.get_cost_position()
            postings.append(Posting(new_entry, source_account, -cost, None, None))

        new_entries.append(new_entry)

    return new_entries


def balance_by_account(entries, date=None):
    """Sum up the balance per account for all entries strictly before 'date'.

    Args:
      entries: A list of directives.
      date: An optional datetime.date instance. If provided, stop accumulating
        on and after this date. This is useful for summarization before a
        specific date.
    Returns:
      A pair of a dict of account string to instance Inventory (the balance of
      this account before the given date), and the index in the list of entries
      where the date was encountered. If all entries are located before the
      cutoff date, an index one beyond the last entry is returned.
    """
    balances = collections.defaultdict(inventory.Inventory)
    for index, entry in enumerate(entries):
        if date and entry.date >= date:
            break

        if isinstance(entry, Transaction):
            for posting in entry.postings:
                account_balance = balances[posting.account]

                # Note: We must allow negative lots at cost, because this may be
                # used to reduce a filtered list of entries which may not
                # include the entries necessary to keep units at cost always
                # above zero. The only summation that is guaranteed to be above
                # zero is if all the entries are being summed together, no
                # entries are filtered, at least for a particular account's
                # postings.
                account_balance.add_position(posting.position, True)
    else:
        index = len(entries)

    return balances, index


def open_at_date(entries, date):
    """Gather the list of active Open entries at date.

    This returns the list of Open entries that have not been closed at the given
    date, in the same order they were observed in the document.

    Args:
      entries: A list of directives.
      date: The date at which
    Returns:
      A list of Open directives.
    """
    open_entries = {}
    for index, entry in enumerate(entries):
        if entry.date >= date:
            break

        if isinstance(entry, Open):
            try:
                ex_index, ex_entry = open_entries[entry.account]
                if entry.date < ex_entry.date:
                    open_entries[entry.account] = (index, entry)
            except KeyError:
                open_entries[entry.account] = (index, entry)

        elif isinstance(entry, Close):
            # If there is no coresponding open, don't raise an error.
            open_entries.pop(entry.account, None)

    return [entry for (index, entry) in sorted(open_entries.values())]
