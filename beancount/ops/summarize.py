"""Summarization of entries.

This code is used to summarize a sequence of entries (e.g. during a time period)
into a few "opening balance" entries. This is when computing a balance sheet for
a specific time period: we don't want to see the entries from before some period
of time, so we fold them into a single transaction per account that has the sum
total amount of that account.
"""

from __future__ import annotations

__copyright__ = "Copyright (C) 2013-2017, 2019-2021, 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime
from typing import TYPE_CHECKING
from typing import Callable

from beancount.core import amount
from beancount.core import convert
from beancount.core import data
from beancount.core import flags
from beancount.core import getters
from beancount.core import interpolate
from beancount.core import inventory
from beancount.core import prices
from beancount.core.account_types import is_income_statement_account
from beancount.core.data import Close
from beancount.core.data import Open
from beancount.core.data import Transaction
from beancount.core.number import ZERO
from beancount.parser import options
from beancount.utils import bisect_key

if TYPE_CHECKING:
    from beancount.core.account import Account
    from beancount.core.account_types import AccountTypes
    from beancount.core.data import Currency
    from beancount.core.data import Directives
    from beancount.loader import OptionsMap


def open(
    entries: Directives,
    date: datetime.date,
    account_types: AccountTypes,
    conversion_currency: Currency,
    account_earnings: Account,
    account_opening: Account,
    account_conversions: Account,
) -> tuple[Directives, int]:
    """Summarize entries before a date and transfer income/expenses to equity.

    This method essentially prepares a list of directives to contain only
    transactions that occur after a particular date. It truncates the past. To
    do so, it will

    1. Insert conversion transactions at the given open date, then

    2. Insert transactions at that date to move accumulated balances from before
       that date from the income and expenses accounts to an equity account, and
       finally

    3. It removes all the transactions previous to the date and replaces them by
       opening balances entries to bring the balances to the same amount.

    The result is a list of entries for which the income and expense accounts
    are beginning with a balance of zero, and all other accounts begin with a
    transaction that brings their balance to the expected amount. All the past
    has been summarized at that point.

    An index is returned to the first transaction past the balance opening
    transactions, so you can keep just those in order to render a balance sheet
    for only the opening balances.

    Args:
      entries: A list of directive tuples.
      date: A datetime.date instance, the date at which to do this.
      account_types: An instance of AccountTypes.
      conversion_currency: A string, the transfer currency to use for zero prices
        on the conversion entry.
      account_earnings: A string, the name of the account to transfer
        previous earnings from the income statement accounts to the balance
        sheet.
      account_opening: A string, the name of the account in equity
        to transfer previous balances from, in order to initialize account
        balances at the beginning of the period. This is typically called an
        opening balances account.
      account_conversions: A string, the name of the equity account to
        book currency conversions against.
    Returns:
      A new list of entries is returned, and the index that points to the first
      original transaction after the beginning date of the period. This index
      can be used to generate the opening balances report, which is a balance
      sheet fed with only the summarized entries.

    """
    # Insert conversion entries.
    entries = conversions(entries, account_conversions, conversion_currency, date)

    # Transfer income and expenses before the period to equity.
    entries, _ = clear(entries, date, account_types, account_earnings)

    # Summarize all the previous balances, after transferring the income and
    # expense balances, so all entries for those accounts before the begin date
    # should now disappear.
    entries, index = summarize(entries, date, account_opening)

    return entries, index


def close(
    entries: Directives,
    date: datetime.date,
    conversion_currency: Currency,
    account_conversions: Account,
) -> tuple[Directives, int]:
    """Truncate entries that occur after a particular date and ensure balance.

    This method essentially removes entries after a date. It truncates the
    future. To do so, it will

    1. Remove all entries which occur after 'date', if given.

    2. Insert conversion transactions at the end of the list of entries to
       ensure that the total balance of all postings sums up to empty.

    The result is a list of entries with a total balance of zero, with possibly
    non-zero balances for the income/expense accounts. To produce a final
    balance sheet, use transfer() to move the net income to the equity accounts.

    Args:
      entries: A list of directive tuples.
      date: A datetime.date instance, one day beyond the end of the period. This
        date can be optionally left to None in order to close at the end of the
        list of entries.
      conversion_currency: A string, the transfer currency to use for zero prices
        on the conversion entry.
      account_conversions: A string, the name of the equity account to
        book currency conversions against.
    Returns:
      A new list of entries is returned, and the index that points to one beyond
      the last original transaction that was provided. Further entries may have
      been inserted to normalize conversions and ensure the total balance sums
      to zero.
    """

    # Truncate the entries after the date, if a date has been provided.
    if date is not None:
        entries = truncate(entries, date)

    # Keep an index to the truncated list of entries (before conversions).
    index = len(entries)

    # Insert a conversions entry to ensure the total balance of all accounts is
    # flush zero.
    entries = conversions(entries, account_conversions, conversion_currency, date)

    return entries, index


# TODO(blais): This needs to be renamed in v3. Long name. Not obvious enough.
def clear(
    entries: Directives,
    date: datetime.date,
    account_types: AccountTypes,
    account_earnings: Account,
) -> tuple[Directives, int]:
    """Transfer income and expenses balances at the given date to the equity accounts.

    This method insert entries to zero out balances on income and expenses
    accounts by transferring them to an equity account.

    Args:
      entries: A list of directive tuples.
      date: A datetime.date instance, one day beyond the end of the period. This
        date can be optionally left to None in order to close at the end of the
        list of entries.
      account_types: An instance of AccountTypes.
      account_earnings: A string, the name of the account to transfer
        previous earnings from the income statement accounts to the balance
        sheet.
    Returns:
      A new list of entries is returned, and the index that points to one before
      the last original transaction before the transfers.
    """
    index = len(entries)

    # Transfer income and expenses before the period to equity.
    income_statement_account_pred = lambda account: is_income_statement_account(
        account, account_types
    )
    new_entries = transfer_balances(
        entries, date, income_statement_account_pred, account_earnings
    )

    return new_entries, index


def open_opt(
    entries: Directives, date: datetime.date, options_map: OptionsMap
) -> tuple[Directives, int]:
    """Convenience function to open() using an options map."""
    account_types = options.get_account_types(options_map)
    previous_accounts = options.get_previous_accounts(options_map)
    conversion_currency = options_map["conversion_currency"]
    return open(entries, date, account_types, conversion_currency, *previous_accounts)


def close_opt(
    entries: Directives, date: datetime.date, options_map: OptionsMap
) -> tuple[Directives, int]:
    """Convenience function to close() using an options map."""
    conversion_currency = options_map["conversion_currency"]
    current_accounts = options.get_current_accounts(options_map)
    return close(entries, date, conversion_currency, current_accounts[1])


def clear_opt(
    entries: Directives, date: datetime.date, options_map: OptionsMap
) -> tuple[Directives, int]:
    """Convenience function to clear() using an options map."""
    account_types = options.get_account_types(options_map)
    current_accounts = options.get_current_accounts(options_map)
    return clear(entries, date, account_types, current_accounts[0])


def clamp(
    entries: Directives,
    begin_date: datetime.date,
    end_date: datetime.date,
    account_types: AccountTypes,
    conversion_currency: Currency,
    account_earnings: Account,
    account_opening: Account,
    account_conversions: Account,
):
    """Filter entries to include only those during a specified time period.

    Firstly, this method will transfer all balances for the income and expense
    accounts occurring before the given period begin date to the
    'account_earnings' account (earnings before the period, or "retained
    earnings") and summarize all of the transactions before that date against
    the 'account_opening' account (usually "opening balances"). The resulting
    income and expense accounts should have no transactions (since their
    balances have been transferred out and summarization of zero balances should
    not add any transactions).

    Secondly, all the entries after the period end date will be truncated and a
    conversion entry will be added for the resulting transactions that reflect
    changes occurring between the beginning and end of the exercise period. The
    resulting balance of all account should be empty.

    Args:
      entries: A list of directive tuples.
      begin_date: A datetime.date instance, the beginning of the period.
      end_date: A datetime.date instance, one day beyond the end of the period.
      account_types: An instance of AccountTypes.
      conversion_currency: A string, the transfer currency to use for zero prices
        on the conversion entry.
      account_earnings: A string, the name of the account to transfer
        previous earnings from the income statement accounts to the balance
        sheet.
      account_opening: A string, the name of the account in equity
        to transfer previous balances from, in order to initialize account
        balances at the beginning of the period. This is typically called an
        opening balances account.
      account_conversions: A string, the name of the equity account to
        book currency conversions against.
    Returns:
      A new list of entries is returned, and the index that points to the first
      original transaction after the beginning date of the period. This index
      can be used to generate the opening balances report, which is a balance
      sheet fed with only the summarized entries.
    """
    # Transfer income and expenses before the period to equity.
    income_statement_account_pred = lambda account: is_income_statement_account(
        account, account_types
    )
    entries = transfer_balances(
        entries, begin_date, income_statement_account_pred, account_earnings
    )

    # Summarize all the previous balances, after transferring the income and
    # expense balances, so all entries for those accounts before the begin date
    # should now disappear.
    entries, index = summarize(entries, begin_date, account_opening)

    # Truncate the entries after this.
    entries = truncate(entries, end_date)

    # Insert conversion entries.
    entries = conversions(entries, account_conversions, conversion_currency, end_date)

    return entries, index


def clamp_opt(
    entries: Directives,
    begin_date: datetime.date,
    end_date: datetime.date,
    options_map: OptionsMap,
) -> tuple[Directives, int]:
    """Clamp by getting all the parameters from an options map.

    See clamp() for details.

    Args:
      entries: See clamp().
      begin_date: See clamp().
      end_date: See clamp().
      options_map: A parser's option_map.
    Returns:
      Same as clamp().
    """
    account_types = options.get_account_types(options_map)
    previous_earnings, previous_balances, _ = options.get_previous_accounts(options_map)
    _, current_conversions = options.get_current_accounts(options_map)

    conversion_currency = options_map["conversion_currency"]
    return clamp(
        entries,
        begin_date,
        end_date,
        account_types,
        conversion_currency,
        previous_earnings,
        previous_balances,
        current_conversions,
    )


def cap(
    entries: Directives,
    account_types: AccountTypes,
    conversion_currency: Currency,
    account_earnings: Account,
    account_conversions: Account,
) -> Directives:
    """Transfer net income to equity and insert a final conversion entry.

    This is used to move and nullify balances from the income and expense
    accounts to an equity account in order to draw up a balance sheet with a
    balance of precisely zero.

    Args:
      entries: A list of directives.
      account_types: An instance of AccountTypes.
      conversion_currency: A string, the transfer currency to use for zero prices
        on the conversion entry.
      account_earnings: A string, the name of the equity account to transfer
        final balances of the income and expense accounts to.
      account_conversions: A string, the name of the equity account to use as
        the source for currency conversions.
    Returns:
      A modified list of entries, with the income and expense accounts
      transferred.
    """

    # Transfer the balances of income and expense accounts as earnings / net
    # income.
    income_statement_account_pred = lambda account: is_income_statement_account(
        account, account_types
    )
    entries = transfer_balances(
        entries, None, income_statement_account_pred, account_earnings
    )

    # Insert final conversion entries.
    entries = conversions(entries, account_conversions, conversion_currency, None)

    return entries


def cap_opt(entries: Directives, options_map: OptionsMap) -> Directives:
    """Close by getting all the parameters from an options map.

    See cap() for details.

    Args:
      entries: See cap().
      options_map: A parser's option_map.
    Returns:
      Same as close().
    """
    account_types = options.get_account_types(options_map)
    current_accounts = options.get_current_accounts(options_map)
    conversion_currency = options_map["conversion_currency"]
    return cap(entries, account_types, conversion_currency, *current_accounts)


def transfer_balances(
    entries: Directives,
    date: datetime.date | None,
    account_pred: Callable[[Account], bool],
    transfer_account: Account,
) -> Directives:
    """Synthesize transactions to transfer balances from some accounts at a given date.

    For all accounts that match the 'account_pred' predicate, create new entries
    to transfer the balance at the given date from the account to the transfer
    account. This is used to transfer balances from income and expenses from a
    previous period to a "retained earnings" account. This is accomplished by
    creating new entries.

    Note that inserting transfers breaks any following balance checks that are
    in the transferred accounts. For this reason, all balance assertion entries
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
    transfer_balances = {
        account: balance for account, balance in balances.items() if account_pred(account)
    }

    # We need to insert the entries at the end of the previous day.
    if date:
        transfer_date = date - datetime.timedelta(days=1)
    else:
        transfer_date = entries[-1].date

    # Create transfer entries.
    transfer_entries = create_entries_from_balances(
        transfer_balances,
        transfer_date,
        transfer_account,
        False,
        data.new_metadata("<transfer_balances>", 0),
        flags.FLAG_TRANSFER,
        "Transfer balance for '{account}' (Transfer balance)",
    )

    # Remove balance assertions that occur after a transfer on an account that
    # has been transferred away; they would break.
    after_entries = [
        entry
        for entry in entries[index:]
        if not (isinstance(entry, data.Balance) and entry.account in transfer_balances)
    ]

    # Split the new entries in a new list.
    return entries[:index] + transfer_entries + after_entries


def summarize(
    entries: Directives, date: datetime.date, account_opening: Account
) -> tuple[Directives, int]:
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
      date: A datetime.date instance, the cutoff date before which to summarize.
      account_opening: A string, the name of the source account to book summarization
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
        balances,
        summarize_date,
        account_opening,
        True,
        data.new_metadata("<summarize>", 0),
        flags.FLAG_SUMMARIZE,
        "Opening balance for '{account}' (Summarization)",
    )

    # Insert the last price entry for each commodity from before the date.
    price_entries = prices.get_last_price_entries(entries, date)

    # Gather the list of active open entries at date.
    open_entries = get_open_entries(entries, date)

    # Compute entries before the date and preserve the entries after the date.
    before_entries = sorted(
        open_entries + price_entries + summarizing_entries, key=data.entry_sortkey
    )
    after_entries = entries[index:]

    # Return a new list of entries and the index that points after the entries
    # were inserted.
    return (before_entries + after_entries), len(before_entries)


def conversions(
    entries: Directives,
    conversion_account: Account,
    conversion_currency: Currency,
    date: datetime.date | None = None,
) -> Directives:
    """Insert a conversion entry at date 'date' at the given account.

    Args:
      entries: A list of entries.
      conversion_account: A string, the account to book against.
      conversion_currency: A string, the transfer currency to use for zero prices
        on the conversion entry.
      date: The date before which to insert the conversion entry. The new
        entry will be inserted as the last entry of the date just previous
        to this date.
    Returns:
      A modified list of entries.
    """
    # Compute the balance at the given date.
    conversion_balance = interpolate.compute_entries_balance(entries, date=date)

    # Early exit if there is nothing to do.
    conversion_cost_balance = conversion_balance.reduce(convert.get_cost)
    if conversion_cost_balance.is_empty():
        return entries

    # Calculate the index and the date for the new entry. We want to store it as
    # the last transaction of the day before.
    if date is not None:
        index = bisect_key.bisect_left_with_key(entries, date, key=lambda entry: entry.date)
        last_date = date - datetime.timedelta(days=1)
    else:
        index = len(entries)
        last_date = entries[-1].date

    meta = data.new_metadata("<conversions>", -1)
    narration = "Conversion for {}".format(conversion_balance)
    conversion_entry = Transaction(
        meta,
        last_date,
        flags.FLAG_CONVERSIONS,
        None,
        narration,
        data.EMPTY_SET,
        data.EMPTY_SET,
        [],
    )
    for position in conversion_cost_balance.get_positions():
        # Important note: Set the cost to zero here to maintain the balance
        # invariant. (This is the only single place we cheat on the balance rule
        # in the entire system and this is necessary; see documentation on
        # Conversions.)
        price = amount.Amount(ZERO, conversion_currency)
        neg_pos = -position
        conversion_entry.postings.append(
            data.Posting(conversion_account, neg_pos.units, neg_pos.cost, price, None, None)
        )

    # Make a copy of the list of entries and insert the new transaction into it.
    new_entries = list(entries)
    new_entries.insert(index, conversion_entry)

    return new_entries


def truncate(entries: Directives, date: datetime.date) -> Directives:
    """Filter out all the entries at and after date. Returns a new list of entries.

    Args:
      entries: A sorted list of directives.
      date: A datetime.date instance.
    Returns:
      A truncated list of directives.
    """
    index = bisect_key.bisect_left_with_key(entries, date, key=lambda entry: entry.date)
    return entries[:index]


def create_entries_from_balances(
    balances: dict[Account, inventory.Inventory],
    date: datetime.date,
    source_account: Account,
    direction: bool,
    meta: data.Meta,
    flag: data.Flag,
    narration_template: str,
) -> Directives:
    """ "Create a list of entries from a dict of balances.

    This method creates a list of new entries to transfer the amounts in the
    'balances' dict to/from another account specified in 'source_account'.

    The balancing posting is created with the equivalent at cost. In other
    words, if you attempt to balance 10 HOOL {500 USD}, this will synthesize a
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
      meta: A dict to use as metadata for the transactions.
      flag: A string, the flag to use for the transactions.
      narration_template: A format string for creating the narration. It is
        formatted with 'account' and 'date' replacement variables.
    Returns:
      A list of newly synthesizes Transaction entries.
    """
    new_entries: Directives = []
    for account, account_balance in sorted(balances.items()):
        # Don't create new entries where there is no balance.
        if account_balance.is_empty():
            continue

        narration = narration_template.format(account=account, date=date)

        if not direction:
            account_balance = -account_balance

        postings: list[data.Posting] = []
        new_entry = Transaction(
            meta, date, flag, None, narration, data.EMPTY_SET, data.EMPTY_SET, postings
        )

        for position in account_balance.get_positions():
            postings.append(
                data.Posting(account, position.units, position.cost, None, None, None)
            )
            cost = -convert.get_cost(position)
            postings.append(data.Posting(source_account, cost, None, None, None, None))

        new_entries.append(new_entry)

    return new_entries


# TODO(blais): Reconcile this with beancount.core.realization.realize() {2196104406ab}.
# TODO(blais): This should be generalized to any type of key, i.e., provide a key func.
def balance_by_account(
    entries: Directives,
    date: datetime.date | None = None,
    compress_unbooked: bool = False,
) -> tuple[dict[Account, inventory.Inventory], int]:
    """Sum up the balance per account for all entries strictly before 'date'.

    Args:
      entries: A list of directives.
      date: An optional datetime.date instance. If provided, stop accumulating
        on and after this date. This is useful for summarization before a
        specific date.
      compress_unbooked: For accounts that have a booking method of NONE,
        compress their positions into a single average position. This can be
        used when you export the full list of positions, because those accounts
        will have a myriad of small positions from fees at negative cost and
        what-not.
    Returns:
      A pair of a dict of account string to instance Inventory (the balance of
      this account before the given date), and the index in the list of entries
      where the date was encountered. If all entries are located before the
      cutoff date, an index one beyond the last entry is returned.

    """
    balances: dict[Account, inventory.Inventory] = collections.defaultdict(
        inventory.Inventory
    )
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
                account_balance.add_position(posting)
    else:
        index = len(entries)

    # If the account has "NONE" booking method, merge all its postings
    # together in order to obtain an accurate cost basis and balance of
    # units.
    #
    # (This is a complex issue.) If you accrued positions without having them
    # booked properly against existing cost bases, you have not properly accounted
    # for the profit/loss to other postings. This means that the resulting
    # profit/loss is merged in the cost basis of the positive and negative
    # postings.
    if compress_unbooked:
        oc_map = getters.get_account_open_close(entries)
        accounts_map = {account: dopen for account, (dopen, _) in oc_map.items()}

        for account, balance in balances.items():
            dopen = accounts_map.get(account, None)
            if dopen is not None and dopen.booking is data.Booking.NONE:
                average_balance = balance.average()
                balances[account] = inventory.Inventory(pos for pos in average_balance)

    return balances, index


def get_open_entries(entries: Directives, date: datetime.date) -> list[Open]:
    """Gather the list of active Open entries at date.

    This returns the list of Open entries that have not been closed at the given
    date, in the same order they were observed in the document.

    Args:
      entries: A list of directives.
      date: The date at which to look for an open entry. If not specified, will
        return the entries still open at the latest date.
    Returns:
      A list of Open directives.
    """
    open_entries: dict[Account, tuple[int, Open]] = {}
    for index, entry in enumerate(entries):
        if date is not None and entry.date >= date:
            break

        if isinstance(entry, Open):
            try:
                ex_index, ex_entry = open_entries[entry.account]
                if entry.date < ex_entry.date:
                    open_entries[entry.account] = (index, entry)
            except KeyError:
                open_entries[entry.account] = (index, entry)

        elif isinstance(entry, Close):
            # If there is no corresponding open, don't raise an error.
            open_entries.pop(entry.account, None)

    return [entry for (index, entry) in sorted(open_entries.values())]
