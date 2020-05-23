#!/usr/bin/env python3
"""Compute the returns of a portfolio.

A document exists to describe the problem in more detail.
http://furius.ca/beancount/doc/portfolio-returns

Calculating the returns is carried out by identifying the entries whose accounts
match a regular expression that defines accounts to consider for valuation, to
compute the returns over. This set of "related accounts" must also cover the
internal flows that occur within that account, that is, the income and expense
accounts that result in the activity of the portfolio itself.

We consider three sets of accounts:

  "VALUE ACCOUNTS" (also, "asset accounts"): Accounts whose balances are counted
    towards calculating the total value of the portfolio. These are asset
    accounts, including cash accounts.

  "INTERNAL ACCOUNTS": Accounts which are not included in the value of the
    account, but which are used to post internal activity of the account, not
    inflows or outflows. For example, the income account used to book the other
    side of a dividend receipt, which increases the value accounts, or an
    expense account used to book a commission paid or a fee, which decreases the
    value accounts. These are income and expenses accounts.

  "EXTERNAL ACCOUNTS": Accounts that are considered outside the group of value
    accounts or internal accounts. These are accounts from which funds will be
    deposited or withdrawn. These deposits or withdrawals must be excluded from
    the portfolio returns. Their presence is the reason computing portfolio
    returns isn't just a trivial exercise of comparing value at two different
    points in time.

Given this characterization, we can characterize transactions by which accounts
they have on their postings. Think of it as a Venn diagram with three circles
and all their possible intersections. We will use the following accounts in our
examples below:

  ;; Value accounts.
  2014-01-01 open Assets:Invest:Cash      USD
  2014-01-01 open Assets:Invest:BOOG      BOOG

  ;; Internal accounts.
  2014-01-01 open Income:Invest:PnL       USD
  2014-01-01 open Income:Invest:Dividends USD
  2014-01-01 open Expenses:Commissions    USD
  2014-01-01 open Expenses:Fees           USD

  ;; External accounts.
  2014-01-01 open Assets:Bank:Checking    USD
  2014-01-01 open Income:Salary           USD
  2014-01-01 open Expenses:Taxes          USD

Let us first consider transactions which have at least some value accounts:

  VALUE ONLY: All postings are on value accounts. An example would be some cash
    converted into units of a stock (with no expenses):

       2014-02-01 * "Buying some BOOG"
         Assets:Invest:Cash       -650.00 USD
         Assets:Invest:BOOG            10 BOOG {65 USD}

  VALUE + INTERNAL: Such a transaction would be one where, for example, there is
    some change that triggers a commission and/or a capital gain:

       2014-02-15 * "Selling half my position"
         Assets:Invest:BOOG            -5 BOOG {65 USD} @ 70 USD
         Assets:Invest:Cash        340.05 USD
         Expenses:Commissions        9.95 USD
         Income:Invest:PnL         -25.00 USD

     Or the receipt of a dividend:

       2014-02-20 * "Dividends from BOOG position"
         Assets:Invest:Cash         12.00 USD
         Income:Invest:Dividends   -12.00 USD

Both of these type of transactions represents transfers within asset accounts
and as such do not present any challenges or events in terms of calculating the
returns. Since internal flow accounts are meant to be considered as revenue or
costs internal to the portfolio, they can just be processed without having to
revalue the portfolio across them.


Other transactions need special treatment , however:

  VALUE + EXTERNAL: These would be transactions either with a deposit or a
    withdrawal from/to one of the value accounts:

       2014-01-10 * "Transferring money for investing"
         Assets:Bank:Checking      -500.00 USD
         Assets:Invest:Cash         500.00 USD

       2014-06-30 * "Taking some money out for car repairs"
         Assets:Invest:Cash       -400.00 USD
         Assets:Bank:Checking      400.00 USD

For these transactions, we need to compute the value of the asset accounts
before they get applied, book the returns for the previous leg, then apply the
transaction to its accounts and revalue the value accounts, and begin a new
piecewise returns segment.


Other transactions are a bit more problematic:

  VALUE + INTERNAL + EXTERNAL: Those transactions with external flows may
    sometimes involve posting amounts to one of the internal flow accounts:

       2014-04-01 * "Transferring money by wire"
         Assets:Bank:Checking      -500.00 USD
         Assets:Invest:Cash         480.00 USD
         Expenses:Fees               20.00 USD

The question here is whether the postings with internal flows should be
internalized or not, e.g., whether the 20.00 USD wire fee in the transaction
above should be considered a cost within the portfolio activity or not. We will
assume that they always are, and in order to keep our algorithm simple, we will
internalize the postings by splitting the transaction like this:

       2014-04-01 * "Transferring money by wire" ^internalized-27356
         Assets:Bank:Checking      -500.00 USD
         Equity:Internalized        500.00 USD

       2014-04-01 * "Transferring money by wire" ^internalized-27356
         Equity:Internalized       -500.00 USD
         Assets:Invest:Cash         480.00 USD
         Expenses:Fees               20.00 USD

Here we have created a "transfer account" called "Equity:Internalized" which is
automatically added to the set of value accounts. Now we have two transactions,
one with only VALUE + EXTERNAL accounts and one with VALUE + INTERNAL accounts.
The 20$ now effectively reduces the returns of the segment that includes the
second transaction, as it is included in the internal flows.


Then, we turn to other groups that don't include value accounts:

  EXTERNAL ONLY: These are other types of transactions on completely unrelated
    accounts. We simply ignore other transactions that do not affect our value
    nor internal flow accounts. Within our limited context above, here is such a
    transaction:

       2014-01-02 * "Salary Pay"
         Income:Salary            -3461.54 USD
         Expenses:Taxes            1176.92 USD
         Assets:Bank:Checking      2284.62 USD


  INTERNAL + EXTERNAL: Then we may have transactions that affect some internal
    accounts and some external accounts. The treatment for these is less clear.
    Some internal accounts are clearly tied to our investment portfolio, such as
    "Income:Invest:Dividends" and others are more general and can be used
    outside of the context of our investment portfolio, such as "Expenses:Fees"
    which could be used to book a monthly bank fee, for example, like this:

       2014-03-17 * "Monthly fees"
         Assets:Bank:Checking        -4.00 USD
         Expenses:Fees                4.00 USD

    Such a transaction should clearly not be considered as part of our portfolio
    in any way. The only relation is the common use of the "Expenses:Fees"
    account between transactions in the portfolio and transactions outside the
    portfolio. However, consider this transaction where an income account that
    is clearly associated with our portfolio is used to receive a dividend in an
    external account:

       2014-03-20 * "Dividend payment correction with fee"
         Income:Invest:Dividends     -9.00 USD
         Assets:Bank:Checking         9.00 USD

     This should clearly be included in the portfolio. The problem here is that
     there is no distinction between an internal flow account tied to this
     portfolio, such as "Income:Invest:Dividends" and one that is not and which
     is used widely outside of this context, such as "Expenses:Fees".

     In the context of this example, such transactions never occur. But...
     consider what would happen if we were attempting to compute the value of
     the portfolio excepting cash: the "Assets:Invest:Cash" account and a
     regular dividend contribution becomes one of these transactions:

       2014-03-20 * "Dividend payment"
         Income:Invest:Dividends     -9.00 USD ;; Internal
         Assets:Invest:Cash           9.00 USD ;; External

     So we will have to do something about those transactions: we provide the
     user with the ability to specify a list of accounts that will force
     internalization ("accounts_internalize"). When specified, transactions with
     no value accounts but with some postings matching one of these accounts
     will be internalized explicitly.


  INTERNAL ONLY: Finally, consider this contrived example transaction where a
    dividend happens to equal exactly some fee:

       2014-03-20 * "Dividend payment with fee"
         Income:Invest:Dividends     -9.00 USD
         Expenses:Fees                9.00 USD

     It is unclear whether that should be in the portfolio or not. We have no
     way to know. In either case, the transaction would have no impact on the
     value of the portfolio, so we choose to ignore these transactions safely.
     (Examples of these are rare.)


Notes:

 - If you use the "account_rounding" option, the rounding postings will
   naturally end up being external flows and that's an undesirable effect. If
   the option is turned on, we automatically ignore those postings. (Note that
   another valid solution would have been to insert this account in the list of
   internal accounts but that creates a more difficult set of resulting
   transactions, it's a bit confusing.).

"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import collections
import copy
import logging
import re
import sys

from dateutil.parser import parse as parse_datetime

from beancount.core.number import ZERO
from beancount import loader
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import inventory
from beancount.core import getters
from beancount.core import flags
from beancount.core import convert
from beancount.core import prices
from beancount.utils import misc_utils
from beancount.utils import date_utils


# A snapshot of a particular set of accounts at a point in time. The balances
# aren't converted to their prices, this is done in a separate step.
#
# Attributes:
#   date: A datetime.date instance, the day of the snapshot.
#   balance: An Inventory instance, the balance at the given date.
Snapshot = collections.namedtuple('Snapshot', 'date balance')

# A segment of a returns chain, with no external flows involved.
#
# Attributes:
#   begin: The snapshot at the beginning of the period.
#   end: The snapshot at the end of the period.
#   entries: A list of internal entries that occur within the segment, only
#     transactions.
#   external_entries: A list of the external entries that immediately follow
#     this segment.
Segment = collections.namedtuple('Segment', 'begin end entries external_entries')


def sum_balances_for_accounts(balance, entry, accounts):
    """Accumulate balance for assets postings accounts on entry.

    Args:
      balance: An instance of Inventory.
      entry: A directive (directives other than Transactions are ignored).
      accounts: A set of strings, the names of accounts whose postings to include.
    Returns:
      WARNING: This destructively modifies balance.
    """
    if isinstance(entry, data.Transaction):
        for posting in entry.postings:
            if posting.account in accounts:
                balance.add_position(posting)
    return balance


def segment_periods(entries, accounts_value, accounts_internal):
    """Segment entries in terms of piecewise periods of internal flow.

    This function iterates through the given entries and computes balances at
    the beginning and end of periods without external flow entries. You should be
    able to then compute the returns from this information.

    Args:
      entries: A list of directives. The list may contain directives other than
        than transactions as well as directives with no relation to the assets or
        internal flow accounts (the function simply ignores that which is not
        relevant).
      accounts_value: A set of the asset accounts in the related group.
      accounts_internal: A set of the internal flow accounts in the related group.
    Returns:
      A timeline, which is a list of Segment instances.
    Raises:
      ValueError: If the dates create an impossible situation, the beginning
        must come before the requested end, if specified.
    """
    accounts_related = accounts_value | accounts_internal
    is_external_flow_entry = lambda entry: (isinstance(entry, data.Transaction) and
                                            any(posting.account not in accounts_related
                                                for posting in entry.postings))

    # Create an iterator over the entries we care about.
    portfolio_entries = [entry
                         for entry in entries
                         if (isinstance(entry, data.Transaction) and
                             getters.get_entry_accounts(entry) & accounts_value)]
    iter_entries = iter(portfolio_entries)
    entry = next(iter_entries)

    # If a beginning cut-off has been specified, skip the entries before then
    # (and make sure to accumulate the initial balance correctly).
    balance = inventory.Inventory()
    period_begin = entry.date

    # Main loop over the entries.
    timeline = []
    done = False
    while True:
        balance_begin = copy.copy(balance)

        # Consume all internal flow entries, simply accumulating the total balance.
        segment_entries = []
        while True:
            period_end = entry.date
            if is_external_flow_entry(entry):
                break
            if entry:
                segment_entries.append(entry)
            sum_balances_for_accounts(balance, entry, accounts_value)
            try:
                entry = next(iter_entries)
            except StopIteration:
                done = True
                entry = None
                break
        balance_end = copy.copy(balance)

        ## FIXME: Bring this back in, this fails for now. Something about the
        ## initialization fails it.
        ## assert period_begin <= period_end, (period_begin, period_end)

        external_entries = []
        segment = Segment(Snapshot(period_begin, balance_begin),
                          Snapshot(period_end, balance_end),
                          segment_entries, external_entries)
        timeline.append(segment)

        # Absorb the balance of the external flow entries as long as they're on
        # the same date (a date change would imply a possible market value
        # change and we would want to create a segment in such cases, even if
        # the portfolio contents do not change).
        if entry is not None:
            date = entry.date
            while is_external_flow_entry(entry) and entry.date == date:
                external_entries.append(entry)
                sum_balances_for_accounts(balance, entry, accounts_value)
                try:
                    entry = next(iter_entries)
                except StopIteration:
                    done = True
                    break

        if done:
            break

        period_begin = period_end

    ## FIXME: Bring this back in, this fails for now.
    # assert all(period_begin <= period_end
    #            for period_begin, period_end, _, _ in periods), periods
    return timeline


def compute_period_returns(date_begin, date_end,
                           balance_begin, balance_end, price_map):
    """Compute the returns of the given begin/end balances.

    Args:
      date_begin: A datetime.date instance, the beginning date of the period.
      date_end: A datetime.date instance, the end date of the period.
      balance_begin: An instance of the Inventory at the beginning of the period.
      balance_end: An instance of the Inventory at the end of the period.
      price_map: An instance of PriceMap as computed by prices.build_price_map().
    Returns:
      A pair of:
        returns: A dict of currency -> floating-point return for the period. The
          union of all currencies for those is returned (this is done to be able
          to evaluate and report on returns in multiple currencies).
        (mktvalue_begin, mktvalue_end): Both instances of Inventory, the balance
          of the portfolio evaluated at the market value at the beginning and end
          of the period.
    """
    # Evaluate the boundary balances at market value.
    mktvalue_begin = balance_begin.reduce(convert.get_value, price_map, date_begin)
    mktvalue_end = balance_end.reduce(convert.get_value, price_map, date_end)

    # Compute the union of all currencies. At this point, ignore currencies
    # held-at-cost and issue a warning if some are found (if the price database
    # covers all the currencies held at cost, this should not occur).
    currencies = set()
    single_begin = {}
    single_end = {}
    for mktvalue, single in [(mktvalue_begin, single_begin),
                             (mktvalue_end, single_end)]:
        for pos in mktvalue.get_positions():
            if pos.cost:
                logging.error('Could not reduce position "%s" to its value', pos)
            else:
                currencies.add(pos.units.currency)
                assert pos.units.currency not in single
                single[pos.units.currency] = pos.units.number

    # Now for each of the currencies, compute the returns. Handle cases where
    # the currency is not present as a zero value for that currency.
    #
    # Note: In the future, we should instead require more information about the
    # desired currency for valuation and convert all contents to a single
    # currency above, so this is not needed except to handle really odd cases.
    returns = {}
    for currency in currencies:
        begin = single_begin.get(currency, ZERO)
        end = single_end.get(currency, ZERO)
        if begin == ZERO:
            returns[currency] = 1.0
        else:
            returns[currency] = float(end / begin)

    return returns, (mktvalue_begin, mktvalue_end)


def annualize_returns(returns, date_first, date_last):
    """Annualize the return rates computed from the given date range.

    Args:
      returns: A dict of floats, the calculated returns.
      date_first: A datetime.date instance, the beginning of the period.
      date_last: A datetime.date instance, the beginning of the period.
    Returns:
      A dict of float, the returns adjusted to equivalent annual rates.
    Raises:
      ValueError: If the data includes 0-day periods to annualize
        non-trivially.
    """
    num_days = (date_last - date_first).days
    if num_days == 0:
        for currency, return_ in returns.items():
            if return_ != 1:
                raise ValueError("Invalid period for return: {} days for {}".format(
                    num_days, return_))
        exponent = 1.
    else:
        exponent = 365. / num_days
    return {currency: return_ ** exponent
            for currency, return_ in returns.items()}


# The format of the links that are added to internalized transactions.
LINK_FORMAT = 'internalized-{:05d}'


def internalize(entries, transfer_account,
                accounts_value, accounts_internal,
                accounts_internalize=None):
    """Internalize flows that would be lost because booked against external
    flow accounts. This splits up entries that have accounts both in internal
    flows and external flows. A new set of entries are returned, along with a
    list of entries that were split and replaced by a pair of entries.

    Args:
      entries: A list of directives to process for internalization.
      transfer_account: A string, the name of an account to use for internalizing entries
        which need to be split between internal and external flows. A good default value
        would be an equity account, 'Equity:Internalized' or something like that.
      accounts_value: A set of account name strings, the names of the asset accounts
        included in valuing the portfolio.
      accounts_internal: A set of account name strings, the names of internal flow
        accounts (normally income and expenses) that aren't external flows.
      accounts_internalize: A set of account name strings to trigger explicit
        internalization of transactions with no value account. If a transaction
        is found that has only internal accounts and external accounts, the
        postings whose accounts are in this set of accounts will be
        internalized. This is a method that can be used to pull dividends into
        the portfolio when valuing portfolios without their cash component. See
        docstring and documentation for details. If specified, this set of
        accounts must be a subset of the internal flows accounts.
    Returns:
      A pair of the new list of internalized entries, including all the other entries, and
      a short list of just the original entries that were removed and replaced by pairs of
      entries.
    """
    # Verify that external flow entries only affect balance sheet accounts and
    # not income or expenses accounts (internal flows). We do this because we
    # want to ensure that all income and expenses are incurred against assets
    # that live within the assets group. An example of something we'd like to
    # avoid is an external flow paying for fees incurred within the account that
    # should diminish the returns of the related accounts. To fix this, we split
    # the entry into two entries, one without external flows against an transfer
    # account that we consider an assets account, and just the external flows
    # against this same transfer account.
    assert(isinstance(transfer_account, str)), (
        "Invalid transfer account: {}".format(transfer_account))

    if accounts_internalize and not (accounts_internalize <= accounts_internal):
        raise ValueError(
            "Internalization accounts is not a subset of internal flows accounts.")

    new_entries = []
    replaced_entries = []
    index = 1
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            new_entries.append(entry)
            continue

        # Break up postings into the three categories.
        postings_assets = []
        postings_internal = []
        postings_external = []
        postings_internalize = []
        postings_ignore = []
        for posting in entry.postings:
            if posting.account in accounts_value:
                postings_list = postings_assets
            elif posting.account in accounts_internal:
                postings_list = postings_internal
            else:
                postings_list = postings_external
            postings_list.append(posting)

            if accounts_internalize and posting.account in accounts_internalize:
                postings_internalize.append(posting)

        # Check if the entry is to be internalized and split it up in two
        # entries and replace the entry if that's the case.
        if (postings_internal and postings_external and
            (postings_assets or postings_internalize)):

            replaced_entries.append(entry)

            # We will attach a link to each of the split entries.
            link = LINK_FORMAT.format(index)
            index += 1

            # Calculate the weight of the balance to transfer.
            balance_transfer = inventory.Inventory()
            for posting in postings_external:
                balance_transfer.add_amount(convert.get_weight(posting))

            prototype_entry = entry._replace(flag=flags.FLAG_RETURNS,
                                             links=(entry.links or set()) | set([link]))

            # Create internal flows posting.
            postings_transfer_int = [
                data.Posting(transfer_account, pos.units, pos.cost, None, None, None)
                for pos in balance_transfer.get_positions()]
            new_entries.append(prototype_entry._replace(
                postings=(postings_assets + postings_internal + postings_transfer_int)))

            # Create external flows posting.
            postings_transfer_ext = [
                data.Posting(transfer_account, -pos.units, pos.cost, None, None, None)
                for pos in balance_transfer.get_positions()]
            new_entries.append(prototype_entry._replace(
                postings=(postings_transfer_ext + postings_external)))
        else:
            new_entries.append(entry)

    # The transfer account does not have an Open entry, insert one. (This is
    # just us being pedantic about Beancount requirements, this will not change
    # the returns, but if someone looks at internalized entries it produces a
    # correct set of entries you can load cleanly).
    open_close_map = getters.get_account_open_close(new_entries)
    if transfer_account not in open_close_map:
        open_transfer_entry = data.Open(data.new_metadata("beancount.projects.returns", 0),
                                        new_entries[0].date,
                                        transfer_account, None, None)
        new_entries.insert(0, open_transfer_entry)

    return new_entries, replaced_entries


def create_timeline(entries, options_map,
                    transfer_account,
                    accounts_value, accounts_internal, accounts_internalize=None):

    """Compute the returns of a portfolio of accounts.

    Args:
      entries: A list of directives that may affect the account.
      transfer_account: A string, the name of an account to use for internalizing entries
        which need to be split between internal and external flows. A good default value
        would be an equity account, 'Equity:Internalized' or something like that.
      accounts_value: A set of account name strings, the names of the asset accounts
        included in valuing the portfolio.
      accounts_internal: A set of account name strings, the names of internal flow
        accounts (normally income and expenses) that aren't external flows.
      accounts_internalize: A set of account name strings used to force internalization.
        See internalize() for details.
      price_map: An instance of PriceMap as computed by prices.build_price_map().
    Returns:
      A timeline, which is a list of Segment instances.
    """
    if not accounts_value:
        raise ValueError("Cannot calculate returns without assets accounts to value")
    if isinstance(accounts_value, list):
        accounts_value = set(accounts_value)
    if isinstance(accounts_internal, list):
        accounts_internal = set(accounts_internal)
    if accounts_internalize and isinstance(accounts_internalize, list):
        accounts_internalize = set(accounts_internalize)
    assert accounts_internalize is None or isinstance(accounts_internalize, set)

    # Add the rounding error account to the list of internal flow accounts in
    # order to avoid causing external flows on these tiny amounts.
    if options_map["account_rounding"]:
        entries = data.remove_account_postings(options_map["account_rounding"], entries)

    # Remove unrealized entries, if any are found. (Note that unrealized gains
    # only inserted at the end of the list of entries have no effect because
    # this module never creates a period after these. This may change in the future).
    entries = [entry
               for entry in entries
               if not (isinstance(entry, data.Transaction) and
                       entry.flag == flags.FLAG_UNREALIZED)]

    # Internalize entries with internal/external flows.
    entries, internalized_entries = internalize(
        entries, transfer_account,
        accounts_value, accounts_internal,
        accounts_internalize)
    accounts_value.add(transfer_account)

    # Segment the entries, splitting at entries with external flow and computing
    # the balances before and after. This returns all such periods with the
    # balances at their beginning and end.
    return segment_periods(entries, accounts_value, accounts_internal)


def value_inventory(price_map, date, inv):
    """Convert a position to its market value at a particular date.

    Args:
      price_map: A mapping of prices as per build_price_map().
      date: A datetime.date instance, the date at which to value the inventory.
      inv: The inventory to convert.
    Returns:
      A resulting inventory.
    """
    result = inventory.Inventory()
    for pos in inv:
        units = pos.units
        if pos.cost is None:
            converted_pos = units
        else:
            converted_pos = prices.convert_amount(price_map, pos.cost.currency, units, date)
            if converted_pos is None:
                logging.warning('Could not convert Position "{}" to {}'.format(
                    units, pos.cost.currency))
                converted_pos = units
        result.add_amount(converted_pos)
    return result


def dump_timeline_brief(timeline, price_map, file):
    """Dump a text rendering of the timeline to a given file output for debugging.

    Args:
      timeline: A list of Segment instances.
      price_map: A mapping of prices as per build_price_map().
      file: A file object to write to.
    """
    pr = lambda *args: print(*args, file=file)
    str_balances = [(
        value_inventory(price_map, segment.begin.date, segment.begin.balance).to_string(),
        value_inventory(price_map, segment.end.date, segment.end.balance).to_string()
    ) for segment in timeline]
    max_width = max(max(len(str_begin), len(str_end))
                    for str_begin, str_end in str_balances)
    fmt = "   {{}} -> {{}}  {{:>{w}}}  {{:>{w}}}".format(w=max_width)
    for segment, (str_begin, str_end) in zip(timeline, str_balances):
        pr(fmt.format(segment.begin.date, segment.end.date, str_begin, str_end))
    pr("")


def dump_timeline(timeline, price_map, file):
    """Dump a text rendering of the timeline to a given file output for debugging.
    This provides all, the detail. Useful just for debugging.

    Args:
      timeline: A list of Segment instances.
      price_map: A mapping of prices as per build_price_map().
      file: A file object to write to.
    """
    # FIXME: Convert this to make use of the price map.
    pr = lambda *args: print(*args, file=file)
    indfile = misc_utils.LineFileProxy(file.write, '   ', write_newlines=True)
    for segment in timeline:
        pr(",-----------------------------------------------------------")
        pr(" Begin:   {}".format(segment.begin.date))
        pr(" Balance: {}".format(segment.begin.balance.units()))
        printer.print_entries(segment.entries, file=indfile)
        pr("")
        pr(" Balance: {}".format(segment.end.balance.units()))
        pr(" End:     {}".format(segment.end.date))
        pr("`-----------------------------------------------------------")
        printer.print_entries(segment.external_entries, file=indfile)
        pr("")


def compute_returns(timeline, price_map, date_begin=None, date_end=None):
    """Compute the returns of a portfolio of accounts from the given timeline.

    Args:
      timeline: A list of Segment instances.
      price_map: An instance of PriceMap as computed by prices.build_price_map().
      date_begin: A datetime.date instance, the beginning date of the period to compute
        returns over.
      date_end: A datetime.date instance, the end date of the period to compute returns
        over.
    Returns:
      A triple of
        returns: A dict of currency -> float total returns.
        dates: A pair of (date_first, date_last) datetime.date instances.
    """
    periods = [(s.begin.date, s.end.date, s.begin.balance, s.end.balance)
               for s in timeline]

    # From the period balances, compute the returns.
    logging.info("Calculating period returns.")
    logging.info("")
    all_returns = []
    for (period_begin, period_end, balance_begin, balance_end) in periods:
        period_returns, mktvalues = compute_period_returns(period_begin, period_end,
                                                           balance_begin, balance_end,
                                                           price_map)
        mktvalue_begin, mktvalue_end = mktvalues
        all_returns.append(period_returns)

        try:
            annual_returns = (annualize_returns(period_returns, period_begin, period_end)
                              if period_end != period_begin
                              else {})
        except OverflowError:
            annual_returns = 'OVERFLOW'

        logging.info("From %s to %s", period_begin, period_end)
        logging.info("  Begin %s => %s",
                     balance_begin.reduce(convert.get_units), mktvalue_begin)
        logging.info("  End   %s => %s",
                     balance_end.reduce(convert.get_units), mktvalue_end)
        logging.info("  Returns     %s", period_returns)
        logging.info("  Annualized  %s", annual_returns)
        logging.info("")

    # Compute the piecewise returns. Note that we have to be careful to handle
    # all available currencies.
    currencies = set(currency
                     for returns in all_returns
                     for currency in returns.keys())
    total_returns = {}
    for currency in currencies:
        total_return = 1.
        for returns in all_returns:
            total_return *= returns.get(currency, 1.)
        total_returns[currency] = total_return

    date_first = periods[0][0]
    date_last = periods[-1][1]
    return total_returns, (date_first, date_last)


def compute_timeline_and_returns(entries, options_map,
                                 transfer_account,
                                 accounts_value, accounts_internal, accounts_internalize=None,
                                 date_begin=None, date_end=None):
    """Compute a timeline and the returns of a portfolio of accounts.

    Args:
      entries: A list of directives that may affect the account.
      transfer_account: A string, the name of an account to use for internalizing entries
        which need to be split between internal and external flows. A good default value
        would be an equity account, 'Equity:Internalized' or something like that.
      accounts_value: A set of account name strings, the names of the asset accounts
        included in valuing the portfolio.
      accounts_internal: A set of account name strings, the names of internal flow
        accounts (normally income and expenses) that aren't external flows.
      accounts_internalize: A set of account name strings used to force internalization.
        See internalize() for details.
      price_map: An instance of PriceMap as computed by prices.build_price_map().
      date_begin: A datetime.date instance, the beginning date of the period to compute
        returns over.
      date_end: A datetime.date instance, the end date of the period to compute returns
        over.
    Returns:
      A triple of
        returns: A dict of currency -> float total returns.
        dates: A pair of (date_first, date_last) datetime.date instances.
    """
    timeline = create_timeline(entries, options_map,
                               transfer_account,
                               accounts_value, accounts_internal, accounts_internalize)

    price_map = prices.build_price_map(entries)

    return compute_returns(timeline, price_map, date_begin, date_end)


# A simple structure to hold the sets of accounts used to construct a timeline
# for a particular asset we want to compute the returns for.
#
# Attributes:
#   value: A set of value account strings.
#   internal: A set of internal account strings.
#   external: A set of external account strings.
#   internalize: A set of internalized account strings.
ReturnAccounts = collections.namedtuple('ReturnAccounts',
                                        'value internal external internalize')


def regexps_to_accounts(entries,
                        regexp_value, regexp_internal, regexp_internalize=None):
    """Extract account name from a list of entries and value & internal account regexps.

    This function takes two regular expressions: one for value accounts and one
    for internal accounts, and given the list of entries, extracts the explicit
    configuration required to create segments for those accounts.

    Args:
      entries: A list of directives.
      regexp_value: A regular expression string that matches names of asset accounts to
        value for the portfolio.
      regexp_internal: A regular expression string that matches names of accounts considered
        internal flows to the portfolio (typically income and expenses accounts).
      regexp_internalize: A regular expression string that matches names of accounts
        to force internalization of. See internalize() for details.
    Returns:
      A list of all entries with an account matching the given pattern, and a
      triplet of account lists:
        accounts_value: A set of the asset accounts in the related group.
        accounts_internal: A set of the internal flow accounts in the related group.
        accounts_external: A set of the external flow accounts.
        accounts_internalize: A set of the explicitly internalized accounts, or None,
          if left unspecified.
    """
    accounts_value = set()
    accounts_internal = set()
    accounts_external = set()
    accounts_internalize = set()

    # Precompute regexps for performance.
    match_value = re.compile(regexp_value).match
    match_internal = re.compile(regexp_internal).match
    match_internalize = re.compile(regexp_internalize).match if regexp_internalize else None

    # Run through all the transactions.
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue

        # If we have a value account or an explicitly internalized account.
        if any((match_value(posting.account) or
                (match_internalize and match_internalize(posting.account)))
               for posting in entry.postings):

            # Categorize accounts of a matching transaction.
            for posting in entry.postings:
                if match_value(posting.account):
                    accounts_value.add(posting.account)
                elif match_internal(posting.account):
                    accounts_internal.add(posting.account)
                else:
                    accounts_external.add(posting.account)

                if match_internalize and match_internalize(posting.account):
                    accounts_internalize.add(posting.account)

    return ReturnAccounts(accounts_value,
                          accounts_internal,
                          accounts_external,
                          accounts_internalize or None)


def dump_return_accounts(racc, file):
    """Render the return accounts.

    Args:
      racc: A ReturnAccounts instance.
      file: A file object to write to.
    """
    pr = lambda *args: print(*args, file=file)
    pr('Asset accounts:')
    for account in sorted(racc.value):
        pr('  {}'.format(account))
    pr('')

    pr('Internal flows:')
    for account in sorted(racc.internal):
        pr('  {}'.format(account))
    pr('')

    pr('External flows:')
    for account in sorted(racc.external):
        pr('  {}'.format(account))
    pr('')

    if racc.internalize:
        pr('Explicitly internalized accounts:')
        for account in sorted(racc.internalize):
            pr('  {}'.format(account))
        pr('')


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument('filename', help='Ledger filename')

    parser.add_argument('regexp_value', action='store',
                        help=("A regular expression string that matches names of asset "
                              "accounts to value for the portfolio."))

    parser.add_argument('regexp_internal', action='store',
                        help=("A regular expression string that matches names of accounts "
                              "considered internal flows to the portfolio (typically "
                              "income and expenses accounts)."))

    parser.add_argument('--regexp_internalize', '--internalize_regexp', action='store',
                        help=("A regular expression string that matches names of internal "
                              "flow accounts to trigger an internalization."))

    parser.add_argument('--transfer-account', action='store',
                        default='Equity:Internalized',
                        help="Default name for subaccount to use for transfer account.")

    parser.add_argument('-v', '--verbose', action='store_true',
                        help="Output detailed processing information. Useful for debugging")

    parser.add_argument('--date-begin', '--begin-date',
                        action='store', type=date_utils.parse_date_liberally,
                        default=None,
                        help=("Beginning date of the period to compute returns over "
                              "(default is the first related directive)"))

    parser.add_argument('--date-end', '--end-date',
                        action='store', type=date_utils.parse_date_liberally,
                        default=None,
                        help=("End date of the period to compute returns over "
                              "(default is the last related directive)"))

    args = parser.parse_args()
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO,
                        format='%(levelname)-8s: %(message)s')

    # Load the input file and build the price database.
    entries, errors, options_map = loader.load_file(args.filename, log_errors=logging.error)

    # Extract the account names using the regular expressions.
    racc = regexps_to_accounts(
        entries, args.regexp_value, args.regexp_internal, args.regexp_internalize)

    # Compute the returns using the explicit configuration.
    returns, (date_first, date_last) = compute_timeline_and_returns(
        entries, options_map,
        args.transfer_account,
        racc.value, racc.internal, racc.internalize,
        args.date_begin, args.date_end)

    # Annualize the returns.
    annual_returns = annualize_returns(returns, date_first, date_last)

    print('Total returns from {} to {}:'.format(date_first, date_last))
    for currency, return_ in sorted(returns.items()):
        print('  {}: {:.3%}'.format(currency, return_ - 1))

    print('Averaged annual returns from {} to {}:'.format(date_first, date_last))
    for currency, return_ in sorted(annual_returns.items()):
        print('  {}: {:.3%}'.format(currency, return_ - 1))


if __name__ == '__main__':
    main()
