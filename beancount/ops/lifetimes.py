"""Given a Beancount ledger, compute time intervals where we hold each commodity.

This script computes, for each commodity, which time intervals it is required at.
This can then be used to identify a list of dates at which we need to fetch prices
in order to properly fill the price database.
"""

__copyright__ = "Copyright (C) 2014-2017, 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime
import itertools

from beancount.core import data
from beancount.core import inventory

ONEDAY = datetime.timedelta(days=1)


def get_commodity_lifetimes(entries):
    """Given a list of directives, figure out the life of each commodity.

    Args:
      entries: A list of directives.
    Returns:
      A dict of (currency, cost-currency) commodity strings to lists of (start,
      end) datetime.date pairs. The dates are inclusive of the day the commodity
      was seen; the end/last dates are one day _after_ the last date seen.
    """
    lifetimes = collections.defaultdict(list)

    # The current set of active commodities.
    commodities = set()

    # The current balances across all accounts.
    balances = collections.defaultdict(inventory.Inventory)

    for entry in entries:
        # Process only transaction entries.
        if not isinstance(entry, data.Transaction):
            continue

        # Update the balance of affected accounts and check locally whether that
        # triggered a change in the set of commodities.
        commodities_changed = False
        for posting in entry.postings:
            balance = balances[posting.account]
            commodities_before = balance.currency_pairs()
            balance.add_position(posting)
            commodities_after = balance.currency_pairs()
            if commodities_after != commodities_before:
                commodities_changed = True

        # If there was a change in one of the affected account's list of
        # commodities, recompute the total set globally. This should not
        # occur very frequently.
        if commodities_changed:
            new_commodities = set(
                itertools.chain(*(inv.currency_pairs() for inv in balances.values()))
            )
            if new_commodities != commodities:
                # The new global set of commodities has changed; update our
                # the dictionary of intervals.
                for currency in new_commodities - commodities:
                    lifetimes[currency].append((entry.date, None))

                for currency in commodities - new_commodities:
                    lifetime = lifetimes[currency]
                    begin_date, end_date = lifetime.pop(-1)
                    assert end_date is None
                    lifetime.append((begin_date, entry.date + ONEDAY))

                # Update our current set.
                commodities = new_commodities

    return lifetimes


def compress_intervals_days(intervals, num_days):
    """Compress a list of date pairs to ignore short stretches of unused days.

    Args:
      intervals: A list of pairs of datetime.date instances.
      num_days: An integer, the number of unused days to require for intervals
        to be distinct, to allow a gap.
    Returns:
      A new dict of lifetimes map where some intervals may have been joined.
    """
    ignore_interval = datetime.timedelta(days=num_days)
    new_intervals = []
    iter_intervals = iter(intervals)
    last_begin, last_end = next(iter_intervals)
    for date_begin, date_end in iter_intervals:
        if date_begin - last_end < ignore_interval:
            # Compress.
            last_end = date_end
            continue
        new_intervals.append((last_begin, last_end))
        last_begin, last_end = date_begin, date_end
    new_intervals.append((last_begin, last_end))
    return new_intervals


def trim_intervals(intervals, trim_start=None, trim_end=None):
    """Trim a list of date pairs to be within a start and end date.
    Useful in update-style price fetching.

    Args:
      intervals: A list of pairs of datetime.date instances
      trim_start: An inclusive starting date.
      trim_end: An exclusive starting date.
    Returns:
      A list of new intervals (pairs of (date, date)).
    """
    new_intervals = []
    iter_intervals = iter(intervals)
    if trim_start is not None and trim_end is not None and trim_end < trim_start:
        raise ValueError("Trim end date is before start date")

    for date_begin, date_end in iter_intervals:
        if trim_start is not None and trim_start > date_begin:
            date_begin = trim_start
        if trim_end is not None:
            if date_end is None or trim_end < date_end:
                date_end = trim_end

        if date_end is None or date_begin <= date_end:
            new_intervals.append((date_begin, date_end))
    return new_intervals


def compress_lifetimes_days(lifetimes_map, num_days):
    """Compress a lifetimes map to ignore short stretches of unused days.

    Args:
      lifetimes_map: A dict of currency intervals as returned by get_commodity_lifetimes.
      num_days: An integer, the number of unused days to ignore.
    Returns:
      A new dict of lifetimes map where some intervals may have been joined.
    """
    return {
        currency_pair: compress_intervals_days(intervals, num_days)
        for currency_pair, intervals in lifetimes_map.items()
    }


ONE_WEEK = datetime.timedelta(days=7)


def required_weekly_prices(lifetimes_map, date_last):
    """Enumerate all the commodities and Fridays where the price is required.

    Given a map of lifetimes for a set of commodities, enumerate all the Fridays
    for each commodity where it is active. This can be used to connect to a
    historical price fetcher routine to fill in missing price entries from an
    existing ledger.

    Args:
      lifetimes_map: A dict of currency to active intervals as returned by
        get_commodity_lifetimes().
      date_last: A datetime.date instance, the last date which we're interested in.
    Returns:
      Tuples of (date, currency, cost-currency).
    """
    results = []
    for currency_pair, intervals in lifetimes_map.items():
        if currency_pair[1] is None:
            continue
        for date_begin, date_end in intervals:
            # Find first Friday before the minimum date.
            diff_days = 4 - date_begin.weekday()
            if diff_days >= 1:
                diff_days -= 7
            date = date_begin + datetime.timedelta(days=diff_days)

            # Iterate over all Fridays.
            if date_end is None:
                date_end = date_last
            while date < date_end:
                results.append((date, currency_pair[0], currency_pair[1]))
                date += ONE_WEEK
    return sorted(results)


def required_daily_prices(lifetimes_map, date_last, weekdays_only=False):
    """Enumerate all the commodities and days where the price is required.

    Given a map of lifetimes for a set of commodities, enumerate all the days
    for each commodity where it is active. This can be used to connect to a
    historical price fetcher routine to fill in missing price entries from an
    existing ledger.

    Args:
      lifetimes_map: A dict of currency to active intervals as returned by
        get_commodity_lifetimes().
      date_last: A datetime.date instance, the last date which we're interested in.
      weekdays_only: Option to limit fetching to weekdays only.
    Returns:
      Tuples of (date, currency, cost-currency).
    """
    results = []
    for currency_pair, intervals in lifetimes_map.items():
        if currency_pair[1] is None:
            continue
        for date_begin, date_end in intervals:
            # Find first Weekday starting on or before minimum date.
            date = date_begin
            if weekdays_only:
                diff_days = 4 - date_begin.weekday()
                if diff_days < 0:
                    date += datetime.timedelta(days=diff_days)

            # Iterate over all weekdays.
            if date_end is None:
                date_end = date_last
            while date < date_end:
                results.append((date, currency_pair[0], currency_pair[1]))
                if weekdays_only and date.weekday() == 4:
                    date += 3 * ONEDAY
                else:
                    date += ONEDAY

    return sorted(results)
