#!/usr/bin/env python3
"""Compute salary averages.

This is useful if your compensation is variable or experiences jumps due to
uneven stock grants and such. It is intended to provide a better answer to the
question "What is my compensation package?". The question needs to be framed
better, e.g., "What is my *average* annual compensation over my entire tenure at
the company?" or "What would be annual compensation like if I were to
extrapolate the last 3 or 6 months?".
"""
__copyright__ = "Copyright (C) 2015  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import datetime

from matplotlib import pyplot

import dateutil
from dateutil import rrule

from beancount.core.number import Decimal
from beancount import loader
from beancount.core import inventory
from beancount.core import data
from beancount.core import account


def rolling_average(series, num_periods=None):
    """Compute the rolling average of a series of (date, number) points.

    Args:
      series: A list of (date, number) pairs, where 'date' is a datetime.date
        instance, and 'number' is a Decimal object.
      num_periods: An integer, the number of points to average over. If None,
        average over the entire series.
    Returns:
      A new series of (date, average) points.
    """
    if num_periods is None:
        num_periods = len(series) + 1
    average = []
    for index, (date, _) in enumerate(series):
        start_date, start_number = series[max(0, index-num_periods)]
        end_date, end_number = series[index]
        days = (end_date - start_date).days
        if days == 0:
            continue
        number = end_number - start_number
        scaling = Decimal(365 / days)
        point = (date, (number * scaling).quantize(Decimal('0.01')))
        average.append(point)
    return average


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename', help='Beancount filename')
    parser.add_argument('account', help='Root account to consider')

    parser.add_argument('-c', '--currency', action='store', default='USD',
                        help="The currency to pull out of the inventory")

    args = parser.parse_args()

    # Load the Beancount input file.
    entries, _, options_map = loader.load_file(args.filename)

    # Compute monthly time intervals.
    start_date = datetime.date(2013, 1, 28)
    dateiter = iter(rrule.rrule(rrule.MONTHLY,
                                dtstart=datetime.datetime(2013,1,1),
                                until=datetime.datetime.now()))

    # Compute cumulative totals accumulated at those dates.
    curve = [(datetime.date(2013, 1, 28), Decimal())]
    date = next(dateiter).date()
    balance = inventory.Inventory()
    is_account = account.parent_matcher(args.account)
    for entry in entries:
        if entry.date >= date:
            # At the boundary, save the date and total number.
            try:
                total = -balance.get_currency_units(args.currency).number
                curve.append((date, total))
                date = next(dateiter).date()
            except StopIteration:
                break

        # Sum up the amounts from those accounts.
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                if is_account(posting.account):
                    balance.add_position(posting.position)

    # Compute multiple averages over fixed windows of a number of months and
    # plot them.
    months = [None, 12, 6, 3]
    for num in months:
        series = rolling_average(curve, num)
        pyplot.plot([date for date, total in series],
                    [total for date, total in series], label=str(num))
        print('{:10}: {:10,.2f}'.format(num if num is not None else 0,
                                        series[-1][1]))

    # Show that joint plot.
    pyplot.legend()
    pyplot.tight_layout()
    pyplot.show()


if __name__ == '__main__':
    main()
