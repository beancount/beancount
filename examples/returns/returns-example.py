#!/usr/bin/env python3
"""Example script calculating returns on the example file.

This script demonstrates calculating the returns of the investment accounts
defined in the auto-generated tutorial example file. You can use this file to
build your own returns calculation script, for the specific time periods you
need and the specific accounts you name.

Note that you can also invoke beancount.projects.returns directly like this:

  python3 -m beancount.projects.returns <filename> <regexp>

"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import logging
from os import path

from beancount.projects import returns
from beancount.core import getters
from beancount.core import data
from beancount.parser import printer
from beancount import loader


def main():
    # Uncomment this if you want to view he detailed log of each calculation.
    #logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    # Load the example file.
    examples_dir = path.dirname(path.dirname(__file__))
    filename = path.join(examples_dir, 'tutorial', 'example.beancount')
    entries, _, options_map = loader.load_file(filename, log_errors=print)

    # Figure out the number of years in action.
    year_min, year_max = [date.year
                          for date in getters.get_min_max_dates(entries, data.Transaction)]

    # Figure out dates for the last month and last quarter intervals.
    date_last = entries[-1].date
    date_one_month = date_last - datetime.timedelta(days=30)
    date_three_months = date_last - datetime.timedelta(days=90)

    # Create a list of periods to compute the returns over.
    periods = [
        (str(year), datetime.date(year, 1, 1), min(date_last, datetime.date(year+1, 1, 1)))
        for year in range(year_min, year_max+1)
        ] + [
            ('Last month', date_one_month, date_last),
            ('Three month', date_three_months, date_last),
        ]

    # Loop over accounts with investments in them. This is defined by the user.
    FORMAT = "  {:<16}  {:10} -> {:10}: {:>12.2%} {:>12.2%}"
    for account_name, regexp in [
        ('ETrade', '(.*:US:ETrade|Expenses:Financial:Commissions)'),
        ('Vanguard', '(.*:US:Vanguard|Expenses:Financial:Commissions)')]:

        # Print a header.
        print("Returns for {} account".format(account_name))
        print(FORMAT.replace('.2%', '').format('Period', 'Begin', 'End', 'Total', 'Annualized'))

        # Loop over each period.
        for period_name, date_begin, date_end in periods:
            # Compute the returns.
            total_returns, dates, int_entries = returns.compute_returns_with_regexp(
                entries, options_map, 'Assets:Internalized', regexp, date_begin, date_end)

            # Annualize the returns for the period.
            annual_returns = returns.annualize_returns(total_returns, date_begin, date_end)

            print(FORMAT.format(period_name, str(date_begin), str(date_end),
                                total_returns['USD'] - 1, annual_returns['USD'] - 1))
        print()


if __name__ == '__main__':
    main()
