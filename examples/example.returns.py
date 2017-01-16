#!/usr/bin/env python3
"""Example script calculating returns on the example file.

This script demonstrates calculating the returns of the investment accounts
defined in the auto-generated tutorial example file. You can use this file to
build your own returns calculation script, for the specific time periods you
need and the specific accounts you name.

Note that you can also invoke beancount.projects.returns directly like this:

  python3 -m beancount.projects.returns <filename> <regexp>

"""
__copyright__ = "Copyright (C) 2014-2015  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import datetime
import logging
from os import path

from beancount.projects import returns
from beancount.core import getters
from beancount.core import data
from beancount.parser import printer
from beancount import loader


def main():
    # Parse and validate options.
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('--verbose', '-v', action='store_true', help='Verbose mode')
    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')

    # Load the example file.
    examples_dir = path.dirname(path.abspath(__file__))
    filename = path.join(examples_dir, 'example.beancount')
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
    for account_name, assets_regexp, intflows_regexp, internalize_regexp in [
        ('ETrade', 'Assets:US:ETrade',
         '(Income:US:ETrade|Expenses:Financial)', None),
        ('ETrade (no cash)', 'Assets:US:ETrade:[A-Z]+$',
         '(Income:US:ETrade|Expenses:Financial)', 'Income:US:ETrade:Dividends'),
        ('Vanguard', 'Assets:US:Vanguard',
         '(Income:US:ETrade|Expenses:Financial)', None)
    ]:

        # Print a header.
        print()
        print("Returns for {} account".format(account_name))
        print(FORMAT.replace('.2%', '').format('Period', 'Begin', 'End', 'Total', 'Annualized'))

        # Loop over each period.
        for period_name, date_begin, date_end in periods:

            # Extract the account names using the regular expressions.
            (accounts_value,
             accounts_internal,
             accounts_external,
             accounts_internalize) = returns.regexps_to_accounts(
                 entries, assets_regexp, intflows_regexp, None)

            # Compute the returns.
            total_returns, dates = returns.compute_timeline_and_returns(
                entries, options_map, 'Assets:Internalized',
                accounts_value, accounts_internal, accounts_internalize,
                date_begin, date_end)

            # Annualize the returns for the period.
            annual_returns = returns.annualize_returns(total_returns, date_begin, date_end)

            print(FORMAT.format(period_name, str(date_begin), str(date_end),
                                total_returns['USD'] - 1, annual_returns['USD'] - 1))


if __name__ == '__main__':
    main()
