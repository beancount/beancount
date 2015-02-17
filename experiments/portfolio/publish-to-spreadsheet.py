#!/usr/bin/env python
"""Publish a list of Beancount holdings to a Google Docs Spreadsheet.

This is intended to publish the list of holdings to a spreadsheet that allows on
to make planning decisions for making changes to a portfolio.

Note: This script has to run under Python 2.x, but Beancount is only implemented
using Python 3.x, so we export the list of Beancount holdings via subprocess and
import them in this script.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import csv
import collections
import datetime
import logging
import functools
import urllib
import os
import shutil
import tempfile
import re
import subprocess
import StringIO
from os import path
from decimal import Decimal as D

# Locally.
import gdrive

import gdata


def memoize_to_file(function):
    def wrapped(filename, *args, **kwds):
        cache_filename = path.join(
            path.dirname(filename),
            '.{}.cache.{}'.format(function.__name__, path.basename(filename)))
        if path.exists(cache_filename):
            contents = open(cache_filename).read()
        else:
            contents = function(filename, *args, **kwds)
            with open(cache_filename, 'w') as file:
                file.write(contents)
        return contents
    return wrapped


def run_beancount_report(command):
    """Run a Beancount report command.

    Args:
      command: A list of strings, the command to run.
    Returns:
      A string, the rendered CSV file contents.
    """
    pipe = subprocess.Popen(command,
                            shell=False,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    output, stderr = pipe.communicate()
    assert not stderr, stderr
    return output

@memoize_to_file
def get_holdings(filename):
    return run_beancount_report(['bean-report', '--format=csv', filename, 'holdings'])

@memoize_to_file
def get_tickers(filename):
    return run_beancount_report(['bean-report', '--format=csv', filename, 'tickers'])


def convert_holdings_to_positions(holdings_csv, tickers_csv, account_re=None):
    """Parse the CSV file and return a list of holdings to be put into the spreadsheet.

    Args:
      holdings_csv: A string, the CSV file of holdings.
      tickers_csv: A string, the CSV file of currency to ticker symbol mappings.
      account_re: A string, the regular expression for accounts to limit to.
    Returns:
      A list of SpreadsheetHolding tuples.
    """
    # Read in the commodity to tickers map.
    reader = csv.reader(StringIO.StringIO(tickers_csv))
    header = next(reader)
    currency_map = {
        (currency, cost_currency): (symbol, name)
        for currency, cost_currency, symbol, name in reader}

    # Read in the list of holdings and aggregate them by commodity.
    reader = csv.reader(StringIO.StringIO(holdings_csv))
    header = next(reader)
    position_map = collections.defaultdict(D)
    for account, units, currency, cost_currency, _, _, _, _ in reader:
        if account_re and not re.match(account_re, account):
            continue
        position_map[(currency, cost_currency)] += D(units.replace(',', ''))

    # Return the list of commodities to publish.
    for (currency, cost_currency), units in position_map.items():
        ticker = currency_map.get((currency, cost_currency), '')
        yield (currency, cost_currency, ticker, units)






def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    parser = gdrive.get_argparser(description=__doc__.strip())
    parser.add_argument('filename', action='store', help="Beancount input file")
    parser.add_argument('-o', '--output', action='store',
                        default=datetime.date.today().strftime('beancount.%Y-%m-%d.pdf'),
                        help="Where to write out the collated PDF file")
    args = parser.parse_args()
    account_re = 'Assets:US:Ameritrade:Main'

    # Produce the beancount reports.
    holdings_csv = get_holdings(args.filename)
    tickers_csv = get_tickers(args.filename)

    # Convert to a list of positions.
    for currency, cost_currency, ticker, units in convert_holdings_to_positions(
            holdings_csv, tickers_csv, account_re):
        print(currency, cost_currency, ticker, units)

    # Connect, with authentication.
    # Check https://developers.google.com/drive/scopes for all available scopes.
    #DRIVE_SCOPE = 'https://www.googleapis.com/auth/drive'
    SCOPE = 'https://spreadsheets.google.com/feeds'
    http = gdrive.get_authenticated_http(SCOPE, args)

    # Access the drive API.
    # sheets = gdrive.discovery.build('feeds', 'v2', http=http)




if __name__ == '__main__':
    main()
