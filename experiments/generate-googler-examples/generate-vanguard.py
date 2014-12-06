#!/usr/bin/env python3
"""
Generate fake investment events for Vanguard account.
"""
import uuid
import argparse
import logging
import textwrap
import decimal
import datetime
import io
import sys
import random
import re
import collections
import inspect
from os import path

from dateutil.parser import parse as parse_datetime
from beancount.core.amount import D
from beancount.core.amount import ZERO
from beancount.core import data
from beancount.core import realization
from beancount.core import inventory
from beancount.parser import parser
from beancount.parser import printer
from beancount import loader

from ledgerhub.prices import google_finance
from ledgerhub.prices import yahoo_finance


def parse_many(string, level=0):
    frame = inspect.stack()[level+1]
    varkwds = frame[0].f_locals
    entries, errors, __ = parser.parse_string(textwrap.dedent(string.format(**varkwds)))
    assert not errors
    return entries

def parse_one(string):
    entries = parse_many(string, level=1)
    assert len(entries) == 1
    return entries[0]


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    argparser = argparse.ArgumentParser(__doc__.strip())
    argparser.add_argument('-n', '--no-fetch-prices', action='store_true')
    args = argparser.parse_args()

    entries, _, __ = loader.load_file(path.join(path.dirname(__file__), 'google.beancount'))
    real_root = realization.realize(entries)
    real_account = realization.get(real_root, 'Assets:US:Vanguard:PreTax401k:Cash')

    currency, cost_currency = 'VFORX', 'USD'
    fetcher = yahoo_finance.YahooFinancePriceFetcher(currency, currency, cost_currency)

    if args.no_fetch_prices:
        fetcher.get_historical_price = lambda date: (D('20'), None)

    Q = D('0.01')
    QU = D('0.0001')

    # Generate investment and match entries.
    new_entries = []
    for posting in real_account.postings:
        if not isinstance(posting, data.Posting):
            continue

        price, _ = fetcher.get_historical_price(posting.entry.date)

        # Create an entry investing the deposited cash.
        date = posting.entry.date + datetime.timedelta(days=1)
        cash = -posting.position.number
        units = -(cash / price).quantize(QU)

        invest_entry = parse_one("""
            {date} * "BUYMF - PRETAX - Price as of date based on closing price"
              Assets:US:Vanguard:PreTax401k:VFORX   {units} VFORX {{{price} USD}}
              Assets:US:Vanguard:PreTax401k:Cash    {cash} USD
        """)

        # Create an entry investing the employer match.
        cash /= 2
        units = -(cash / price).quantize(QU)

        match_entry = parse_one("""
            {date} * "BUYMF - MATCH - Price as of date based on closing price"
              Assets:US:Vanguard:Match401k:VFORX   {units} VFORX {{{price} USD}}
              Income:US:GoogleInc:Match401k        {cash} USD
        """)

        new_entries.append(invest_entry)
        new_entries.append(match_entry)


    # Generate dividend entries.
    year = 2014
    for num_month in range(1, 13):
        month = (num_month % 12) + 1
        date = datetime.date(year, month, 1) - datetime.timedelta(days=1)

        price, _ = fetcher.get_historical_price(date)
        cash = D(random.uniform(2, 25)).quantize(Q)

        units = (cash / price).quantize(QU)

        cash2 = cash / 2
        units2 = (cash2 / price).quantize(QU)

        div_entries = parse_many("""

          {date} * "REINVEST - DIV - PRETAX - Price as of date based on closing price"
            Assets:US:Vanguard:PreTax401k:VFORX   {units} VFORX {{{price} USD}}
            Income:US:Vanguard:Dividend           -{cash} USD

          {date} * "REINVEST - DIV - MATCH - Price as of date based on closing price"
            Assets:US:Vanguard:Match401k:VFORX   {units2} VFORX {{{price} USD}}
            Income:US:Vanguard:Dividend           -{cash2} USD

        """)

        new_entries.extend(div_entries)


    # Generate quarterly fee entries.
    year = 2014
    fee = D('5.00')
    today = datetime.date.today()
    for num_quarter in range(0, 4):
        month = (num_quarter * 3) + 3
        date = datetime.date(year, month, 1) + datetime.timedelta(days=3)

        if date > today:
            # If we're extrapolating, randomly move the last price.
            price += D(float(price) * random.uniform(-0.02, 0.02)).quantize(Q)
        else:
            price, _ = fetcher.get_historical_price(date)

        fee1 = D(random.uniform(float(fee) * 0.20, float(fee) * 0.80)).quantize(Q)
        fee2 = fee - fee1

        units1 = -(fee1 / price).quantize(QU)
        units2 = -(fee2 / price).quantize(QU)

        div_entries = parse_many("""

            {date} * "TRANSFER - PRETAX - Investment Expense"
              Assets:US:Vanguard:PreTax401k:VFORX  {units1} VFORX {{{price} USD}}
              Expenses:Financial:Fees              {fee1} USD

            {date} * "TRANSFER - MATCH - Investment Expense"
              Assets:US:Vanguard:Match401k:VFORX    {units2} VFORX {{{price} USD}}
              Expenses:Financial:Fees               {fee2} USD

        """)

        new_entries.extend(div_entries)

    new_entries = data.sort(new_entries)
    print('plugin "beancount.ops.auto_accounts"\n\n')
    printer.print_entries(new_entries)


if __name__ == '__main__':
    main()
