__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import re
import logging
import textwrap
import sys
import subprocess
from os import path
from unittest import mock

from beancount import loader
from beancount.core import inventory
from beancount.core import data
from beancount.ops import prices
from beancount.parser import parser
from beancount.parser import cmptest
from beancount.parser import options
from beancount.projects import lifetimes
from beancount.utils import test_utils



class TestCommodityLifetimes(test_utils.TestCase):

    @loader.loaddoc
    def test_lifetimes_different_currencies(self, entries, errors, _):
        """
        2000-01-01 open Assets:US:Invest:Cash    USD
        2000-01-01 open Assets:US:Invest:AAPL    AAPL
        2000-01-01 open Assets:US:Invest:CSCO    CSCO
        2000-01-01 open Assets:US:Invest:INTL    INTL
        2000-01-01 open Assets:US:Invest:IBM     IBM
        2000-01-01 open Income:US:Invest:PnL
        2000-01-01 open Assets:US:Bank:Checking  USD

        2000-01-02 * "Deposit"
          Assets:US:Bank:Checking    -10000 USD
          Assets:US:Invest:Cash       10000 USD

        2001-02-10 * "Buy shares"
          Assets:US:Invest:AAPL     10 AAPL {43.00 USD}
          Assets:US:Invest:CSCO     10 CSCO {21.00 USD}
          Assets:US:Invest:INTL     10 INTL {75.00 USD}
          Assets:US:Invest:IBM      10 IBM  {16.00 USD}
          Assets:US:Invest:Cash

        2001-07-20 * "Sell AAPL"
          Assets:US:Invest:AAPL     -10 AAPL {43.00 USD}
          Assets:US:Invest:Cash   500.00 USD
          Income:US:Invest:PnL

        2001-07-21 * "Sell CSCO"
          Assets:US:Invest:CSCO     -10 CSCO {21.00 USD}
          Assets:US:Invest:Cash   300.00 USD
          Income:US:Invest:PnL

        2001-07-22 * "Sell INTL"
          Assets:US:Invest:INTL     -10 INTL {75.00 USD}
          Assets:US:Invest:Cash   800.00 USD
          Income:US:Invest:PnL

        2001-07-23 * "Sell IBM"
          Assets:US:Invest:IBM     -10 IBM {16.00 USD}
          Assets:US:Invest:Cash   200.00 USD
          Income:US:Invest:PnL

        """
        self.assertFalse(errors)
        lifetimes_map = lifetimes.get_time_intervals(entries)
        self.assertEqual(
            {'USD': [(datetime.date(2000, 1, 2), None)],
             'AAPL': [(datetime.date(2001, 2, 10), datetime.date(2001, 7, 21))],
             'CSCO': [(datetime.date(2001, 2, 10), datetime.date(2001, 7, 22))],
             'INTL': [(datetime.date(2001, 2, 10), datetime.date(2001, 7, 23))],
             'IBM': [(datetime.date(2001, 2, 10), datetime.date(2001, 7, 24))]},
            lifetimes_map)

    @loader.loaddoc
    def test_lifetimes_closed_open(self, entries, errors, _):
        """
        2000-01-01 open Assets:US:Invest:Cash    USD
        2000-01-01 open Assets:US:Invest:AAPL    AAPL
        2000-01-01 open Income:US:Invest:PnL
        2000-01-01 open Assets:US:Bank:Checking  USD

        2000-01-02 * "Deposit"
          Assets:US:Bank:Checking    -10000 USD
          Assets:US:Invest:Cash       10000 USD

        2001-03-10 * "Buy Apple"
          Assets:US:Invest:AAPL     10 AAPL {43.40 USD}
          Assets:US:Invest:Cash

        2001-08-10 * "Sell some Apple - some will remain"
          Assets:US:Invest:AAPL     -8 AAPL {43.40 USD}
          Assets:US:Invest:Cash   360.00 USD
          Income:US:Invest:PnL

        2001-12-10 * "Sell remaining Apple - this completes the interval"
          Assets:US:Invest:AAPL     -2 AAPL {43.40 USD}
          Assets:US:Invest:Cash   96.00 USD
          Income:US:Invest:PnL

        2002-02-10 * "Buy Apple again - this begins a new interval"
          Assets:US:Invest:AAPL     5 AAPL {48.00 USD}
          Assets:US:Invest:Cash

        2002-06-10 * "Sell Apple again - this ends it"
          Assets:US:Invest:AAPL     -5 AAPL {48.00 USD}
          Assets:US:Invest:Cash   260.00 USD
          Income:US:Invest:PnL

        2003-04-10 * "Buy Apple - keep this open"
          Assets:US:Invest:AAPL     7 AAPL {50.00 USD}
          Assets:US:Invest:Cash
        """
        self.assertFalse(errors)
        lifetimes_map = lifetimes.get_time_intervals(entries)
        self.assertEqual(
            {'USD': [(datetime.date(2000, 1, 2), None)],
             'AAPL': [(datetime.date(2001, 3, 10), datetime.date(2001, 12, 11)),
                      (datetime.date(2002, 2, 10), datetime.date(2002, 6, 11)),
                      (datetime.date(2003, 4, 10), None)]},
            lifetimes_map)

    @loader.loaddoc
    def test_lifetimes_cross_accounts(self, entries, errors, _):
        """
        2000-01-01 open Assets:US:InvestA:Cash    USD
        2000-01-01 open Assets:US:InvestA:AAPL    AAPL
        2000-01-01 open Income:US:InvestA:PnL
        2000-01-01 open Assets:US:InvestB:Cash    USD
        2000-01-01 open Assets:US:InvestB:AAPL    AAPL
        2000-01-01 open Income:US:InvestB:PnL
        2000-01-01 open Assets:US:Bank:Checking  USD

        2000-01-02 * "Deposit"
          Assets:US:Bank:Checking    -10000 USD
          Assets:US:InvestA:Cash       5000 USD
          Assets:US:InvestB:Cash       5000 USD

        2001-03-10 * "Buy Apple - in first account"
          Assets:US:InvestA:AAPL     10 AAPL {43.40 USD}
          Assets:US:InvestA:Cash

        2001-05-10 * "Buy Apple - in second account"
          Assets:US:InvestB:AAPL     10 AAPL {44.10 USD}
          Assets:US:InvestB:Cash

        2001-06-10 * "Sell Apple - in first account, resulting position is zero"
          Assets:US:InvestA:AAPL     -10 AAPL {43.40 USD}
          Assets:US:InvestA:Cash  500.00 USD
          Income:US:InvestA:PnL

        2001-06-11 balance Assets:US:InvestA:AAPL    0 AAPL

        2001-09-10 * "Sell Apple - in second account, this is the last AAPL position"
          Assets:US:InvestB:AAPL     -10 AAPL {44.10 USD}
          Assets:US:InvestB:Cash  500.00 USD
          Income:US:InvestB:PnL

        2001-09-11 balance Assets:US:InvestB:AAPL    0 AAPL
        """
        self.assertFalse(errors)
        lifetimes_map = lifetimes.get_time_intervals(entries)
        self.assertEqual(
            {'AAPL': [(datetime.date(2001, 3, 10), datetime.date(2001, 9, 11))],
             'USD': [(datetime.date(2000, 1, 2), None)]},
            lifetimes_map)
