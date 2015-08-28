__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import re
import logging
import textwrap
import sys
import subprocess
from os import path
from unittest import mock

from beancount.projects.returns import Segment
from beancount.projects.returns import Snapshot
from beancount import loader
from beancount.core import inventory
from beancount.ops import prices
from beancount.parser import cmptest
from beancount.parser import options
from beancount.projects import returns
from beancount.utils import test_utils


def setUp(self):
    logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')


def dates_from_timeline(timeline):
    """Given a list of segments (timeline), return the begin/end date pairs.

    This is essentially used for testing.

    Args:
      timeline: A list of Segment instances.
    Returns:
      A corresponding list of date pairs.
    """
    return [(segment.begin.date, segment.end.date)
            for segment in timeline]


class TestReturnsFunctions(test_utils.TestCase):

    INPUT = textwrap.dedent("""

    2000-01-31 open Assets:US:TD:Checking                 USD
    2000-01-31 open Equity:Opening-Balances               USD,LENCLUB

    2013-06-10 open Assets:US:Prosper:Cash            USD
    2013-06-10 open Assets:US:Prosper:InFunding       USD
    2013-06-10 open Assets:US:Prosper:FundsLent       LENCLUB
    2013-06-10 open Income:US:Prosper:LoanInterest    USD
    2013-06-10 open Income:US:Prosper:ChargedOff      USD
    2013-06-10 open Income:US:Prosper:LateFeesRecv    USD
    2013-06-10 open Expenses:Financial:Fees:Prosper   USD


    2014-06-29 P "Opening balances"
      Assets:US:Prosper:Cash            802.30 USD
      Assets:US:Prosper:FundsLent     19954.84 LENCLUB {1 USD}
      Equity:Opening-Balances


    2014-06-30 balance Assets:US:Prosper:Cash            802.30 USD
    2014-06-30 balance Assets:US:Prosper:InFunding         0.00 USD
    2014-06-30 balance Assets:US:Prosper:FundsLent     19954.84 LENCLUB


    2014-07-01 * "Monthly_Statement"
      Assets:US:Prosper:InFunding                               2575.00 USD
      Assets:US:Prosper:FundsLent                               1825.00 LENCLUB {1 USD}
      Assets:US:Prosper:FundsLent                               -774.05 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                     774.05 USD
      Income:US:Prosper:LoanInterest                            -184.55 USD
      Expenses:Financial:Fees:Prosper                              9.59 USD
      Assets:US:Prosper:Cash

    2014-07-30 balance Assets:US:Prosper:Cash           7351.31 USD
    2014-07-30 balance Assets:US:Prosper:InFunding      2575.00 USD
    2014-07-30 balance Assets:US:Prosper:FundsLent     21005.79 LENCLUB


    2014-07-10 * "PROSPER" | "Transfer money into it."
      Assets:US:TD:Checking                                               -10000.00 USD
      Assets:US:Prosper:Cash


    2014-07-31 * "Monthly_Statement"
      Assets:US:Prosper:FundsLent                               9325.00 LENCLUB {1 USD}
      Assets:US:Prosper:FundsLent                               -807.23 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                     807.23 USD
      Income:US:Prosper:LoanInterest                            -190.23 USD
      Income:US:Prosper:LateFeesRecv                              -0.04 USD
      Expenses:Financial:Fees:Prosper                              9.97 USD
      Assets:US:Prosper:InFunding                              -1775.00 USD
      Assets:US:Prosper:Cash

    2014-08-30 balance Assets:US:Prosper:Cash            788.84 USD
    2014-08-30 balance Assets:US:Prosper:InFunding       800.00 USD
    2014-08-30 balance Assets:US:Prosper:FundsLent     29523.56 LENCLUB


    2014-08-31 * "Monthly_Statement"
      Assets:US:Prosper:FundsLent                               1400.00 LENCLUB {1 USD}
      Assets:US:Prosper:FundsLent                              -1128.78 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                    1128.78 USD
      Income:US:Prosper:LoanInterest                            -257.59 USD
      Expenses:Financial:Fees:Prosper                             13.86 USD
      Assets:US:Prosper:InFunding                               -800.00 USD
      Assets:US:Prosper:Cash
      Assets:US:Prosper:FundsLent                                -21.61 LENCLUB {1 USD}
      Income:US:Prosper:ChargedOff                                21.61 USD

    2014-09-30 balance Assets:US:Prosper:Cash           1561.35 USD
    2014-09-30 balance Assets:US:Prosper:InFunding         0.00 USD
    2014-09-30 balance Assets:US:Prosper:FundsLent     29773.17 LENCLUB


    2014-10-01 * "Monthly_Statement"
      Assets:US:Prosper:FundsLent                              -1227.38 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                    1227.37 USD
      Income:US:Prosper:LoanInterest                            -263.13 USD
      Income:US:Prosper:LateFeesRecv                              -0.08 USD
      Expenses:Financial:Fees:Prosper                             14.90 USD
      Assets:US:Prosper:Cash

    2014-10-31 balance Assets:US:Prosper:Cash           3037.04 USD
    2014-10-31 balance Assets:US:Prosper:InFunding         0.00 USD
    2014-10-31 balance Assets:US:Prosper:FundsLent     28545.79 LENCLUB


    2014-11-01 * "Monthly_Statement"
      Assets:US:Prosper:FundsLent                              -1063.20 LENCLUB {1 USD}
      Assets:US:Prosper:Cash                                    1063.20 USD
      Income:US:Prosper:LoanInterest                            -277.26 USD
      Income:US:Prosper:LateFeesRecv                              -0.08 USD
      Expenses:Financial:Fees:Prosper                             13.15 USD
      Expenses:Financial:Fees:Prosper                              0.42 USD ;; Collection fees
      Assets:US:Prosper:Cash

    2014-11-06 * "PROSPER" | "Withdrawal"
      Assets:US:TD:Checking                                                 3200.00 USD
      Assets:US:Prosper:Cash


    2014-11-30 balance Assets:US:Prosper:Cash           1164.01 USD
    2014-11-30 balance Assets:US:Prosper:InFunding         0.00 USD
    2014-11-30 balance Assets:US:Prosper:FundsLent     27482.59 LENCLUB


    2014-12-04 * "PROSPER"
      Assets:US:TD:Checking                                                       1180.00 USD
      Assets:US:Prosper:Cash

    """)

    def setUp(self):
        self.entries, errors, self.options_map = loader.load_string(self.INPUT)
        self.assertFalse(errors)
        self.acc_types = options.get_account_types(self.options_map)

    def test_regexps_to_accounts(self):
        (acc_assets,
         acc_intflows,
         acc_extflows,
         acc_internalize) = returns.regexps_to_accounts(
             self.entries, 'Assets:US:Prosper', '(Income|Expenses):')

        self.assertEqual({'Assets:US:Prosper:InFunding',
                          'Assets:US:Prosper:Cash',
                          'Assets:US:Prosper:FundsLent'}, acc_assets)

        self.assertEqual({'Income:US:Prosper:LateFeesRecv',
                          'Income:US:Prosper:ChargedOff',
                          'Expenses:Financial:Fees:Prosper',
                          'Income:US:Prosper:LoanInterest'}, acc_intflows)

        self.assertEqual({'Equity:Opening-Balances',
                          'Assets:US:TD:Checking'},
                         acc_extflows)

        self.assertEqual(None,
                         acc_internalize)

    def test_compute_returns(self):
        (acc_assets, acc_intflows, _, _) = returns.regexps_to_accounts(
            self.entries, '.*:Prosper', '(Income|Expenses):')
        returns.compute_returns(self.entries, self.options_map,
                                'Equity:Internalized', acc_assets, acc_intflows)


class TestReturnsPeriods(test_utils.TestCase):

    # Ensure that prelude and epiloge periods are correctly found and returned.
    @loader.load_doc()
    def test_segment_periods(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:US:Investments:Cash
        2014-01-01 open Assets:US:Bank:Checking

        2014-01-01 price ACME  90.00 USD

        2014-02-01 * "Deposit"
          Assets:US:Investments:Cash       10,000 USD
          Assets:US:Bank:Checking

        2014-08-01 balance Assets:US:Investments:Cash       10,000 USD
        2014-08-01 price ACME  100.00 USD
        """
        self.assertFalse(errors)
        assets = {'Assets:US:Investments:Cash'}
        timeline = returns.segment_periods(entries, assets, assets)
        self.assertEqual(2, len(timeline))
        empty = inventory.from_string('')
        self.assertEqual(timeline[0].begin, Snapshot(datetime.date(2014, 1, 1), empty))
        self.assertEqual(timeline[0].end, Snapshot(datetime.date(2014, 2, 1), empty))
        self.assertEqual(timeline[1].begin, Snapshot(datetime.date(2014, 2, 1),
                                                     inventory.from_string('10000 USD')))
        self.assertEqual(timeline[1].end, Snapshot(datetime.date(2014, 8, 1),
                                                   inventory.from_string('10000 USD')))

    # Deposit, one investment, no other changes but a price change.
    @loader.load_doc()
    def test_returns_one_transfer(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.auto_accounts"

        2014-02-01 * "Deposit"
          Assets:US:Investments:Cash       10,000 USD
          Assets:US:Bank:Checking

        2014-02-01 * "Invest"
          Assets:US:Investments:ACME       100 ACME {90 USD}
          Assets:US:Investments:Cash       -9,000 USD

        2014-08-01 price ACME  100.00 USD

        2014-08-01 * "Withdrawal"
          Assets:US:Investments:Cash       -1,000 USD
          Assets:US:Bank:Checking

        2014-08-02 balance Assets:US:Investments:Cash    0 USD
        """
        self.assertFalse(errors)
        returns_dict, dates, _ = returns.compute_returns(
            entries, options_map,
            'Equity:Internalized',
            ['Assets:US:Investments:ACME', 'Assets:US:Investments:Cash'], [])
        self.assertEqual({'USD': 1.1}, returns_dict)
        self.assertEqual((datetime.date(2014, 2, 1), datetime.date(2014, 8, 2)), dates)

    # Dilute returns from a faraway initial date.
    @loader.load_doc()
    def test_returns_diluted(self, entries, errors, options_map):
        """
        1990-01-01 open Assets:US:Investments:Cash
        1990-01-01 open Assets:US:Investments:ACME
        1990-01-01 open Assets:US:Bank:Checking

        2014-02-01 * "Deposit"
          Assets:US:Investments:Cash       10,000 USD
          Assets:US:Bank:Checking

        2014-02-01 * "Invest"
          Assets:US:Investments:ACME       100 ACME {90 USD}
          Assets:US:Investments:Cash       -9,000 USD

        2014-08-01 price ACME  100.00 USD
        2014-08-02 balance Assets:US:Investments:Cash       1,000 USD
        """
        self.assertFalse(errors)
        returns_dict, dates, _ = returns.compute_returns(
            entries, options_map,
            'Equity:Internalized',
            ['Assets:US:Investments:ACME', 'Assets:US:Investments:Cash'], [])
        self.assertEqual({'USD': 1.1}, returns_dict)
        self.assertEqual((datetime.date(1990, 1, 1), datetime.date(2014, 8, 2)), dates)


class TestReturnsConstrained(test_utils.TestCase):

    @loader.load_doc()
    def setUp(self, entries, errors, _):
        """
        2014-01-01 open Assets:US:Investments:ACME
        2014-01-01 open Assets:US:Investments:Cash
        2014-01-01 open Assets:US:Bank:Checking

        2014-01-15 * "Deposit"
          Assets:US:Investments:Cash       5000 USD
          Assets:US:Bank:Checking

        2014-02-01 * "Buy"
          Assets:US:Investments:ACME       21 ACME {100 USD}
          Assets:US:Investments:Cash

        2014-05-01 * "Buy"
          Assets:US:Investments:ACME       22 ACME {110 USD}
          Assets:US:Investments:Cash

        2014-06-15 * "Deposit"
          Assets:US:Investments:Cash       6000 USD
          Assets:US:Bank:Checking

        2014-08-01 * "Buy"
          Assets:US:Investments:ACME       23 ACME {120 USD}
          Assets:US:Investments:Cash

        2014-10-01 * "Buy"
          Assets:US:Investments:ACME       24 ACME {130 USD}
          Assets:US:Investments:Cash
        """
        self.entries = entries
        self.assertFalse(errors)
        self.assets = {'Assets:US:Investments:ACME',
                       'Assets:US:Investments:Cash'}

    def test_segment_periods_no_constraint(self):
        # Test the default case, no beginning.
        timeline = returns.segment_periods(self.entries, self.assets, self.assets)
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 10, 1)),
            ], dates_from_timeline(timeline))
        self.assertEqual(inventory.from_string(''), timeline[0].end.balance)

    def test_segment_periods_with_begin(self):
        # Test with a begin date.
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_begin=datetime.date(2014, 4, 20))
        self.assertEqual([
            (datetime.date(2014, 4, 20), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 10, 1)),
            ], dates_from_timeline(timeline))

        self.assertEqual(inventory.from_string('2900 USD, 21 ACME {100 USD}'),
                         timeline[0].begin.balance)

        # Test with another begin date.
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_begin=datetime.date(2014, 9, 10))
        self.assertEqual([
            (datetime.date(2014, 9, 10), datetime.date(2014, 10, 1)),
            ], dates_from_timeline(timeline))

        self.assertEqual(inventory.from_string('3720 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}'),
                         timeline[0].begin.balance)

        # Test with another begin date.
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_begin=datetime.date(2014, 10, 15))
        self.assertEqual([
            (datetime.date(2014, 10, 15), datetime.date(2014, 10, 15))
            ], dates_from_timeline(timeline))

        self.assertEqual(inventory.from_string('600 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}, '
                                               '24 ACME {130 USD}'),
                         timeline[0].begin.balance)

    def test_segment_periods_with_end(self):
        # Test with an end date.
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_end=datetime.date(2014, 4, 20))
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 4, 20)),
            ], dates_from_timeline(timeline))

        self.assertEqual(inventory.from_string('2900 USD, 21 ACME {100 USD}'),
                         timeline[-1].end.balance)

        # Test with another end date.
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_end=datetime.date(2014, 9, 10))
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 9, 10)),
            ], dates_from_timeline(timeline))

        self.assertEqual(inventory.from_string('3720 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}'),
                         timeline[-1].end.balance)

        # Test with yet another end date.
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_end=datetime.date(2014, 10, 15))
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 10, 15)),
            ], dates_from_timeline(timeline))

        self.assertEqual(inventory.from_string('600 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}, '
                                               '24 ACME {130 USD}'),
                         timeline[-1].end.balance)

    def test_segment_periods_with_begin_and_end(self):
        # Test with an end date.
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_begin=datetime.date(2014, 4, 20),
                                           date_end=datetime.date(2014, 9, 10))
        self.assertEqual([
            (datetime.date(2014, 4, 20), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 9, 10)),
            ], dates_from_timeline(timeline))

        self.assertEqual(inventory.from_string('3720 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}'),
                         timeline[-1].end.balance)

    def test_segment_periods_preceding(self):
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_begin=datetime.date(2013, 9, 1),
                                           date_end=datetime.date(2013, 12, 1))
        self.assertEqual([
            Segment(Snapshot(datetime.date(2013, 9, 1), inventory.Inventory()),
                    Snapshot(datetime.date(2013, 12, 1), inventory.Inventory()),
                    [], [])
        ], timeline)

    def test_segment_periods_following(self):
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_begin=datetime.date(2015, 3, 1),
                                           date_end=datetime.date(2015, 6, 1))

        inv_final = inventory.from_string('600 USD, '
                                          '21 ACME {100 USD}, '
                                          '22 ACME {110 USD}, '
                                          '23 ACME {120 USD}, '
                                          '24 ACME {130 USD}')
        self.assertEqual([
            Segment(Snapshot(datetime.date(2015, 3, 1), inv_final),
                    Snapshot(datetime.date(2015, 6, 1), inv_final),
                    [], []),
        ], timeline)

    def test_segment_periods_around(self):
        timeline = returns.segment_periods(self.entries, self.assets, self.assets,
                                           date_begin=datetime.date(2013, 12, 1),
                                           date_end=datetime.date(2015, 3, 1))

        self.assertEqual([
            (datetime.date(2013, 12, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2015, 3, 1)),
            ], dates_from_timeline(timeline))

        inv_final = inventory.from_string('600 USD, '
                                          '21 ACME {100 USD}, '
                                          '22 ACME {110 USD}, '
                                          '23 ACME {120 USD}, '
                                          '24 ACME {130 USD}')
        self.assertEqual(inv_final, timeline[-1].end.balance)

    @loader.load_doc()
    def test_segment_periods_hanging_last_period(self, entries, errors, _):
        """
        2014-01-01 open Assets:US:Investments:ACME
        2014-01-01 open Assets:US:Investments:Cash
        2014-01-01 open Assets:US:Bank:Checking

        2014-01-15 * "Deposit"
          Assets:US:Investments:Cash       2500 USD
          Assets:US:Bank:Checking

        2014-02-01 * "Buy"
          Assets:US:Investments:ACME       21 ACME {100 USD}
          Assets:US:Investments:Cash

        2014-06-15 * "Deposit"
          Assets:US:Investments:Cash       1000 USD
          Assets:US:Bank:Checking
        """
        self.assertFalse(errors)

        timeline = returns.segment_periods(entries, self.assets, self.assets,
                                           date_end=datetime.date(2020, 1, 1))

        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2020, 1, 1)),
            ], dates_from_timeline(timeline))

        self.assertEqual(inventory.from_string('1400 USD, '
                                               '21 ACME {100 USD}'),
                         timeline[-1].end.balance)


class TestReturnsInternalize(cmptest.TestCase):

    # Check internalization of all transaction categories.
    @loader.load_doc()
    def test_internalization_implicit(self, entries, errors, _):
        """
        ;; Value accounts
        2014-01-01 open Assets:Invest:Cash      USD
        2014-01-01 open Assets:Invest:BOOG      BOOG

        ;; Internal accounts (non-value)
        2014-01-01 open Income:Invest:PnL       USD
        2014-01-01 open Income:Invest:Dividends USD
        2014-01-01 open Expenses:Commissions    USD
        2014-01-01 open Expenses:Fees           USD

        ;; External accounts
        2014-01-01 open Assets:Bank:Checking    USD
        2014-01-01 open Income:Salary           USD
        2014-01-01 open Expenses:Taxes          USD

        ;; EXTERNAL ONLY
        2014-01-02 * "Salary Pay"
          Income:Salary            -3461.54 USD
          Expenses:Taxes            1176.92 USD
          Assets:Bank:Checking      2284.62 USD

        ;; VALUE + EXTERNAL
        2014-01-10 * "Transferring money for investing"
          Assets:Bank:Checking      -500.00 USD
          Assets:Invest:Cash         500.00 USD

        ;; VALUE ONLY
        2014-02-01 * "Buying some BOOG"
          Assets:Invest:Cash        -650.00 USD
          Assets:Invest:BOOG             10 BOOG {65 USD}

        ;; VALUE + INTERNAL
        2014-02-15 * "Selling half my position"
          Assets:Invest:BOOG             -5 BOOG {65 USD} @ 70 USD
          Assets:Invest:Cash         340.05 USD
          Expenses:Commissions         9.95 USD
          Income:Invest:PnL          -25.00 USD

        ;; VALUE + INTERNAL
        2014-02-20 * "Dividends from BOOG position"
          Assets:Invest:Cash          12.00 USD
          Income:Invest:Dividends    -12.00 USD

        ;; INTERNAL + EXTERNAL
        2014-03-17 * "Monthly fees"
          Assets:Bank:Checking        -4.00 USD
          Expenses:Fees                4.00 USD

        ;; INTERNAL + EXTERNAL
        2014-03-20 * "Dividend payment correction with fee"
          Income:Invest:Dividends     -9.00 USD
          Assets:Bank:Checking         9.00 USD

        ;; INTERNAL ONLY
        2014-03-20 * "Dividend payment with fee"
          Income:Invest:Dividends     -9.00 USD
          Expenses:Fees                9.00 USD

        ;; VALUE + INTERNAL + EXTERNAL
        2014-04-01 * "Transferring money by wire"
          Assets:Bank:Checking      -500.00 USD
          Assets:Invest:Cash         480.00 USD
          Expenses:Fees               20.00 USD

        ;; VALUE + EXTERNAL
        2014-06-30 * "Taking some money out for car repairs"
          Assets:Invest:Cash        -400.00 USD
          Assets:Bank:Checking       400.00 USD

        2015-01-01 balance Assets:Invest:Cash     282.05 USD
        2015-01-01 balance Assets:Bank:Checking  1689.62 USD
        """
        self.assertFalse(errors)

        accounts_value = {'Assets:Invest:Cash', 'Assets:Invest:BOOG'}
        accounts_intflows = {'Income:Invest:PnL', 'Income:Invest:Dividends',
                             'Expenses:Commissions', 'Expenses:Fees'}
        new_entries, replaced_entries = returns.internalize(
            entries, 'Equity:Internalized',
            accounts_value, accounts_intflows)

        # Check that the split entry has been replaced.
        self.assertEqualEntries("""
        2014-04-01 * "Transferring money by wire"
          Assets:Bank:Checking      -500.00 USD
          Assets:Invest:Cash         480.00 USD
          Expenses:Fees               20.00 USD
        """, replaced_entries)

        # Look for the replaced entries and assert them.
        self.assertIncludesEntries("""
        2014-04-01 R "Transferring money by wire" ^internalized-00001
          Assets:Invest:Cash    480.00 USD
          Expenses:Fees          20.00 USD
          Equity:Internalized  -500.00 USD

        2014-04-01 R "Transferring money by wire" ^internalized-00001
          Equity:Internalized    500.00 USD
          Assets:Bank:Checking  -500.00 USD
        """, new_entries)

        # Check that the internalized account is present.
        self.assertIncludesEntries("""
        2014-01-01 open Equity:Internalized
        """, new_entries)

    @loader.load_doc()
    def test_internalization_explicit(self, entries, errors, _):
        """
        2014-01-01 open Assets:Invest:Cash      USD
        2014-01-01 open Assets:Invest:BOOG      BOOG
        2014-01-01 open Income:Invest:Dividends USD
        2014-01-01 open Assets:Bank:Checking    USD

        2014-01-10 * "Transferring money for investing"
          Assets:Bank:Checking      -1000.00 USD
          Assets:Invest:Cash         1000.00 USD

        2014-02-01 * "Buying"
          Assets:Invest:Cash       -1000.00 USD
          Assets:Invest:BOOG             10 BOOG {100 USD}

        2014-03-01 * "Dividend"
          Income:Invest:Dividends     -90.00 USD
          Assets:Invest:Cash           90.00 USD

        2015-01-01 balance Assets:Invest:BOOG         10 BOOG
        2015-01-01 balance Assets:Invest:Cash      90.00 USD
        """
        self.assertFalse(errors)

        # Check internalizing when including cash accounts.
        new_entries, replaced_entries = returns.internalize(
            entries, 'Equity:Internalized',
            {'Assets:Invest:Cash', 'Assets:Invest:BOOG'},
            {'Income:Invest:Dividends'})
        self.assertEqual([], replaced_entries)

        # Check internalizing when excluding cash accounts.
        new_entries, replaced_entries = returns.internalize(
            entries, 'Equity:Internalized',
            {'Assets:Invest:BOOG'},
            {'Income:Invest:Dividends'},
            {'Income:Invest:Dividends'})

        self.assertEqualEntries("""
        2014-03-01 * "Dividend"
          Income:Invest:Dividends     -90.00 USD
          Assets:Invest:Cash           90.00 USD
        """, replaced_entries)


    @loader.load_doc()
    def test_internalization_implicit_returns(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:Bank:Checking    USD
        2014-01-01 open Assets:Invest:Cash      USD
        2014-01-01 open Expenses:Fees           USD

        2014-01-10 * "Transferring money for investing"
          Assets:Bank:Checking      -1000.00 USD
          Assets:Invest:Cash         1000.00 USD

        2014-04-01 * "Transferring money by wire"
          Assets:Bank:Checking      -500.00 USD
          Assets:Invest:Cash         480.00 USD
          Expenses:Fees               20.00 USD

        2015-01-01 balance Assets:Invest:Cash     1480.00 USD
        """
        self.assertFalse(errors)
        returns_, dates, internalized_entries = returns.compute_returns(
            entries, options_map, 'Equity:Internalized',
            {'Assets:Invest:Cash'},
            {'Expenses:Fees'},
            None)

        # IMPORTANT NOTE: When you internalize, the returns are affected by
        # whether the internalized transaction is added before or after the
        # externalized one, because the fee is taken over one side or the other,
        # and their amounts are of a different magnitude. I'm not sure how best
        # it is to handle this.
        self.assertEqual({'USD': 0.98}, returns_)
        self.assertEqual((datetime.date(2014, 1, 1), datetime.date(2015, 1, 1)), dates)

    def test_internalization_explicit_fails(self):
        with self.assertRaises(ValueError):
            returns.internalize(
                [], 'Equity:Internalized',
                {'Assets:Invest:Cash'}, {'Expenses:Fees'}, {'Income:Invest:PnL'})

    @loader.load_doc()
    def test_internalization_explicit_returns(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:Bank:Checking     USD
        2014-01-01 open Assets:Invest:Cash       USD
        2014-01-01 open Income:Invest:Dividends  USD

        2014-01-10 * "Transferring money for investing"
          Assets:Bank:Checking      -1000.00 USD
          Assets:Invest:Cash         1000.00 USD

        2014-04-01 * "Dividends paid outside the account"
          Assets:Bank:Checking       100.00 USD
          Income:Invest:Dividends   -100.00 USD

        2015-01-01 balance Assets:Invest:Cash     1000.00 USD
        """
        self.assertFalse(errors)
        returns_, dates, internalized_entries = returns.compute_returns(
            entries, options_map, 'Equity:Internalized',
            {'Assets:Invest:Cash'},
            {'Income:Invest:Dividends', 'Expenses:Fees'},
            {'Income:Invest:Dividends'})
        self.assertEqual({'USD': 1.1}, returns_)
        self.assertEqual((datetime.date(2014, 1, 1), datetime.date(2015, 1, 1)), dates)

    @loader.load_doc()
    def test_internalization_explicit_returns_bycash(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:Bank:Checking     USD
        2014-01-01 open Assets:Invest:BOOG       BOOG
        2014-01-01 open Assets:Invest:Cash       USD
        2014-01-01 open Income:Invest:Dividends  USD

        2014-01-10 * "Transferring money for investing"
          Assets:Bank:Checking      -1000.00 USD
          Assets:Invest:Cash         1000.00 USD

        2014-01-10 * "Buying"
          Assets:Invest:Cash       -500.00 USD
          Assets:Invest:BOOG            50 BOOG {10.00 USD}
        2014-01-10 price      BOOG   10.00 USD

        2014-04-01 * "Dividends"
          Assets:Invest:Cash         100.00 USD
          Income:Invest:Dividends   -100.00 USD

        2015-01-01 balance Assets:Invest:Cash     600.00 USD
        2015-01-01 balance Assets:Invest:BOOG         50 BOOG
        """
        self.assertFalse(errors)
        returns_, dates, internalized_entries = returns.compute_returns(
            entries, options_map, 'Equity:Internalized',
            {'Assets:Invest:BOOG'},
            {'Income:Invest:Dividends'},
            {'Income:Invest:Dividends'})
        self.assertEqual({'USD': 1.2}, returns_)
        self.assertEqual((datetime.date(2014, 1, 1), datetime.date(2015, 1, 1)), dates)


class TestReturnsExampleScript(test_utils.TestCase):

    def test_returns_invoke_via_main(self):
        # We want to ensure we can call the module and it doesn't fail.
        example_filename = path.join(test_utils.find_repository_root(__file__),
                                     'examples', 'example.beancount')
        self.assertTrue(path.exists(example_filename))

        command = [sys.executable, '-m', 'beancount.projects.returns',
                   example_filename,
                   'Assets:US:ETrade', 'Expenses:Financial:Commissions']
        pipe = subprocess.Popen(command,
                                env=test_utils.subprocess_env(),
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output, errors = pipe.communicate()
        self.assertEqual(0, pipe.returncode)
        self.assertTrue(re.search(b'Total returns', output))
        self.assertTrue(re.search(b'Averaged annual returns', output))

    def test_returns_example_script(self):
        # We want to ensure the example script doesn't break unexpectedly, so
        # call it from the unit tests.
        script_name = path.join(test_utils.find_repository_root(__file__),
                                'examples', 'example.returns.py')
        self.assertTrue(path.exists(script_name))

        pipe = subprocess.Popen(script_name,
                                env=test_utils.subprocess_env(),
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output, errors = pipe.communicate()
        self.assertEqual(0, pipe.returncode)
        self.assertFalse(errors)
        self.assertTrue(re.search(b'Returns for', output))


class TestReturnsWithUnrealized(test_utils.TestCase):

    @loader.load_doc()
    def test_returns_with_unrealized(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.unrealized"

        2014-01-01 open Assets:US:Investments:ACME
        2014-01-01 open Assets:US:Investments:Cash
        2014-01-01 open Assets:US:Bank:Checking
        2014-01-01 open Income:US:Investments:Dividends

        2014-01-01 * "Deposit"
          Assets:US:Investments:Cash      1000 USD
          Assets:US:Bank:Checking

        2014-01-01 * "Buy"
          Assets:US:Investments:ACME        50 ACME {20.00 USD}
          Assets:US:Investments:Cash

        2015-01-01 price ACME            20.30 USD

        2015-01-01 balance Assets:US:Investments:ACME  50 ACME
        """
        self.assertFalse(errors)

        expected_returns = {'USD': 1.015}

        assets = {'Assets:US:Investments:ACME', 'Assets:US:Investments:Cash'}
        intflows = {'Income:US:Investments:Dividends'}
        returns_, dates, internalized_entries = returns.compute_returns(
            entries, options_map, 'Equity:Internalized', assets, intflows)
        self.assertEqual(expected_returns, returns_)

        # Now, the unrealized gains is usually inserted at the end of the list
        # of entries, which has no effect because no final period is created for
        # it. Try moving the unrealized gains a bit earlier, just to make sure
        # it has no effect.
        last_entry = entries[-1]
        moved_entry = last_entry._replace(
            date=last_entry.date - datetime.timedelta(days=20))
        new_entries = entries[:-1] + [moved_entry]

        returns_, dates, internalized_entries = returns.compute_returns(
            new_entries, options_map, 'Equity:Internalized', assets, intflows)
        self.assertEqual(expected_returns, returns_)
