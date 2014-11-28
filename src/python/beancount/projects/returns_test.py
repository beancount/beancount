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
from beancount.parser import cmptest
from beancount.parser import options
from beancount.parser import printer
from beancount.projects import returns
from beancount.utils import test_utils


def setUp(self):
    logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')


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

    def test_find_matching(self):
        matching_entries, (acc_assets,
                           acc_intflows,
                           acc_extflows) = returns.find_matching(
                               self.entries, self.acc_types,
                               'Assets:US:Prosper', '(Income|Expenses):')

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

    @mock.patch('beancount.projects.returns.compute_returns')
    def test_compute_returns_with_regexp(self, mock_obj):
        returns.compute_returns_with_regexp(self.entries, self.options_map,
                                            'Equity:Internalized',
                                            '.*:Prosper', '(Income|Expenses):')
        self.assertTrue([list, dict, set, set], map(type, mock_obj.call_args))
        self.assertTrue(mock_obj.called)

    def test_compute_returns(self):
        price_map = prices.build_price_map(self.entries)
        matching_entries, (acc_assets, acc_intflows, _) = returns.find_matching(
            self.entries, self.acc_types, '.*:Prosper', '(Income|Expenses):')
        returns.compute_returns(self.entries, 'Equity:Internalized',
                                acc_assets, acc_intflows, price_map)


class TestReturnsPeriods(test_utils.TestCase):

    # Ensure that prelude and epiloge periods are correctly found and returned.
    @loader.loaddoc
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
        periods = returns.segment_periods(entries, assets, assets)
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 2, 1),
             inventory.from_string(''), inventory.from_string('')),
            (datetime.date(2014, 2, 1), datetime.date(2014, 8, 1),
             inventory.from_string('10000 USD'), inventory.from_string('10000 USD'))
            ], periods)

    # Deposit, one investment, no other changes but a price change.
    @loader.loaddoc
    def test_returns_one_transfer(self, entries, errors, options_map):
        """
        plugin "beancount.ops.auto_accounts"

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
        returns_dict, dates, _ = returns.compute_returns_with_regexp(
            entries, options_map,
            'Equity:Internalized',
            '.*:Investments', '(Income|Expenses):')
        self.assertEqual({'USD': 1.1}, returns_dict)
        self.assertEqual((datetime.date(2014, 2, 1), datetime.date(2014, 8, 2)), dates)

    # Dilute returns from a faraway initial date.
    @loader.loaddoc
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
        returns_dict, dates, _ = returns.compute_returns_with_regexp(
            entries, options_map,
            'Equity:Internalized',
            '.*:Investments', '(Income|Expenses):')
        self.assertEqual({'USD': 1.1}, returns_dict)
        self.assertEqual((datetime.date(1990, 1, 1), datetime.date(2014, 8, 2)), dates)


class TestReturnsConstrained(test_utils.TestCase):

    @loader.loaddoc
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
        periods = returns.segment_periods(self.entries, self.assets, self.assets)
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 10, 1)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string(''), periods[0][2])

    def test_segment_periods_with_begin(self):
        # Test with a begin date.
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_begin=datetime.date(2014, 4, 20))
        self.assertEqual([
            (datetime.date(2014, 4, 20), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 10, 1)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string('2900 USD, 21 ACME {100 USD}'),
                         periods[0][2])

        # Test with another begin date.
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_begin=datetime.date(2014, 9, 10))
        self.assertEqual([
            (datetime.date(2014, 9, 10), datetime.date(2014, 10, 1)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string('3720 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}'), periods[0][2])

        # Test with another begin date.
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_begin=datetime.date(2014, 10, 15))
        self.assertEqual([
            (datetime.date(2014, 10, 15), datetime.date(2014, 10, 15))
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string('600 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}, '
                                               '24 ACME {130 USD}'), periods[0][2])

    def test_segment_periods_with_end(self):
        # Test with an end date.
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_end=datetime.date(2014, 4, 20))
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 4, 20)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string('2900 USD, 21 ACME {100 USD}'),
                         periods[-1][3])

        # Test with another end date.
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_end=datetime.date(2014, 9, 10))
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 9, 10)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string('3720 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}'), periods[-1][3])

        # Test with yet another end date.
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_end=datetime.date(2014, 10, 15))
        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 10, 15)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string('600 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}, '
                                               '24 ACME {130 USD}'), periods[-1][3])

    def test_segment_periods_with_begin_and_end(self):
        # Test with an end date.
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_begin=datetime.date(2014, 4, 20),
                                          date_end=datetime.date(2014, 9, 10))
        self.assertEqual([
            (datetime.date(2014, 4, 20), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2014, 9, 10)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string('3720 USD, '
                                               '21 ACME {100 USD}, '
                                               '22 ACME {110 USD}, '
                                               '23 ACME {120 USD}'), periods[-1][3])

    def test_segment_periods_preceding(self):
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_begin=datetime.date(2013, 9, 1),
                                          date_end=datetime.date(2013, 12, 1))
        self.assertEqual([
            (datetime.date(2013, 9, 1), datetime.date(2013, 12, 1),
             inventory.Inventory(), inventory.Inventory()),
            ], periods)

    def test_segment_periods_following(self):
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_begin=datetime.date(2015, 3, 1),
                                          date_end=datetime.date(2015, 6, 1))

        inv_final = inventory.from_string('600 USD, '
                                          '21 ACME {100 USD}, '
                                          '22 ACME {110 USD}, '
                                          '23 ACME {120 USD}, '
                                          '24 ACME {130 USD}')
        self.assertEqual([
            (datetime.date(2015, 3, 1), datetime.date(2015, 6, 1), inv_final, inv_final),
            ], periods)

    def test_segment_periods_around(self):
        periods = returns.segment_periods(self.entries, self.assets, self.assets,
                                          date_begin=datetime.date(2013, 12, 1),
                                          date_end=datetime.date(2015, 3, 1))

        self.assertEqual([
            (datetime.date(2013, 12, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2015, 3, 1)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        inv_final = inventory.from_string('600 USD, '
                                          '21 ACME {100 USD}, '
                                          '22 ACME {110 USD}, '
                                          '23 ACME {120 USD}, '
                                          '24 ACME {130 USD}')
        self.assertEqual(inv_final, periods[-1][3])

    @loader.loaddoc
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

        periods = returns.segment_periods(entries, self.assets, self.assets,
                                          date_end=datetime.date(2020, 1, 1))

        self.assertEqual([
            (datetime.date(2014, 1, 1), datetime.date(2014, 1, 15)),
            (datetime.date(2014, 1, 15), datetime.date(2014, 6, 15)),
            (datetime.date(2014, 6, 15), datetime.date(2020, 1, 1)),
            ], [(date_begin, date_end) for date_begin, date_end, _, __ in periods])

        self.assertEqual(inventory.from_string('1400 USD, '
                                               '21 ACME {100 USD}'), periods[-1][3])


class TestReturnsInternalize(cmptest.TestCase):

    # Check internalization by ignoring cash accounts.
    @loader.loaddoc
    def test_internalization(self, entries, errors, _):
        """
        2014-01-01 open Equity:Somewhere
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-01-01 open Income:Account
        2014-01-01 open Expenses:Account
        2014-01-01 open Expenses:External

        2014-01-01 * "Funding accounts... external flow, with other account"
          Equity:Somewhere     -10000 USD
          Assets:Account1       5000 USD
          Assets:Account2       5000 USD

        2014-01-01 * "Internal flows between asset accounts"
          Assets:Account1       -1000 USD
          Assets:Account2

        2014-01-01 * "Internal flows between asset and intflows accounts"
          Income:Account     -1000 USD
          Expenses:Account      10 USD
          Assets:Account1

        2014-01-01 * "Int/ext flows - should be internalized"
          Income:Account     -100 USD
          Expenses:External   100 USD

        2014-01-01 * "Internal flows only"
          Income:Account       -10 USD
          Expenses:Account      10 USD

        2015-01-01 balance Assets:Account1   4990 USD
        2015-01-01 balance Assets:Account2   6000 USD
        """
        self.assertFalse(errors)

        new_entries, replaced_entries = returns.internalize(
            entries,
            {'Assets:Account1', 'Assets:Account2'},
            {'Income:Account', 'Expenses:Account'},
            'Equity:Internalized')

        # Check that the split entry has been replaced.
        self.assertEqualEntries("""
        2014-01-01 * "Int/ext flows - should be internalized"
          Income:Account     -100 USD
          Expenses:External   100 USD
        """, replaced_entries)

        # Look for the replaced entries and assert them.
        self.assertIncludesEntries("""
        2014-01-01 R "Int/ext flows - should be internalized" ^internalized-00001
          Income:Account       -100 USD
          Equity:Internalized   100 USD

        2014-01-01 R "Int/ext flows - should be internalized" ^internalized-00001
          Expenses:External     100 USD
          Equity:Internalized  -100 USD
        """, new_entries)

        # Check that the internalized account is present.
        self.assertIncludesEntries("""
        2014-01-01 open Equity:Internalized
        """, new_entries)


    # Demonstrate internalization by ignoring cash accounts by explicitly
    # building entries without the need internalization, i.e., we internalize
    # manually.
    @loader.loaddoc
    def test_explicit_solution(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:US:Investments:ACME
        2014-01-01 open Assets:US:Investments:Cash
        2014-01-01 open Assets:US:Bank:Checking
        2014-01-01 open Income:US:Investments:Dividends
        2014-01-01 open Equity:Internalized

        2014-01-01 * "Deposit"
          Assets:US:Investments:Cash       10000 USD
          Assets:US:Bank:Checking

        2014-01-01 * "Buy"
          Assets:US:Investments:ACME       5 ACME {20.00 USD}
          Assets:US:Investments:Cash

        2014-06-01 * "Dividend from ACME"
          Income:US:Investments:Dividends -300 USD
          Equity:Internalized

        2014-06-01 * "Dividend (Internalized)"
          Equity:Internalized             -300 USD
          Assets:US:Investments:Cash

        2015-01-01 price ACME            26.00 USD

        2015-01-01 balance Assets:US:Investments:Cash  10200 USD
        2015-01-01 balance Assets:US:Investments:ACME  5 ACME
        """
        self.assertFalse(errors)
        self.compute_and_check_returns(entries, [])

    # Check internalization by ignoring cash accounts.
    @loader.loaddoc
    def test_implicit_solution(self, entries, errors, _):
        """
        2014-01-01 open Assets:US:Investments:ACME
        2014-01-01 open Assets:US:Investments:Cash
        2014-01-01 open Assets:US:Bank:Checking
        2014-01-01 open Income:US:Investments:Dividends

        2014-01-01 * "Deposit"
          Assets:US:Investments:Cash       10000 USD
          Assets:US:Bank:Checking

        2014-01-01 * "Buy"
          Assets:US:Investments:ACME       5 ACME {20.00 USD}
          Assets:US:Investments:Cash

        2014-06-01 * "Dividend from ACME"
          Income:US:Investments:Dividends -300 USD
          Assets:US:Investments:Cash

        2015-01-01 price ACME            26.00 USD

        2015-01-01 balance Assets:US:Investments:Cash  10200 USD
        2015-01-01 balance Assets:US:Investments:ACME  5 ACME
        """
        self.assertFalse(errors)
        self.compute_and_check_returns(entries, """
            2014-06-01 * "Dividend from ACME"
              Income:US:Investments:Dividends -300 USD
              Assets:US:Investments:Cash
        """)

    def compute_and_check_returns(self, entries, expected_internalized):
        # Compute the returns including cash in the account. This should return
        # the same as the first case in the previous example.
        assets = {'Assets:US:Investments:ACME', 'Assets:US:Investments:Cash'}
        intflows = {'Income:US:Investments:Dividends'}
        returns_with_cash, dates, internalized_entries = returns.compute_returns(
            entries, 'Equity:Internalized', assets, intflows)
        self.assertEqual({'USD': 1.033}, returns_with_cash)
        self.assertEqual((datetime.date(2014, 1, 1), datetime.date(2015, 1, 1)), dates)

        # Compute the returns excluding cash in the account.
        #
        # Now this should correctly reflect the impact of the large dividend when not
        # considering the cash account.
        assets = {'Assets:US:Investments:ACME'}
        returns_no_cash, dates, internalized_entries = returns.compute_returns(
            entries, 'Equity:Internalized', assets, intflows)
        self.assertEqual({'USD': 5.200}, returns_no_cash)
        self.assertEqual((datetime.date(2014, 1, 1), datetime.date(2015, 1, 1)), dates)

        if expected_internalized:
            self.assertEqualEntries(expected_internalized, internalized_entries)


    # Check internalization against an obvious case of internal + external
    # flows, not with an example ignoring cash (though it's the same thing,
    # really).
    @loader.loaddoc
    def test_non_cash_mixed(self, entries, errors, _):
        """
        2014-01-01 open Assets:US:Investments:ACME
        2014-01-01 open Assets:US:Investments:Cash
        2014-01-01 open Assets:US:Bank:Checking
        2014-01-01 open Income:US:Investments:Dividends

        2014-01-01 * "Deposit"
          Assets:US:Investments:Cash       10000 USD
          Assets:US:Bank:Checking

        2014-01-01 * "Buy"
          Assets:US:Investments:ACME       5 ACME {20.00 USD}
          Assets:US:Investments:Cash

        2014-06-01 * "Dividend from ACME paid outside"
          Income:US:Investments:Dividends -300 USD
          Assets:US:Bank:Checking

        2015-01-01 price ACME            26.00 USD

        2015-01-01 balance Assets:US:Investments:Cash  9900 USD
        2015-01-01 balance Assets:US:Investments:ACME  5 ACME
        """
        self.assertFalse(errors)

        assets = {'Assets:US:Investments:ACME', 'Assets:US:Investments:Cash'}
        intflows = {'Income:US:Investments:Dividends'}
        returns_with_cash, dates, internalized_entries = returns.compute_returns(
            entries, 'Equity:Internalized', assets, intflows)

        self.assertEqual({'USD': 1.0330899999999998}, returns_with_cash)
        self.assertEqual((datetime.date(2014, 1, 1), datetime.date(2015, 1, 1)), dates)

        self.assertEqualEntries("""

        2014-06-01 * "Dividend from ACME paid outside"
          Income:US:Investments:Dividends -300 USD
          Assets:US:Bank:Checking

        """, internalized_entries)


class TestReturnsExampleScript(test_utils.TestCase):

    def test_returns_invoke_via_main(self):
        # We want to ensure we can call the module and it doesn't fail.
        example_filename = path.join(test_utils.find_repository_root(__file__),
                                     'examples', 'tutorial', 'example.beancount')
        self.assertTrue(path.exists(example_filename))

        command = [sys.executable, '-m', 'beancount.projects.returns',
                   example_filename,
                   'Assets:US:ETrade', 'Expenses:Financial:Commissions']
        pipe = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output, errors = pipe.communicate()
        self.assertEqual(0, pipe.returncode)
        self.assertTrue(re.search(b'Total returns', output))
        self.assertTrue(re.search(b'Averaged annual returns', output))

    def test_returns_example_script(self):
        # We want to ensure the example script doesn't break unexpectedly, so
        # call it from the unit tests.
        script_name = path.join(test_utils.find_repository_root(__file__),
                                'examples', 'returns', 'returns-example.py')
        self.assertTrue(path.exists(script_name))

        pipe = subprocess.Popen(script_name, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output, errors = pipe.communicate()
        self.assertEqual(0, pipe.returncode)
        self.assertFalse(errors)
        self.assertTrue(re.search(b'Returns for', output))


class TestReturnsWithUnrealized(test_utils.TestCase):

    @loader.loaddoc
    def test_returns_with_unrealized(self, entries, errors, _):
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
            entries, 'Equity:Internalized', assets, intflows)
        self.assertEqual(expected_returns, returns_)

        # Now, the unrealized gains is usually inserted at the end of the list
        # of entries, which has no effect because no final period is created for
        # it. Try moving the unrealized gains a bit earlier, just to make sure
        # it has no effect.
        last_entry = entries[-1]
        moved_entry = data.entry_replace(last_entry,
                                         date=last_entry.date - datetime.timedelta(days=20))
        new_entries = entries[:-1] + [moved_entry]

        returns_, dates, internalized_entries = returns.compute_returns(
            new_entries, 'Equity:Internalized', assets, intflows)
        self.assertEqual(expected_returns, returns_)
