__author__ = "Martin Blais <blais@furius.ca>"

import re
import unittest

from beancount.core.amount import Decimal
from beancount.ops import balance
from beancount.loader import loaddoc
from beancount.core import amount


class TestBalance(unittest.TestCase):

    @loaddoc
    def test_simple_error(self, entries, errors, __):
        """
          2013-05-01 open Assets:US:Checking

          2013-05-03 balance Assets:US:Checking   100 USD
        """
        self.assertEqual([balance.BalanceError], list(map(type, errors)))
        entry = entries[1]
        self.assertTrue(isinstance(entry, balance.Balance))
        self.assertEqual(amount.Amount('-100', 'USD'), entry.diff_amount)

    @loaddoc
    def test_simple_first(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Equity:Opening-Balances

          2013-05-03 balance Assets:Bank:Checking   100 USD
        """
        self.assertEqual([], list(map(type, errors)))
        entry = entries[-1]
        self.assertTrue(isinstance(entry, balance.Balance))
        self.assertEqual(None, entry.diff_amount)

    @loaddoc
    def test_simple_cont(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Equity:Opening-Balances

          2013-05-03 balance Assets:Bank:Checking   100 USD

          2013-05-04 *
            Assets:Bank:Checking                 10 USD
            Equity:Opening-Balances

          2013-05-05 balance Assets:Bank:Checking   110 USD
        """
        self.assertEqual([], list(map(type, errors)))

    @loaddoc
    def test_simple_partial_currency_first(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Assets:Bank:Checking                200 CAD
            Equity:Opening-Balances

          2013-05-03 balance Assets:Bank:Checking   100 USD
          2013-05-03 balance Assets:Bank:Checking   200 CAD
        """
        self.assertEqual([], list(map(type, errors)))

    @loaddoc
    def test_simple_partial_currency_cont(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Assets:Bank:Checking                200 CAD
            Equity:Opening-Balances

          2013-05-03 balance Assets:Bank:Checking   100 USD

          2013-05-04 *
            Assets:Bank:Checking                 10 USD
            Assets:Bank:Checking                 20 CAD
            Equity:Opening-Balances

          2013-05-05 balance Assets:Bank:Checking   110 USD
          2013-05-05 balance Assets:Bank:Checking   220 CAD
        """
        self.assertEqual([], list(map(type, errors)))

    @loaddoc
    def test_parents(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank
          2013-05-01 open Assets:Bank:Checking1
          2013-05-01 open Assets:Bank:Checking2
          2013-05-01 open Assets:Bank:Savings   ;; Will go negative
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Checking1                100 USD
            Equity:Opening-Balances

          2013-05-03 *
            Assets:Bank:Checking2                10 USD
            Equity:Opening-Balances

          2013-05-04 *
            Assets:Bank:Savings                 -50 USD
            Equity:Opening-Balances

          2013-05-05 balance Assets:Bank:Checking1  100 USD
          2013-05-05 balance Assets:Bank:Checking2   10 USD
          2013-05-05 balance Assets:Bank:Savings    -50 USD
          2013-05-05 balance Assets:Bank             60 USD
        """
        self.assertEqual([], list(map(type, errors)))
        diff_amounts = [entry.diff_amount
                        for entry in entries
                        if isinstance(entry, balance.Balance)]
        self.assertEqual([None, None, None, None], diff_amounts)

    @loaddoc
    def test_parents_only(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank
          2013-05-01 open Assets:Bank:Checking1
          2013-05-01 open Assets:Bank:Checking2
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Checking1                100 USD
            Equity:Opening-Balances

          2013-05-03 *
            Assets:Bank:Checking2                10 USD
            Equity:Opening-Balances

          2013-05-05 balance Assets:Bank             110 USD
        """
        self.assertEqual([], list(map(type, errors)))
        diff_amounts = [entry.diff_amount
                        for entry in entries
                        if isinstance(entry, balance.Balance)]
        self.assertEqual([None], diff_amounts)

    @loaddoc
    def test_parents_with_postings(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank
          2013-05-01 open Assets:Bank:Checking1
          2013-05-01 open Assets:Bank:Checking2
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Checking1                100 USD
            Equity:Opening-Balances

          2013-05-03 *
            Assets:Bank:Checking2                10 USD
            Equity:Opening-Balances

          2013-05-04 * "Posting on a parent account"
            Assets:Bank                          15 USD
            Equity:Opening-Balances

          2013-05-05 balance Assets:Bank             125 USD
        """
        self.assertFalse(errors)
        diff_amounts = [entry.diff_amount
                        for entry in entries
                        if isinstance(entry, balance.Balance)]
        self.assertEqual([None], diff_amounts)

    @loaddoc
    def test_with_lots(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                1 GOOG {501 USD}
            Equity:Opening-Balances

          2013-05-03 balance Assets:Bank:Investing    1 GOOG
        """
        self.assertFalse(errors)

    # This test ensures that the 'check' directives apply at the beginning of
    # the day.
    @loaddoc
    def test_check_samedate(self, entries, errors, __):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-02 balance Assets:US:Checking     0 USD
          2013-05-03 balance Assets:US:Checking   100 USD
        """
        self.assertEqual([], list(map(type, errors)))

    @loaddoc
    def test_precision(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Income:Interest

          2013-05-02 *
            Assets:Bank:Checking        0.00001 USD
            Income:Interest

          2013-05-03 balance Assets:Bank:Checking   0.00 USD

          2013-05-03 *
            Assets:Bank:Checking        0.00001 USD
            Income:Interest

          2013-05-04 balance Assets:Bank:Checking   0.00 USD

          2013-05-04 *
            Assets:Bank:Checking        0.015 USD
            Income:Interest

          2013-05-05 balance Assets:Bank:Checking   0.01502 USD
        """
        self.assertEqual([], list(map(type, errors)))

    @loaddoc
    def test_balance_before_create(self, entries, errors, __):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-01 balance Assets:US:Checking     0 USD

          2013-05-03 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD
        """
        self.assertEqual([], list(map(type, errors)))

    @loaddoc
    def test_balance_with_prefix_account(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Assets:Bank:CheckingOld
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Equity:Opening-Balances

          2013-05-03 *
            Assets:Bank:CheckingOld              27 USD
            Equity:Opening-Balances

          2013-05-10 balance Assets:Bank:Checking   100 USD
        """
        self.assertEqual([], list(map(type, errors)))


class TestBalancePrecision(unittest.TestCase):

    @loaddoc
    def test_balance_with_prefix_account(self, entries, errors, __):
        """
          2015-05-01 open Assets:Bank:Checking
          2015-05-02 balance Assets:Bank:Checking   0 USD
          2015-05-02 balance Assets:Bank:Checking   0.0 USD
          2015-05-02 balance Assets:Bank:Checking   0.00 USD
          2015-05-02 balance Assets:Bank:Checking   0.000 USD
          2015-05-02 balance Assets:Bank:Checking   1 USD
          2015-05-02 balance Assets:Bank:Checking   1.0 USD
          2015-05-02 balance Assets:Bank:Checking   1.00 USD
          2015-05-02 balance Assets:Bank:Checking   1.000 USD
          2015-05-02 balance Assets:Bank:Checking   1.01 USD
        """
        tolerances = [balance.get_tolerance(entry)
                      for entry in entries[1:]]
        self.assertEqual([Decimal('0'),
                          Decimal('0.1'),
                          Decimal('0.01'),
                          Decimal('0.001'),
                          Decimal('0'),
                          Decimal('0.1'),
                          Decimal('0.01'),
                          Decimal('0.001'),
                          Decimal('0.01')], tolerances)

    @loaddoc
    def test_balance_with_tolerance(self, entries, errors, __):
        """
          option "experiments" "exp-explicit-tolerances"

          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-03 *
            Assets:Bank:Checking              23.024 USD
            Equity:Opening-Balances

          2015-05-02 balance Assets:Bank:Checking   23.022 ~ 0.001 USD
          2015-05-03 balance Assets:Bank:Checking   23.023 ~ 0.001 USD
          2015-05-04 balance Assets:Bank:Checking   23.024 ~ 0.001 USD
          2015-05-05 balance Assets:Bank:Checking   23.025 ~ 0.001 USD
          2015-05-06 balance Assets:Bank:Checking   23.026 ~ 0.001 USD

          2015-05-10 balance Assets:Bank:Checking   23.03 ~ 0.01 USD
        """
        self.assertEqual(2, len(errors))
        self.assertTrue(re.search('23.022', errors[0].message))
        self.assertTrue(re.search('23.026', errors[1].message))
