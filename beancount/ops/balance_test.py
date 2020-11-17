__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core.number import D
from beancount.core.amount import A
from beancount.ops import balance
from beancount import loader


class TestBalance(unittest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_simple_error(self, entries, errors, __):
        """
          2013-05-01 open Assets:US:Checking

          2013-05-03 balance Assets:US:Checking   100 USD
        """
        self.assertEqual([balance.BalanceError], list(map(type, errors)))
        entry = entries[1]
        self.assertTrue(isinstance(entry, balance.Balance))
        self.assertEqual(A('-100 USD'), entry.diff_amount)

    @loader.load_doc()
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

    @loader.load_doc()
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

    @loader.load_doc()
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

    @loader.load_doc()
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

    @loader.load_doc(expect_errors=True)
    def test_simple_invalid_currency(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking      CAD
          2013-05-01 open Equity:Opening-Balances

          ;; This ought to fail because USD isn't a valid currency.
          2013-05-03 balance Assets:Bank:Checking   0.00 USD
        """
        self.assertEqual([balance.BalanceError], list(map(type, errors)))

    @loader.load_doc()
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

    @loader.load_doc()
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

    @loader.load_doc()
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

    @loader.load_doc()
    def test_with_lots(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                1 HOOL {501 USD}
            Equity:Opening-Balances

          2013-05-03 balance Assets:Bank:Investing    1 HOOL
        """
        self.assertFalse(errors)

    # This test ensures that the 'check' directives apply at the beginning of
    # the day.
    @loader.load_doc()
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

    @loader.load_doc()
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

    @loader.load_doc()
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

    @loader.load_doc()
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


    @loader.load_doc()
    def test_balance_mixed_cost_and_no_cost(self, entries, errors, __):
        """
          2013-05-01 open Assets:Invest
          2013-05-01 open Equity:Opening-Balances

          2013-05-01 *
            Assets:Invest                100 HOOL {14.33 USD}
            Equity:Opening-Balances

          2013-05-02 *
            Assets:Invest               -100 HOOL @ 15.66 USD
            Equity:Opening-Balances

          2013-05-10 balance Assets:Invest   0 HOOL
        """
        self.assertEqual([], list(map(type, errors)))

    @loader.load_doc(expect_errors=True)
    def test_balance_account_does_not_exist(self, entries, errors, __):
        """
          2013-05-01 open Assets:Invest
          2013-05-01 open Equity:Opening-Balances

          2013-05-10 balance Assets:Invest:Invalid   0 HOOL
        """
        self.assertEqual([balance.BalanceError], list(map(type, errors)))


class TestBalancePrecision(unittest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_get_balance_tolerance__explicit(self, entries, errors, options_map):
        """
          2015-05-01 open Assets:Bank:Checking
          2015-05-02 balance Assets:Bank:Checking   0    ~ 0.002 USD
          2015-05-02 balance Assets:Bank:Checking   0.0  ~ 0.002 USD
          2015-05-02 balance Assets:Bank:Checking   0.00 ~ 0.002 USD
          2015-05-02 balance Assets:Bank:Checking   1    ~ 0.002 USD
          2015-05-02 balance Assets:Bank:Checking   1.0  ~ 0.002 USD
          2015-05-02 balance Assets:Bank:Checking   1.00 ~ 0.002 USD
        """
        tolerances = [balance.get_balance_tolerance(entry, options_map)
                      for entry in entries[1:]]
        self.assertEqual([D('0.002')] * 6, tolerances)

    @loader.load_doc(expect_errors=True)
    def test_get_balance_tolerance__regular(self, entries, errors, options_map):
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
        tolerances = [balance.get_balance_tolerance(entry, options_map)
                      for entry in entries[1:]]
        self.assertEqual([D('0'),
                          D('0.1'),
                          D('0.01'),
                          D('0.001'),
                          D('0'),
                          D('0.1'),
                          D('0.01'),
                          D('0.001'),
                          D('0.01')], tolerances)

    @loader.load_doc(expect_errors=True)
    def test_balance_with_tolerance(self, entries, errors, __):
        """
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
        self.assertRegex(errors[0].message, '23.022')
        self.assertRegex(errors[1].message, '23.026')


if __name__ == '__main__':
    unittest.main()
