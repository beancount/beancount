import unittest

from beancount.ops import check
from beancount.ops import validation
from beancount.loader import loaddoc
from beancount.parser import printer
from beancount.core import amount


class TestCheck(unittest.TestCase):

    @loaddoc
    def test_simple_error(self, entries, errors, __):
        """
          2013-05-01 open Assets:US:Checking

          2013-05-03 balance Assets:US:Checking   100 USD
        """
        self.assertEqual([check.BalanceError], list(map(type, errors)))
        entry = entries[1]
        self.assertTrue(isinstance(entry, check.Balance))
        self.assertEqual(amount.Amount('-100', 'USD'), entry.diff_amount)

    @loaddoc
    def test_simple_first(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Equity:OpeningBalances

          2013-05-03 balance Assets:Bank:Checking   100 USD
        """
        self.assertEqual([], list(map(type, errors)))
        entry = entries[-1]
        self.assertTrue(isinstance(entry, check.Balance))
        self.assertEqual(None, entry.diff_amount)

    @loaddoc
    def test_simple_cont(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Equity:OpeningBalances

          2013-05-03 balance Assets:Bank:Checking   100 USD

          2013-05-04 *
            Assets:Bank:Checking                 10 USD
            Equity:OpeningBalances

          2013-05-05 balance Assets:Bank:Checking   110 USD
        """
        self.assertEqual([], list(map(type, errors)))

    @loaddoc
    def test_simple_partial_currency_first(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Assets:Bank:Checking                200 CAD
            Equity:OpeningBalances

          2013-05-03 balance Assets:Bank:Checking   100 USD
          2013-05-03 balance Assets:Bank:Checking   200 CAD
        """
        self.assertEqual([], list(map(type, errors)))

    @loaddoc
    def test_simple_partial_currency_cont(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-02 *
            Assets:Bank:Checking                100 USD
            Assets:Bank:Checking                200 CAD
            Equity:OpeningBalances

          2013-05-03 balance Assets:Bank:Checking   100 USD

          2013-05-04 *
            Assets:Bank:Checking                 10 USD
            Assets:Bank:Checking                 20 CAD
            Equity:OpeningBalances

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
          2013-05-01 open Equity:OpeningBalances

          2013-05-02 *
            Assets:Bank:Checking1                100 USD
            Equity:OpeningBalances

          2013-05-03 *
            Assets:Bank:Checking2                10 USD
            Equity:OpeningBalances

          2013-05-04 *
            Assets:Bank:Savings                 -50 USD
            Equity:OpeningBalances

          2013-05-05 balance Assets:Bank:Checking1  100 USD
          2013-05-05 balance Assets:Bank:Checking2   10 USD
          2013-05-05 balance Assets:Bank:Savings    -50 USD
          2013-05-05 balance Assets:Bank             60 USD
        """
        self.assertEqual([], list(map(type, errors)))
        diff_amounts = [entry.diff_amount
                        for entry in entries
                        if isinstance(entry, check.Balance)]
        self.assertEqual([None, None, None, None], diff_amounts)

    # Note: This may be more appropriate to be moved to the validation module,
    # but this used to be raised from the balance checking routine, which is why
    # it is located here now.
    @loaddoc
    def test_negative_lots(self, entries, errors, __):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:OpeningBalances

          2013-05-02 *
            Assets:Bank:Investing                -1 GOOG {501 USD}
            Equity:OpeningBalances
        """
        self.assertEqual([validation.ValidationError], list(map(type, errors)))

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

          2013-05-03 balance Assets:Bank:Checking   0 USD

          2013-05-03 *
            Assets:Bank:Checking        0.00001 USD
            Income:Interest

          2013-05-04 balance Assets:Bank:Checking   0 USD

          2013-05-04 *
            Assets:Bank:Checking        0.015 USD
            Income:Interest

          2013-05-05 balance Assets:Bank:Checking   0.01502 USD
        """
        self.assertEqual([], list(map(type, errors)))
