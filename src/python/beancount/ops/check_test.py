import unittest

from beancount.ops import check
from beancount.loader import loaddoc
from beancount.parser import printer


## FIXME: this needs become TestCheck().

class TestCheck(unittest.TestCase):

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
    def test_check_error(self, entries, errors, __):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-03 balance Assets:US:Checking   100 USD
        """
        self.assertEqual([check.BalanceError], list(map(type, errors)))








class _(unittest.TestCase):

    # This test ensures that the 'check' directives apply at the beginning of
    # the day.
    @loaddoc
    def test_check_samedate(self, entries, _, __):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-02 balance Assets:US:Checking     0 USD
          2013-05-03 balance Assets:US:Checking   100 USD
        """
        printer.print_errors(errors)
        assert len(errors) == 0



# FIXME: Check that we only allow negative lots at cost.

# FIXME: Check with precision.



# FIXME: We need a test that triggers all the possible kinds of errors that we
# may issue, everywhere actually. That's a great way to start coverage.



# FIXME: Check a test for the amounts in the subaccounts, check with the parent account.


__incomplete__ = True
