from beancount.core.tests.realization_test import realizedoc
import unittest


## FIXME: this needs become TestCheck().

class __TestRealization(unittest.TestCase):

    @realizedoc
    def test_check_error(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-03 check Assets:US:Checking   100 USD
        """
        self.assertEqual(len(errors), 1)

    @realizedoc
    def test_check_okay(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-03 check Assets:US:Checking   100 USD

        """
        self.assertEqual(len(errors), 0)

    # This test ensures that the 'check' directives apply at the beginning of
    # the day.
    @realizedoc
    def test_check_samedate(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-02 check Assets:US:Checking     0 USD
          2013-05-03 check Assets:US:Checking   100 USD
        """
        data.print_errors(errors)
        assert len(errors) == 0


# FIXME: We need a test that triggers all the possible kinds of errors that we
# may issue, everywhere actually. That's a great way to start coverage.
