import unittest

from beancount.loader import loaddoc
from beancount.parser import printer


## FIXME: this needs become TestCheck().

class __TestCheck(unittest.TestCase):

    @loaddoc
    def test_check_error(self, entries, _, __):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-03 balance Assets:US:Checking   100 USD
        """
        self.assertEqual(len(errors), 1)

    @loaddoc
    def test_check_okay(self, entries, _, __):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-03 balance Assets:US:Checking   100 USD

        """
        self.assertEqual(len(errors), 0)

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


# FIXME: We need a test that triggers all the possible kinds of errors that we
# may issue, everywhere actually. That's a great way to start coverage.



# FIXME: Check a test for the amounts in the subaccounts.


__incomplete__ = True
