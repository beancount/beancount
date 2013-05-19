"""
Unit tests for realizations.
"""

import unittest
from textwrap import dedent

from beancount2 import parser
from beancount2 import checks
from beancount2 import realization


class TestRealization(unittest.TestCase):

    def parsetest_check_error(self, contents):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-03 check Assets:US:Checking   100 USD
        """
        errors = checks.check(contents.entries, contents.accounts)
        self.assertFalse(errors)

        real_accounts, real_errors = realization.realize(contents.entries, True)
        assert len(real_errors) == 1

    def parsetest_check_okay(self, contents):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-03 check Assets:US:Checking   100 USD

        """
        errors = checks.check(contents.entries, contents.accounts)
        self.assertFalse(errors)

        real_accounts, real_errors = realization.realize(contents.entries, True)
        print(real_errors)
        assert len(real_errors) == 1

    # def parsetest_check_samedate(self, contents):
    #     pass

parser.create_parsetest_methods(TestRealization)
