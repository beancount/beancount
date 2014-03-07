import sys
import re

from beancount.scripts.scripts_test_support import TestCase, docfile, capture, run_with_args
from beancount.scripts import check


class TestScriptCheck(TestCase):

    @docfile
    def test_success(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with capture() as stdout:
            run_with_args(check.main, [filename])
        r = self.assertLines("", stdout.getvalue())

    @docfile
    def test_fail(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash

        2014-03-07 balance Assets:Cash  100 USD
        """
        with capture() as stdout:
            run_with_args(check.main, [filename])
        self.assertTrue(re.search("Balance failed", stdout.getvalue()))
        self.assertTrue(re.search("Assets:Cash", stdout.getvalue()))
