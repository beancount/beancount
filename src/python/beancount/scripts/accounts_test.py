import sys

from beancount.scripts import TestCase, docfile, capture, run_with_args
from beancount.scripts import accounts


class TestScriptAccounts(TestCase):

    @docfile
    def test_invocation(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with capture() as stdout:
            run_with_args(accounts.main, [filename])

        r = self.assertLines("""
            Assets:Cash          2013-01-01
            Expenses:Restaurant  2013-01-01
        """, stdout.getvalue())
