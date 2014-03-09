import sys
import re

from beancount.scripts import TestCase, docfile, capture, run_with_args
from beancount.scripts import trial


class TestScriptTrial(TestCase):

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
            run_with_args(trial.main, [filename])
        output = stdout.getvalue()
        self.assertLines("""
            |-- Assets
            |   `-- Cash               -50.02 USD
            `-- Expenses
                `-- Restaurant          50.02 USD
            """, output)
