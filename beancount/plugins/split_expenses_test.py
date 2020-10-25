__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import textwrap
import unittest
from os import path

from beancount import loader
from beancount.parser import cmptest
from beancount.plugins import split_expenses
from beancount.utils import test_utils


class TestSplitExpenses(cmptest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_simple(self, entries, errors, __):
        """
            plugin "beancount.plugins.split_expenses" "Martin Caroline"

            2011-05-17 * "Something"
              Expenses:Restaurant   2.00 USD
              Assets:Cash          -2.00 USD
        """
        self.assertEqualEntries("""

            ;; Added automatically by the plugin.
            2011-05-17 open Expenses:Restaurant:Caroline
            2011-05-17 open Expenses:Restaurant:Martin

            2011-05-17 * "Something"
              Expenses:Restaurant:Martin     1.00 USD
              Expenses:Restaurant:Caroline   1.00 USD
              Assets:Cash                   -2.00 USD

        """, entries)

    @loader.load_doc(expect_errors=True)
    def test_unaffected(self, entries, errors, __):
        """
            plugin "beancount.plugins.split_expenses" "Martin Caroline"

            2011-05-17 * "Something"
              Expenses:Restaurant:Martin   2.00 USD
              Assets:Cash                 -2.00 USD
        """
        # Note that this checks that the plugin did not insert any Open
        # directive by itself where not required. This is correct behaviour.
        self.assertEqualEntries(self.test_unaffected.__input__, entries)

    @loader.load_doc()
    def test_work_with_auto_accounts(self, entries, errors, __):
        """
            plugin "beancount.plugins.auto_accounts"
            plugin "beancount.plugins.split_expenses" "Martin Caroline"

            2011-05-17 * "Something"
              Expenses:Restaurant:Martin   2.00 USD
              Assets:Cash                 -2.00 USD
        """
        expected_input = textwrap.dedent("""
            2011-05-17 open Assets:Cash
            2011-05-17 open Expenses:Restaurant:Martin
        """ + self.test_work_with_auto_accounts.__input__)
        self.assertEqualEntries(expected_input, entries)

    @loader.load_doc(expect_errors=True)
    def test_with_one_member_only(self, entries, errors, __):
        """
            plugin "beancount.plugins.split_expenses" "Martin"

            2011-05-17 * "Something"
              Expenses:Restaurant          2.00 USD
              Assets:Cash                 -2.00 USD
        """
        self.assertEqualEntries("""
            2011-05-17 open Expenses:Restaurant:Martin

            2011-05-17 * "Something"
              Expenses:Restaurant:Martin   2.00 USD
              Assets:Cash                 -2.00 USD
        """, entries)

    @loader.load_doc()
    def test_other_directives_copied(self, entries, errors, __):
        """
            plugin "beancount.plugins.split_expenses" "Martin Caroline"

            2011-01-01 open Expenses:Restaurant  USD
            2011-01-01 open Assets:Cash

            2011-05-17 * "Something"
              Expenses:Restaurant   2.00 USD
              Assets:Cash          -2.00 USD

            2011-05-30 balance Expenses:Restaurant  2.00 USD
            2011-06-01 close   Expenses:Restaurant
        """
        self.assertIncludesEntries("""
            2011-01-01 open Expenses:Restaurant  USD
            2011-01-01 open Assets:Cash

            2011-05-30 balance Expenses:Restaurant  2.00 USD
            2011-06-01 close   Expenses:Restaurant
        """, entries)

    @loader.load_doc()
    def test_tolerances__ignore_from_auto_postings(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.split_expenses" "Martin Caroline Sheila"

        option "inferred_tolerance_default" "USD:0.005"

        1970-01-01 open Expenses:Food
        1970-01-01 open Assets:Caroline

        2010-01-01 * "Balances"
          Expenses:Food      -8.00 USD
          Assets:Caroline
        """
        # Interesting case: The Assets leg is filled in with 8.00 USD
        # automatically here, so it is not used in inference. Further forward,
        # the split_expenses plugin splits the first leg as well, and that is
        # also marked as automatic, so if cannot use inference there either. So
        # all legs end up being automatic... and we have to fall back on the
        # default tolerance.


class TestSplitReports(unittest.TestCase):

    def run_split_reports(self, args):
        """Run the split_reports command.

        Args:
          args: A list of extra arguments (beyond the filename).
        """
        rootdir = test_utils.find_repository_root(__file__)
        filename = path.join(rootdir, 'examples', 'sharing', 'duxbury2015.beancount')
        with test_utils.capture() as stdout:
            test_utils.run_with_args(split_expenses.main, args + [filename])
        output = stdout.getvalue()
        self.assertRegex(output, "Participant")
        self.assertRegex(output, "Expenses:Food:Restaurant")
        self.assertRegex(output, "description")
        self.assertRegex(output, "balance")

    def test_split_reports(self):
        self.run_split_reports(['--output-stdout'])

    def test_split_reports_with_currency(self):
        self.run_split_reports(['--output-stdout', '--currency=USD'])


if __name__ == '__main__':
    unittest.main()
