__author__ = "Martin Blais <blais@furius.ca>"

import textwrap

from beancount import loader
from beancount.parser import cmptest


class TestSplitExpenses(cmptest.TestCase):

    @loader.loaddoc(expect_errors=True)
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

    @loader.loaddoc(expect_errors=True)
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

    @loader.loaddoc()
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
        """) + self.test_work_with_auto_accounts.__input__
        self.assertEqualEntries(expected_input, entries)

    @loader.loaddoc(expect_errors=True)
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

    @loader.loaddoc()
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
