__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from beancount import loader
from beancount.plugins import check_commodity
from beancount.parser import printer
from beancount.parser import cmptest


class TestSplitExpenses(cmptest.TestCase):

    @loader.loaddoc
    def test_simple(self, entries, errors, __):
        """
            plugin "beancount.ops.auto_accounts"
            plugin "beancount.plugins.split_expenses" "Martin Caroline"

            2011-05-17 * "Something"
              Expenses:Restaurant   2.00 USD
              Assets:Cash          -2.00 USD
        """
        self.assertEqualEntries("""
            2011-05-17 * "Something"
              Expenses:Restaurant:Martin     1.00 USD
              Expenses:Restaurant:Caroline   1.00 USD
              Assets:Cash                   -2.00 USD
        """, [entries[-1]])

    @loader.loaddoc
    def test_unaffected(self, entries, errors, __):
        """
            plugin "beancount.ops.auto_accounts"
            plugin "beancount.plugins.split_expenses" "Martin Caroline"

            2011-05-17 * "Something"
              Expenses:Restaurant:Martin   2.00 USD
              Assets:Cash                 -2.00 USD
        """
        self.assertEqualEntries("""
            2011-05-17 * "Something"
              Expenses:Restaurant:Martin   2.00 USD
              Assets:Cash                 -2.00 USD
        """, [entries[-1]])
