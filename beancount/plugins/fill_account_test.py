__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.parser import cmptest
from beancount.plugins import fill_account
from beancount import loader


class TestFillAccountOpen(cmptest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_fill_account_invalid_account(self, entries, errors, _):
        """
        plugin "beancount.plugins.fill_account" "Assets:Not_An_Account_Name"

        2001-01-01 open Assets:Cash
        2001-01-01 open Liabilities:CreditCard
        2001-01-01 open Expenses:Restaurant
        """
        self.assertRegex(errors[0].message, "Invalid account name")

    @loader.load_doc()
    def test_fill_account(self, entries, _, options_map):
        """
        plugin "beancount.plugins.fill_account" "Assets:Cash"

        2001-01-01 open Assets:Cash
        2001-01-01 open Liabilities:CreditCard
        2001-01-01 open Expenses:Restaurant

        2014-02-01 *
          Expenses:Restaurant       100 USD

        2014-03-11 *
          Expenses:Restaurant       100 USD
          Liabilities:CreditCard
        """
        new_entries, _ = fill_account.fill_account(entries, options_map,
                                                   "Assets:Cash")

        self.assertEqualEntries("""

        2001-01-01 open Assets:Cash
        2001-01-01 open Liabilities:CreditCard
        2001-01-01 open Expenses:Restaurant

        2014-02-01 *
          Expenses:Restaurant       100 USD
          Assets:Cash              -100 USD

        2014-03-11 *
          Expenses:Restaurant       100 USD
          Liabilities:CreditCard   -100 USD

        """, new_entries)

    @loader.load_doc()
    def test_fill_account_with_cost(self, entries, _, options_map):
        """
        plugin "beancount.plugins.fill_account" "Assets:Cash"

        2001-01-01 open Assets:Cash
        2001-01-01 open Liabilities:CreditCard
        2001-01-01 open Expenses:Restaurant

        2014-02-01 *
          Expenses:Restaurant       100 MSFT {101.23 USD}
        """
        new_entries, _ = fill_account.fill_account(entries, options_map,
                                                   "Assets:Cash")

        self.assertEqualEntries("""

        2001-01-01 open Assets:Cash
        2001-01-01 open Liabilities:CreditCard
        2001-01-01 open Expenses:Restaurant

        2014-02-01 *
          Expenses:Restaurant       100 MSFT {101.23 USD}
          Assets:Cash         -10123.00 USD

        """, new_entries)


if __name__ == '__main__':
    unittest.main()
