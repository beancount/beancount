__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import types

# TODO(blais): Fix this, perhaps have a module per.
from beancount.ccore import extmodule as account
from beancount.utils import test_utils


class TestAccount(unittest.TestCase):

    def test_is_valid(self):
        self.assertTrue(account.is_valid("Assets:US:RBS:Checking"))
        self.assertTrue(account.is_valid("Equity:Opening-Balances"))
        self.assertTrue(account.is_valid("Income:US:ETrade:Dividends-USD"))
        self.assertTrue(account.is_valid("Assets:US:RBS"))
        self.assertTrue(account.is_valid("Assets:US"))
        self.assertFalse(account.is_valid("Assets"))
        self.assertFalse(account.is_valid("Invalid"))
        self.assertFalse(account.is_valid("Other"))
        self.assertFalse(account.is_valid("Assets:US:RBS*Checking"))
        self.assertFalse(account.is_valid("Assets:US:RBS:Checking&"))
        self.assertFalse(account.is_valid("Assets:US:RBS:checking"))
        self.assertFalse(account.is_valid("Assets:us:RBS:checking"))

    def test_account_join(self):
        account_name = account.join(["Expenses", "Toys", "Computer"])
        self.assertEqual("Expenses:Toys:Computer", account_name)

        account_name = account.join(["Expenses"])
        self.assertEqual("Expenses", account_name)

        account_name = account.join([])
        self.assertEqual("", account_name)

    def test_account_split(self):
        account_name = account.split("Expenses:Toys:Computer")
        self.assertEqual(["Expenses", "Toys", "Computer"], account_name)

        account_name = account.split("Expenses")
        self.assertEqual(["Expenses"], account_name)

        account_name = account.split("")
        # TODO(blais): Fix this.
        self.assertEqual([""], account_name)


if __name__ == '__main__':
    unittest.main()
