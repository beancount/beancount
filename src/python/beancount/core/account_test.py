import unittest

from beancount.core.account import *


class TestAccount(unittest.TestCase):

    def test_account_join(self):
        account_name = join("Expenses", "Toys", "Computer")
        self.assertEqual("Expenses:Toys:Computer", account_name)

        account_name = join("Expenses")
        self.assertEqual("Expenses", account_name)

        account_name = join()
        self.assertEqual("", account_name)

    def test_account_name_parent(self):
        self.assertEqual("Expenses:Toys", account_name_parent("Expenses:Toys:Computer"))
        self.assertEqual("Expenses", account_name_parent("Expenses:Toys"))
        self.assertEqual("", account_name_parent("Expenses"))
        self.assertEqual(None, account_name_parent(""))

    def test_account_name_leaf(self):
        self.assertEqual("Computer", account_name_leaf("Expenses:Toys:Computer"))
        self.assertEqual("Toys", account_name_leaf("Expenses:Toys"))
        self.assertEqual("Expenses", account_name_leaf("Expenses"))
        self.assertEqual(None, account_name_leaf(""))

    def test_account_name_sortkey(self):
        account_names_input = [
            "Expenses:Toys:Computer",
            "Income:US:Intel",
            "Income:US:ETrade:Dividends",
            "Equity:OpeningBalances",
            "Liabilities:US:RBS:MortgageLoan",
            "Equity:NetIncome",
            "Assets:US:RBS:Savings",
            "Assets:US:RBS:Checking"
        ]
        account_names_expected = [
            "Assets:US:RBS:Checking",
            "Assets:US:RBS:Savings",
            "Liabilities:US:RBS:MortgageLoan",
            "Equity:NetIncome",
            "Equity:OpeningBalances",
            "Income:US:ETrade:Dividends",
            "Income:US:Intel",
            "Expenses:Toys:Computer",
        ]

        # Test account_name_sortkey.
        account_names_actual = sorted(account_names_input,
                                      key=account_name_sortkey)
        self.assertEqual(account_names_expected, account_names_actual)

    def test_account_name_type(self):
        self.assertEqual("Assets", account_name_type("Assets:US:RBS:Checking"))
        self.assertEqual("Assets", account_name_type("Assets:US:RBS:Savings"))
        self.assertEqual("Liabilities", account_name_type("Liabilities:US:RBS:MortgageLoan"))
        self.assertEqual("Equity", account_name_type("Equity:NetIncome"))
        self.assertEqual("Equity", account_name_type("Equity:OpeningBalances"))
        self.assertEqual("Income", account_name_type("Income:US:ETrade:Dividends"))
        self.assertEqual("Income", account_name_type("Income:US:Intel"))
        self.assertEqual("Expenses", account_name_type("Expenses:Toys:Computer"))
        with self.assertRaises(AssertionError):
            account_name_type("Invalid:Toys:Computer")

    def test_is_account_name(self):
        self.assertTrue(is_account_name("Assets:US:RBS:Checking"))
        self.assertTrue(is_account_name("Equity:OpeningBalances"))
        self.assertTrue(is_account_name("Income:US:ETrade:Dividends-USD"))
        self.assertTrue(is_account_name("Assets:US:RBS"))
        self.assertTrue(is_account_name("Assets:US"))
        self.assertFalse(is_account_name("Assets"))
        self.assertFalse(is_account_name("Invalid"))
        self.assertFalse(is_account_name("Other"))
        self.assertFalse(is_account_name("Assets:US:RBS*Checking"))
        self.assertFalse(is_account_name("Assets:US:RBS:Checking&"))
        self.assertFalse(is_account_name("Assets:US:RBS:checking"))
        self.assertFalse(is_account_name("Assets:us:RBS:checking"))

    def test_is_account_name_root(self):
        for account_name, expected in [
                ("Assets:US:RBS:Checking", False),
                ("Equity:OpeningBalances", False),
                ("Income:US:ETrade:Dividends-USD", False),
                ("Assets", True),
                ("Liabilities", True),
                ("Equity", True),
                ("Income", True),
                ("Expenses", True),
                ("Invalid", False),
                ]:
            self.assertEqual(expected, is_account_name_root(account_name), account_name)

    OPTIONS = {'name_assets'      : 'Assets',
               'name_liabilities' : 'Liabilities',
               'name_equity'      : 'Equity',
               'name_income'      : 'Income',
               'name_expenses'    : 'Expenses'}

    def test_is_account_categories(self):
        for account_name, expected in [
                ("Assets:US:RBS:Savings", True),
                ("Liabilities:US:RBS:MortgageLoan", True),
                ("Equity:OpeningBalances", True),
                ("Income:US:ETrade:Dividends", False),
                ("Expenses:Toys:Computer", False),
                ]:
            self.assertEqual(expected,
                             is_balance_sheet_account(account_name, self.OPTIONS))

            self.assertEqual(not expected,
                             is_income_statement_account(account_name, self.OPTIONS))

