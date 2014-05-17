import unittest

from beancount.core import account_types


class TestAccountGlobals(unittest.TestCase):

    def test_reset_globals(self):
        account_types.update_valid_account_names()

    def test_basics(self):
        self.assertEqual(5, len(account_types.DEFAULT_ACCOUNT_TYPES))
        self.assertTrue(account_types.ACCOUNT_TYPES is not None)
        self.assertTrue(account_types.TYPES_ORDER is not None)

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
                                      key=account_types.account_name_sortkey)
        self.assertEqual(account_names_expected, account_names_actual)

    def test_account_name_type(self):
        self.assertEqual("Assets", account_types.account_name_type("Assets:US:RBS:Checking"))
        self.assertEqual("Assets", account_types.account_name_type("Assets:US:RBS:Savings"))
        self.assertEqual("Liabilities",
                         account_types.account_name_type("Liabilities:US:RBS:MortgageLoan"))
        self.assertEqual("Equity", account_types.account_name_type("Equity:NetIncome"))
        self.assertEqual("Equity", account_types.account_name_type("Equity:OpeningBalances"))
        self.assertEqual("Income", account_types.account_name_type("Income:US:ETrade:Dividends"))
        self.assertEqual("Income", account_types.account_name_type("Income:US:Intel"))
        self.assertEqual("Expenses", account_types.account_name_type("Expenses:Toys:Computer"))
        with self.assertRaises(AssertionError):
            account_types.account_name_type("Invalid:Toys:Computer")

    def test_is_account_name(self):
        self.assertTrue(account_types.is_account_name("Assets:US:RBS:Checking"))
        self.assertTrue(account_types.is_account_name("Equity:OpeningBalances"))
        self.assertTrue(account_types.is_account_name("Income:US:ETrade:Dividends-USD"))
        self.assertTrue(account_types.is_account_name("Assets:US:RBS"))
        self.assertTrue(account_types.is_account_name("Assets:US"))
        self.assertFalse(account_types.is_account_name("Assets"))
        self.assertFalse(account_types.is_account_name("Invalid"))
        self.assertFalse(account_types.is_account_name("Other"))
        self.assertFalse(account_types.is_account_name("Assets:US:RBS*Checking"))
        self.assertFalse(account_types.is_account_name("Assets:US:RBS:Checking&"))
        self.assertFalse(account_types.is_account_name("Assets:US:RBS:checking"))
        self.assertFalse(account_types.is_account_name("Assets:us:RBS:checking"))

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
            self.assertEqual(expected,
                             account_types.is_account_name_root(account_name))

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
                             account_types.is_balance_sheet_account(account_name, self.OPTIONS))

            self.assertEqual(not expected,
                             account_types.is_income_statement_account(account_name, self.OPTIONS))


# The main file needs a fair bit of cleanup, simplification, mark this as incomplete again.
__incomplete__ = True
