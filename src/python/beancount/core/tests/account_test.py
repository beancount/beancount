import unittest
import re

from beancount.core.account import *

class TestAccount(unittest.TestCase):

    def test_ctor(self):
        account = Account("Expenses:Toys:Computer", 'Expenses')
        self.assertEqual("Expenses:Toys:Computer", account.name)
        self.assertEqual("Expenses", account.type)

    def test_account_from_name(self):
        account = account_from_name("Expenses:Toys:Computer")
        self.assertEqual("Expenses:Toys:Computer", account.name)
        self.assertEqual("Expenses", account.type)

    def test_account_parent_name(self):
        self.assertEqual("Expenses:Toys", account_parent_name("Expenses:Toys:Computer"))
        self.assertEqual("Expenses", account_parent_name("Expenses:Toys"))
        self.assertEqual("", account_parent_name("Expenses"))
        self.assertEqual(None, account_parent_name(""))

    def test_account_leaf_name(self):
        self.assertEqual("Computer", account_leaf_name("Expenses:Toys:Computer"))
        self.assertEqual("Toys", account_leaf_name("Expenses:Toys"))
        self.assertEqual("Expenses", account_leaf_name("Expenses"))
        self.assertEqual(None, account_leaf_name(""))

    def test_account_sortkey(self):
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

        # Test account_names_sortkey.
        account_names_actual = sorted(account_names_input,
                                      key=account_names_sortkey)
        self.assertEqual(account_names_expected, account_names_actual)

        # Test account_sortkey.
        accounts_input = map(account_from_name, account_names_input)
        accounts_actual = sorted(accounts_input,
                                 key=account_sortkey)
        self.assertEqual(account_names_expected,
                         [account.name for account in accounts_actual])
        accounts_expected = list(map(account_from_name, account_names_expected))
        self.assertEqual(accounts_expected, accounts_actual)

    def test_account_type(self):
        self.assertEqual("Assets", account_type("Assets:US:RBS:Checking"))
        self.assertEqual("Assets", account_type("Assets:US:RBS:Savings"))
        self.assertEqual("Liabilities", account_type("Liabilities:US:RBS:MortgageLoan"))
        self.assertEqual("Equity", account_type("Equity:NetIncome"))
        self.assertEqual("Equity", account_type("Equity:OpeningBalances"))
        self.assertEqual("Income", account_type("Income:US:ETrade:Dividends"))
        self.assertEqual("Income", account_type("Income:US:Intel"))
        self.assertEqual("Expenses", account_type("Expenses:Toys:Computer"))
        self.assertRaises(AssertionError, account_type, "Invalid:Toys:Computer")

    def test_is_account_name(self):
        is_account_name

    def test_is_account_root(self):
        is_account_root

    def test_is_balance_sheet_account(self):
        is_balance_sheet_account

    def test_is_balance_sheet_account_name(self):
        is_balance_sheet_account_name

    def test_is_income_statement_account(self):
        is_income_statement_account

    def test_accountify_dict(self):
        accountify_dict



unittest.main()
