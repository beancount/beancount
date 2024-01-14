__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import types

from beancount.core import account
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
        # CJK character set "Lu_and_CJK" test start
        self.assertFalse(account.is_valid("资产"))
        self.assertFalse(account.is_valid("Assets"))
        self.assertFalse(account.is_valid("资产:银行存款"))
        self.assertTrue(account.is_valid("Assets:现金"))
        self.assertTrue(account.is_valid("Assets:现金:CNY"))
        self.assertTrue(account.is_valid("Assets:现金:US"))
        self.assertTrue(account.is_valid("Assets:银行存款"))
        self.assertTrue(account.is_valid("Assets:银行存款:招商银行:1254"))

    def test_account_join(self):
        account_name = account.join("Expenses", "Toys", "Computer")
        self.assertEqual("Expenses:Toys:Computer", account_name)

        account_name = account.join("Expenses", "餐饮", "早餐")
        self.assertEqual("Expenses:餐饮:早餐", account_name)

        account_name = account.join("Expenses")
        self.assertEqual("Expenses", account_name)

        account_name = account.join()
        self.assertEqual("", account_name)

    def test_account_split(self):
        account_name = account.split("Expenses:Toys:Computer")
        self.assertEqual(["Expenses", "Toys", "Computer"], account_name)

        account_name = account.split("Expenses:餐饮:早餐")
        self.assertEqual(["Expenses", "餐饮", "早餐"], account_name)

        account_name = account.split("Expenses")
        self.assertEqual(["Expenses"], account_name)

        account_name = account.split("")
        self.assertEqual([""], account_name)

    def test_parent(self):
        self.assertEqual("Expenses:Toys", account.parent(
            "Expenses:Toys:Computer"))
        self.assertEqual("Expenses", account.parent("Expenses:Toys"))
        self.assertEqual("", account.parent("Expenses"))
        self.assertEqual(None, account.parent(""))
        self.assertEqual("Expenses:餐饮", account.parent("Expenses:餐饮:早餐"))
        self.assertEqual("Expenses", account.parent("Expenses:餐饮"))

    def test_leaf(self):
        self.assertEqual("Computer", account.leaf("Expenses:Toys:Computer"))
        self.assertEqual("Toys", account.leaf("Expenses:Toys"))
        self.assertEqual("Expenses", account.leaf("Expenses"))
        self.assertEqual(None, account.leaf(""))
        self.assertEqual("早餐", account.leaf("Expenses:餐饮:早餐"))
        self.assertEqual("餐饮", account.leaf("Expenses:餐饮"))

    def test_sans_root(self):
        self.assertEqual("Toys:Computer", account.sans_root(
            "Expenses:Toys:Computer"))
        self.assertEqual(
            "US:BofA:Checking", account.sans_root("Assets:US:BofA:Checking")
        )
        self.assertEqual("", account.sans_root("Assets"))
        self.assertEqual("餐饮:早餐", account.sans_root("Expenses:餐饮:早餐"))

    def test_root(self):
        name = "Liabilities:US:Credit-Card:Blue"
        self.assertEqual("", account.root(0, name))
        self.assertEqual("Liabilities", account.root(1, name))
        self.assertEqual("Liabilities:US", account.root(2, name))
        self.assertEqual("Liabilities:US:Credit-Card", account.root(3, name))
        self.assertEqual("Liabilities:US:Credit-Card:Blue",
                         account.root(4, name))
        self.assertEqual("Liabilities:US:Credit-Card:Blue",
                         account.root(5, name))
        name = "Liabilities:人民币:信用卡:Blue"
        self.assertEqual("", account.root(0, name))
        self.assertEqual("Liabilities", account.root(1, name))
        self.assertEqual("Liabilities:人民币", account.root(2, name))
        self.assertEqual("Liabilities:人民币:信用卡", account.root(3, name))
        self.assertEqual("Liabilities:人民币:信用卡:Blue", account.root(4, name))
        self.assertEqual("Liabilities:人民币:信用卡:Blue", account.root(5, name))

    def test_has_component(self):
        self.assertTrue(account.has_component(
            'Liabilities:US:Credit-Card', 'US'))
        self.assertFalse(account.has_component(
            'Liabilities:US:Credit-Card', 'CA'))
        self.assertTrue(account.has_component(
            'Liabilities:US:Credit-Card', 'Credit-Card'))
        self.assertTrue(account.has_component(
            'Liabilities:US:Credit-Card', 'Liabilities'))
        self.assertFalse(account.has_component(
            'Liabilities:US:Credit-Card', 'Credit'))
        self.assertFalse(account.has_component(
            'Liabilities:US:Credit-Card', 'Card'))

    def test_commonprefix(self):
        self.assertEqual(
            "Assets:US:TD",
            account.commonprefix(
                ["Assets:US:TD:Checking", "Assets:US:TD:Savings"]),
        )
        self.assertEqual(
            "Assets:US",
            account.commonprefix(
                ["Assets:US:TD:Checking", "Assets:US:BofA:Checking"]),
        )
        self.assertEqual(
            "Assets",
            account.commonprefix(
                ["Assets:US:TD:Checking", "Assets:CA:RBC:Savings"]),
        )
        self.assertEqual(
            "",
            account.commonprefix(
                ["Assets:US:TD:Checking", "Liabilities:US:CreditCard"]
            ),
        )
        self.assertEqual("", account.commonprefix([""]))

    def test_parent_matcher(self):
        is_child = account.parent_matcher("Assets:Bank:Checking")
        self.assertTrue(is_child("Assets:Bank:Checking"))
        self.assertTrue(is_child("Assets:Bank:Checking:SubAccount"))
        self.assertFalse(is_child("Assets:Bank:CheckingOld"))
        self.assertFalse(is_child("Assets:Bank:Checking-Old"))

    def test_parents(self):
        iterator = account.parents("Assets:Bank:Checking")
        self.assertIsInstance(iterator, types.GeneratorType)
        self.assertEqual(
            ["Assets:Bank:Checking", "Assets:Bank", "Assets"], list(iterator)
        )


class TestWalk(test_utils.TmpFilesTestBase):

    TEST_DOCUMENTS = [
        "root/Assets/US/Bank/Checking/other.txt",
        "root/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf",
        "root/Assets/US/Bank/Checking/otherdir/",
        "root/Assets/US/Bank/Checking/otherdir/another.txt",
        "root/Assets/US/Bank/Checking/otherdir/2014-06-08.bank-statement.pdf",
        "root/Assets/US/Bank/Savings/2014-07-01.savings.pdf",
        "root/Liabilities/US/Bank/",  # Empty directory.
    ]

    def test_walk(self):
        actual_data = [
            (root[len(self.root):], account_, dirs, files)
            for root, account_, dirs, files in account.walk(self.root)
        ]

        self.assertEqual(
            [
                ("/Assets/US", "Assets:US", ["Bank"], []),
                ("/Assets/US/Bank", "Assets:US:Bank",
                 ["Checking", "Savings"], []),
                (
                    "/Assets/US/Bank/Checking",
                    "Assets:US:Bank:Checking",
                    ["otherdir"],
                    ["2014-06-08.bank-statement.pdf", "other.txt"],
                ),
                (
                    "/Assets/US/Bank/Savings",
                    "Assets:US:Bank:Savings",
                    [],
                    ["2014-07-01.savings.pdf"],
                ),
                ("/Liabilities/US", "Liabilities:US", ["Bank"], []),
                ("/Liabilities/US/Bank", "Liabilities:US:Bank", [], []),
            ],
            actual_data,
        )


class TestAccountTransformer(unittest.TestCase):
    def test_render(self):
        xfr = account.AccountTransformer("__")
        self.assertEqual(
            "Assets__US__BofA__Checking", xfr.render("Assets:US:BofA:Checking")
        )

    def test_parse(self):
        xfr = account.AccountTransformer("__")
        self.assertEqual(
            "Assets:US:BofA:Checking", xfr.parse("Assets__US__BofA__Checking")
        )

    def test_noop(self):
        xfr = account.AccountTransformer()
        acc = "Assets:US:BofA:Checking"
        self.assertEqual(acc, xfr.render(acc))
        self.assertEqual(acc, xfr.parse(acc))


if __name__ == "__main__":
    unittest.main()
