import unittest
import tempfile
import os
import shutil
from os import path

from . import account


class TestAccount(unittest.TestCase):

    def test_is_valid(self):
        self.assertTrue(account.is_valid("Assets:US:RBS:Checking"))
        self.assertTrue(account.is_valid("Equity:OpeningBalances"))
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
        account_name = account.join("Expenses", "Toys", "Computer")
        self.assertEqual("Expenses:Toys:Computer", account_name)

        account_name = account.join("Expenses")
        self.assertEqual("Expenses", account_name)

        account_name = account.join()
        self.assertEqual("", account_name)

    def test_parent(self):
        self.assertEqual("Expenses:Toys",
                         account.parent("Expenses:Toys:Computer"))
        self.assertEqual("Expenses", account.parent("Expenses:Toys"))
        self.assertEqual("", account.parent("Expenses"))
        self.assertEqual(None, account.parent(""))

    def test_account_name_leaf(self):
        self.assertEqual("Computer", account.account_name_leaf("Expenses:Toys:Computer"))
        self.assertEqual("Toys", account.account_name_leaf("Expenses:Toys"))
        self.assertEqual("Expenses", account.account_name_leaf("Expenses"))
        self.assertEqual(None, account.account_name_leaf(""))

    def test_account_sans_root(self):
        self.assertEqual("Toys:Computer",
                         account.account_name_sans_root("Expenses:Toys:Computer"))
        self.assertEqual("US:BofA:Checking",
                         account.account_name_sans_root("Assets:US:BofA:Checking"))
        self.assertEqual("", account.account_name_sans_root("Assets"))

    def test_has_component(self):
        self.assertTrue(account.has_component('Liabilities:US:Credit-Card', 'US'))
        self.assertFalse(account.has_component('Liabilities:US:Credit-Card', 'CA'))
        self.assertTrue(account.has_component('Liabilities:US:Credit-Card', 'Credit-Card'))
        self.assertTrue(account.has_component('Liabilities:US:Credit-Card', 'Liabilities'))
        self.assertFalse(account.has_component('Liabilities:US:Credit-Card', 'Credit'))
        self.assertFalse(account.has_component('Liabilities:US:Credit-Card', 'Card'))

    def test_commonprefix(self):
        self.assertEqual('Assets:US:TD',
                         account.commonprefix(['Assets:US:TD:Checking',
                                               'Assets:US:TD:Savings']))
        self.assertEqual('Assets:US',
                         account.commonprefix(['Assets:US:TD:Checking',
                                               'Assets:US:BofA:Checking']))
        self.assertEqual('Assets',
                         account.commonprefix(['Assets:US:TD:Checking',
                                               'Assets:CA:RBC:Savings']))
        self.assertEqual('',
                         account.commonprefix(['Assets:US:TD:Checking',
                                               'Liabilities:US:CreditCard']))
        self.assertEqual('',
                         account.commonprefix(['']))


def create_file_hierarchy(test_files, subdir='root'):
    """Create a hierarchy of files.

    Args:
      test_files: A list of strings, relative filenames to a temporary root
        directory. If the filename ends with a '/', we create a directory;
        otherwise, we create a regular file.
      subdir: A string, the subdirectory name under the temporary directory
        location, to create the hierarchy under.
    Returns:
      A pair of strings, the temporary directory, and the subdirectory under
        that which hosts the root of the tree.
    """
    tempdir = tempfile.mkdtemp(prefix="beancount-test-tmpdir.")
    root = path.join(tempdir, subdir)
    for filename in test_files:
        abs_filename = path.join(tempdir, filename)
        if filename.endswith('/'):
            os.makedirs(abs_filename)
        else:
            parent_dir = path.dirname(abs_filename)
            if not path.exists(parent_dir):
                os.makedirs(parent_dir)
            open(abs_filename, 'w')
    return tempdir, root


class TestWalk(unittest.TestCase):

    test_documents = [
        'root/Assets/US/Bank/Checking/other.txt',
        'root/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf',
        'root/Assets/US/Bank/Checking/otherdir/',
        'root/Assets/US/Bank/Checking/otherdir/another.txt',
        'root/Assets/US/Bank/Checking/otherdir/2014-06-08.bank-statement.pdf',
        'root/Assets/US/Bank/Savings/2014-07-01.savings.pdf',
        'root/Liabilities/US/Bank/',  # Empty directory.
    ]

    def setUp(self):
        self.tempdir, self.root = create_file_hierarchy(self.test_documents)

    def tearDown(self):
        shutil.rmtree(self.tempdir, ignore_errors=True)

    def test_walk(self):
        actual_data = [
            (root[len(self.root):], account, dirs, files)
            for root, account, dirs, files in account.walk(self.root)]

        self.assertEqual([
            ('/Assets/US', 'Assets:US',
             ['Bank'],
             []),
            ('/Assets/US/Bank', 'Assets:US:Bank',
             ['Checking', 'Savings'],
             []),
            ('/Assets/US/Bank/Checking', 'Assets:US:Bank:Checking',
             ['otherdir'],
             ['2014-06-08.bank-statement.pdf', 'other.txt']),

            ('/Assets/US/Bank/Savings', 'Assets:US:Bank:Savings',
             [],
             ['2014-07-01.savings.pdf']),

            ('/Liabilities/US', 'Liabilities:US',
             ['Bank'],
             []),
            ('/Liabilities/US/Bank', 'Liabilities:US:Bank',
             [],
             []),
            ], actual_data)
