__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import tempfile
import os
import shutil
import types
from os import path

from beancount.core import account


# Note: This should live in beancount.utils.test_utils.
class TmpFilesTestBase(unittest.TestCase):
    """A test utility base class that creates and cleans up a directory hierarchy.
    This convenience is useful for testing functions that work on files, such as the
    documents tests, or the accounts walk.
    """

    # The list of strings, documents to create.
    # Filenames ending with a '/' will be created as directories.
    TEST_DOCUMENTS = None

    def setUp(self):
        self.tempdir, self.root = self.create_file_hierarchy(self.TEST_DOCUMENTS)

    def tearDown(self):
        shutil.rmtree(self.tempdir, ignore_errors=True)

    @staticmethod
    def create_file_hierarchy(test_files, subdir='root'):
        """A test utility that creates a hierarchy of files.

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
                with open(abs_filename, 'w'): pass
        return tempdir, root


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
        account_name = account.join("Expenses", "Toys", "Computer")
        self.assertEqual("Expenses:Toys:Computer", account_name)

        account_name = account.join("Expenses")
        self.assertEqual("Expenses", account_name)

        account_name = account.join()
        self.assertEqual("", account_name)

    def test_account_split(self):
        account_name = account.split("Expenses:Toys:Computer")
        self.assertEqual(["Expenses", "Toys", "Computer"], account_name)

        account_name = account.split("Expenses")
        self.assertEqual(["Expenses"], account_name)

        account_name = account.split("")
        self.assertEqual([""], account_name)

    def test_parent(self):
        self.assertEqual("Expenses:Toys",
                         account.parent("Expenses:Toys:Computer"))
        self.assertEqual("Expenses", account.parent("Expenses:Toys"))
        self.assertEqual("", account.parent("Expenses"))
        self.assertEqual(None, account.parent(""))

    def test_leaf(self):
        self.assertEqual("Computer", account.leaf("Expenses:Toys:Computer"))
        self.assertEqual("Toys", account.leaf("Expenses:Toys"))
        self.assertEqual("Expenses", account.leaf("Expenses"))
        self.assertEqual(None, account.leaf(""))

    def test_sans_root(self):
        self.assertEqual("Toys:Computer",
                         account.sans_root("Expenses:Toys:Computer"))
        self.assertEqual("US:BofA:Checking",
                         account.sans_root("Assets:US:BofA:Checking"))
        self.assertEqual("", account.sans_root("Assets"))

    def test_root(self):
        name = "Liabilities:US:Credit-Card:Blue"
        self.assertEqual("", account.root(0, name))
        self.assertEqual("Liabilities", account.root(1, name))
        self.assertEqual("Liabilities:US", account.root(2, name))
        self.assertEqual("Liabilities:US:Credit-Card", account.root(3, name))
        self.assertEqual("Liabilities:US:Credit-Card:Blue", account.root(4, name))
        self.assertEqual("Liabilities:US:Credit-Card:Blue", account.root(5, name))

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

    def test_parent_matcher(self):
        is_child = account.parent_matcher('Assets:Bank:Checking')
        self.assertTrue(is_child('Assets:Bank:Checking'))
        self.assertTrue(is_child('Assets:Bank:Checking:SubAccount'))
        self.assertFalse(is_child('Assets:Bank:CheckingOld'))

    def test_parents(self):
        iterator = account.parents('Assets:Bank:Checking')
        self.assertIsInstance(iterator, types.GeneratorType)
        self.assertEqual(['Assets:Bank:Checking', 'Assets:Bank', 'Assets'],
                         list(iterator))



class TestWalk(TmpFilesTestBase):

    TEST_DOCUMENTS = [
        'root/Assets/US/Bank/Checking/other.txt',
        'root/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf',
        'root/Assets/US/Bank/Checking/otherdir/',
        'root/Assets/US/Bank/Checking/otherdir/another.txt',
        'root/Assets/US/Bank/Checking/otherdir/2014-06-08.bank-statement.pdf',
        'root/Assets/US/Bank/Savings/2014-07-01.savings.pdf',
        'root/Liabilities/US/Bank/',  # Empty directory.
    ]

    def test_walk(self):
        actual_data = [
            (root[len(self.root):], account_, dirs, files)
            for root, account_, dirs, files in account.walk(self.root)]

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


class TestAccountTransformer(unittest.TestCase):

    def test_render(self):
        xfr = account.AccountTransformer('__')
        self.assertEqual('Assets__US__BofA__Checking',
                         xfr.render('Assets:US:BofA:Checking'))

    def test_parse(self):
        xfr = account.AccountTransformer('__')
        self.assertEqual('Assets:US:BofA:Checking',
                         xfr.parse('Assets__US__BofA__Checking'))

    def test_noop(self):
        xfr = account.AccountTransformer()
        acc = 'Assets:US:BofA:Checking'
        self.assertEqual(acc, xfr.render(acc))
        self.assertEqual(acc, xfr.parse(acc))
