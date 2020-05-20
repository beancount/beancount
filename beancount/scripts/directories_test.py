__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import tempfile
import unittest
import os
from os import path
import shutil

from beancount.scripts import directories


class TestScriptCheckDirectories(unittest.TestCase):

    def setUp(self):
        self.tmpdir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmpdir)

    TEST_DIRECTORIES = [
        "Expenses/Restaurant",
        "Income",
        "Assets/Cash",
        "Assets/Extra", # Illegal subaccount directory.
        "Expenses/Restaurant/notes", # Legal extra.
        "Expenses/Restaurant/Sub", # Illegal subaccount.
        "Expenses/notes" # Legal extra.
        "Expenses/notes/Fun" # Legal extra in the middle.
    ]

    def test_validation(self):
        for directory in self.TEST_DIRECTORIES:
            os.makedirs(path.join(self.tmpdir, directory))

        accounts = set("""
            Expenses:Restaurant
            Expenses:Movie
            Expenses:Alcohol
            Assets:Cash
        """.strip().split())
        errors = directories.validate_directory(accounts, self.tmpdir)
        self.assertEqual(2, len(errors))

        expected_error_accounts = set("""
            Assets/Extra
            Expenses/Restaurant/Sub
        """.strip().split())
        for error in errors:
            message = str(error)
            self.assertTrue(any(expected_account in message
                                for expected_account in expected_error_accounts))

    def test_validation_no_parent(self):
        for directory in ['Liabilities/US/CreditCard']:
            os.makedirs(path.join(self.tmpdir, directory))

        accounts = set("""
            Liabilities:US:CreditCard
        """.strip().split())
        errors = directories.validate_directory(accounts, self.tmpdir)

        # The parent directory Liabilities:US should not trigger an error here.
        self.assertEqual([], errors)


if __name__ == '__main__':
    unittest.main()
