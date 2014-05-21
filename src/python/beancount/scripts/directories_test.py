import tempfile
import re
import unittest
import os
from os import path
import shutil

from beancount.scripts import docfile, capture, run_with_args
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
        errors = directories.validate_directories(accounts, self.tmpdir)
        self.assertEqual(2, len(errors))

        expected_error_accounts = set("""
            Assets/Extra
            Expenses/Restaurant/Sub
        """.strip().split())
        for error in errors:
            message = str(error)
            self.assertTrue(any(expected_account in message
                                for expected_account in expected_error_accounts))

    @docfile
    def test_invocation(self, filename):
        """
            2013-01-01 open Expenses:Restaurant
            2013-01-01 open Expenses:Movie
            2013-01-01 open Expenses:Alcohol
            2013-01-01 open Assets:Cash

            2014-03-02 * "Something"
              Expenses:Restaurant   50.02 USD
              Expenses:Alcohol      10.30 USD
              Expenses:Movie        25.00 USD
              Assets:Cash
        """
        for directory in self.TEST_DIRECTORIES:
            os.makedirs(path.join(self.tmpdir, directory))

        with capture() as stdout:
            run_with_args(directories.main, [filename, self.tmpdir])
        self.assertEqual(2, len(stdout.getvalue().splitlines()))
        matches = set(mo.group(1) for mo in re.finditer("'(.*?)'", stdout.getvalue()))
        clean_matches = set(match[len(self.tmpdir)+1:]
                            if match.startswith(self.tmpdir)
                            else match
                            for match in matches)
        self.assertEqual({'Expenses/Restaurant/Sub',
                          'Expenses:Restaurant:Sub',
                          'Assets:Extra',
                          'Assets/Extra'}, clean_matches)

    def test_validation_no_parent(self):
        for directory in ['Liabilities/US/CreditCard']:
            os.makedirs(path.join(self.tmpdir, directory))

        accounts = set("""
            Liabilities:US:CreditCard
        """.strip().split())
        errors = directories.validate_directories(accounts, self.tmpdir)

        # The parent directory Liabilities:US should not trigger an error here.
        self.assertEqual([], errors)
