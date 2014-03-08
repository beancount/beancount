import tempfile
import re
import unittest
import os
from os import path
import shutil

from beancount.scripts.scripts_test_support import docfile, capture, run_with_args
from beancount.scripts import check_directories


class TestScriptCheckDirectories(unittest.TestCase):

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

    def setUp(self):
        self.tmpdir = tempfile.mkdtemp()
        for direc in self.TEST_DIRECTORIES:
            os.makedirs(path.join(self.tmpdir, direc))

    def tearDown(self):
        shutil.rmtree(self.tmpdir)

    def test_validation(self):
        accounts = set("""
            Expenses:Restaurant
            Expenses:Movie
            Expenses:Alcohol
            Assets:Cash
        """.strip().split())
        errors = check_directories.validate_directories(accounts, self.tmpdir)
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
        with capture() as stdout:
            run_with_args(check_directories.main, [filename, self.tmpdir])
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
