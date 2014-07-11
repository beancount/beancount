import unittest
import tempfile
import os
import re
from os import path

from beancount.core import account
from beancount.utils import test_utils
from beancount.scripts import doctor
from beancount.scripts import directories_test


class TestScriptDoctor(test_utils.TestCase):

    @test_utils.docfile
    def test_dump_lexer(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['dump-lexer', filename])

        expected_output = """
            EOL               2 '\\n'
            DATE              2 '2013-01-01'
            OPEN              2 'open'
            ACCOUNT           2 'Expenses:Restaurant'
            EOL               3 '\\n'
            DATE              3 '2013-01-01'
            OPEN              3 'open'
            ACCOUNT           3 'Assets:Cash'
            EOL               4 '\\n'
            EOL               5 '\\n'
            DATE              5 '2014-03-02'
            FLAG              5 '*'
            STRING            5 '"Something"'
            EOL               6 '\\n'
            INDENT            6 '  '
            ACCOUNT           6 'Expenses:Restaurant'
            NUMBER            6 '50.02'
            CURRENCY          6 'USD'
            EOL               7 '\\n'
            INDENT            7 '  '
            ACCOUNT           7 'Assets:Cash'
            EOL               8 '\\n'
        """
        self.assertLines(expected_output, stdout.getvalue())

    @test_utils.docfile
    def test_dump_lexer_empty(self, filename):
        ""
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['dump-lexer', filename])

    @test_utils.docfile
    def test_list_accounts(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['list-accounts', filename])

        r = self.assertLines("""
            Assets:Cash          2013-01-01
            Expenses:Restaurant  2013-01-01
        """, stdout.getvalue())

    @test_utils.docfile
    def test_list_accounts_empty(self, filename):
        ""
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['list-accounts', filename])


class TestScriptCheckDirectories(directories_test.TestScriptCheckDirectories):

    @test_utils.docfile
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

        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['directories', filename, self.tmpdir])
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
