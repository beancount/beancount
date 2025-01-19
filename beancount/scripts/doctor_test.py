__copyright__ = "Copyright (C) 2014-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import os
import re
import textwrap
import unittest
from os import path

from beancount.parser import cmptest
from beancount.scripts import directories_test
from beancount.scripts.doctor import doctor
from beancount.utils import test_utils


class TestScriptDoctor(test_utils.ClickTestCase):
    @test_utils.docfile
    def test_dump_lexer(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        result = self.run_with_args(doctor, "dump-lexer", filename)

        expected_output = """
            EOL               2 b'\\n'
            DATE              2 b'2013-01-01'
            OPEN              2 b'open'
            ACCOUNT           2 b'Expenses:Restaurant'
            EOL               3 b'\\n'
            DATE              3 b'2013-01-01'
            OPEN              3 b'open'
            ACCOUNT           3 b'Assets:Cash'
            EOL               4 b'\\n'
            EOL               5 b'\\n'
            DATE              5 b'2014-03-02'
            ASTERISK          5 b'*'
            STRING            5 b'"Something"'
            EOL               6 b'\\n'
            INDENT            6 b'  '
            ACCOUNT           6 b'Expenses:Restaurant'
            NUMBER            6 b'50.02'
            CURRENCY          6 b'USD'
            EOL               7 b'\\n'
            INDENT            7 b'  '
            ACCOUNT           7 b'Assets:Cash'
            EOL               8 b'\\n'
        """
        self.assertLines(expected_output, result.stdout)

    @test_utils.docfile
    def test_dump_lexer_empty(self, filename):
        ""
        rv = self.run_with_args(doctor, "dump-lexer", filename)
        self.assertEqual(rv.stdout, "")

    @test_utils.docfile
    def test_dump_roundtrip(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        self.run_with_args(doctor, "roundtrip", filename)

    def test_list_options(self):
        self.run_with_args(doctor, "list_options")


class TestScriptCheckDirectories(
    directories_test.TestScriptCheckDirectories, test_utils.ClickTestCase
):
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

        rv = self.run_with_args(doctor, "directories", filename, self.tmpdir)
        self.assertEqual(2, len(rv.stdout.splitlines()))
        matches = set(match.group(1) for match in re.finditer("'(.*?)'", rv.stdout))
        tmpdir = path.realpath(self.tmpdir)
        clean_matches = set(
            match[len(tmpdir) + 1 :] if match.startswith(tmpdir) else match
            for match in matches
        )
        self.assertEqual(
            {
                "Expenses/Restaurant/Sub".replace("/", os.sep),
                "Expenses:Restaurant:Sub",
                "Assets:Extra",
                "Assets/Extra".replace("/", os.sep),
            },
            clean_matches,
        )


class TestScriptMissingOpen(cmptest.TestCase, test_utils.ClickTestCase):
    @test_utils.docfile
    def test_missing_open(self, filename):
        """
        2013-01-01 open Expenses:Movie
        2013-01-01 open Assets:Cash

        2014-03-03 * "Something"
          Expenses:Restaurant   50.02 USD
          Expenses:Movie        25.00 USD
          Assets:Cash

        2014-04-04 * "Something"
          Expenses:Alcohol      10.30 USD
          Expenses:Movie        25.00 USD
          Assets:Cash
        """
        rv = self.run_with_args(doctor, "missing-open", filename)

        self.assertEqualEntries(
            """

            2014-03-03 open Expenses:Restaurant
            2014-04-04 open Expenses:Alcohol

        """,
            rv.stdout,
        )


class TestScriptDisplayContext(cmptest.TestCase, test_utils.ClickTestCase):
    @test_utils.docfile
    def test_display_context(self, filename):
        """
        2013-01-01 open Expenses:Movie
        2013-01-01 open Assets:Cash

        2014-03-03 * "Something"
          Expenses:Restaurant   50.02 USD
          Expenses:Movie        25.00 USD
          Assets:Cash

        2014-04-04 * "Something"
          Expenses:Alcohol      10.30 USD
          Expenses:Movie        25.00 USD
          Assets:Cash
        """
        rv = self.run_with_args(doctor, "display-context", filename)
        # Note: This probably deserves a little more love.
        self.assertTrue(rv.stdout)


class TestContext(cmptest.TestCase, test_utils.ClickTestCase):
    @test_utils.docfile
    def test_context(self, filename):
        """
        2013-01-01 open Expenses:Movie
        2013-01-01 open Assets:Cash

        2014-03-03 * "Something"
          Expenses:Restaurant   50.02 USD
          Expenses:Movie        25.00 USD
          Assets:Cash

        2014-04-04 * "Something"
          Expenses:Alcohol      10.30 USD
          Expenses:Movie        25.00 USD
          Assets:Cash
        """
        rv = self.run_with_args(doctor, "context", filename, "6")
        self.assertRegex(rv.stdout, "Location:")
        self.assertRegex(rv.stdout, "50.02")

    @test_utils.docfile
    def test_context_multiple_files(self, filename: str):
        """
        2013-01-01 open Expenses:Movie
        2013-01-01 open Assets:Cash

        2014-03-03 * "Something"
          Expenses:Restaurant   50.02 USD
          Expenses:Movie        25.00 USD
          Assets:Cash

        2014-04-04 * "Something"
          Expenses:Alcohol      10.30 USD
          Expenses:Movie        25.00 USD
          Assets:Cash
        """

        with test_utils.temp_file() as topfile:
            topfile.write_text(
                textwrap.dedent(
                    """
                include "{}"
            """.format(filename.replace("\\", r"\\"))
                ),
                encoding="utf8",
            )
            rv = self.run_with_args(
                doctor, "context", str(topfile), "{}:6".format(filename)
            )
            self.assertRegex(rv.stdout, "Location:")
            self.assertRegex(rv.stdout, "50.02")


class TestLinked(cmptest.TestCase, test_utils.ClickTestCase):
    test_string = """
        2013-01-01 open Expenses:Movie
        2013-01-01 open Assets:Cash

        2014-03-03 * "Apples" ^abc
          Expenses:Restaurant   50.02 USD
          Expenses:Movie        25.00 USD
          Assets:Cash

        2014-04-04 * "Something"
          Expenses:Alcohol      10.30 USD
          Expenses:Movie        25.00 USD
          Assets:Cash

        2014-05-05 * "Oranges" ^abc
          Expenses:Alcohol      10.30 USD
          Expenses:Movie        25.00 USD
          Assets:Cash
    """

    @test_utils.docfile_extra(contents=test_string)
    def test_linked_lineno_only(self, filename):
        rv = self.run_with_args(doctor, "linked", filename, "6")
        self.assertRegex(rv.stdout, "Apples")
        self.assertRegex(rv.stdout, "Oranges")
        self.assertEqual(2, len(list(re.finditer(r"\.beancount:\d+:", rv.stdout))))

    @test_utils.docfile_extra(contents=test_string)
    def test_linked_multiple_files(self, filename: str):
        with test_utils.temp_file() as topfile:
            topfile.write_text(
                textwrap.dedent(
                    """
                include "{}"
            """.format(filename.replace("\\", r"\\"))
                ),
                encoding="utf8",
            )
            rv = self.run_with_args(doctor, "linked", str(topfile), "{}:6".format(filename))
            self.assertRegex(rv.stdout, "Apples")
            self.assertRegex(rv.stdout, "Oranges")
            self.assertEqual(2, len(list(re.finditer(r"\.beancount:\d+:", rv.stdout))))

    @test_utils.docfile_extra(contents=test_string)
    def test_linked_explicit_link(self, filename):
        rv = self.run_with_args(doctor, "linked", filename, "^abc")
        self.assertRegex(rv.stdout, "Apples")
        self.assertRegex(rv.stdout, "Oranges")
        self.assertEqual(
            2, len(list(re.finditer(r"\.beancount:\d+:", rv.stdout))), rv.stdout
        )


class TestRegion(cmptest.TestCase, test_utils.ClickTestCase):
    test_string = TestLinked.test_string

    @test_utils.docfile_extra(contents=test_string)
    def test_region(self, filename):
        rv = self.run_with_args(doctor, "region", filename, "4:12")
        self.assertRegex(rv.stdout, r"Cash\s+-110.32 USD")


if __name__ == "__main__":
    unittest.main()
