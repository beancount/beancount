__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import os
import re
import textwrap
import tempfile
from os import path
import unittest

from beancount.parser import cmptest
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
        self.assertLines(expected_output, stdout.getvalue())

    # pylint: disable=empty-docstring
    @test_utils.docfile
    def test_dump_lexer_empty(self, filename):
        ""
        with test_utils.capture():
            test_utils.run_with_args(doctor.main, ['dump-lexer', filename])

    @test_utils.docfile
    def test_dump_roundtrip(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.capture('stdout', 'stderr'):
            test_utils.run_with_args(doctor.main, ['roundtrip', filename])

    def test_list_options(self):
        with test_utils.capture():
            test_utils.run_with_args(doctor.main, ['list_options'])
            test_utils.run_with_args(doctor.main, ['list-options'])

    def test_deps(self):
        with test_utils.capture():
            test_utils.run_with_args(doctor.main, ['deps'])
            test_utils.run_with_args(doctor.main, ['checkdeps'])


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
        matches = set(match.group(1) for match in re.finditer("'(.*?)'", stdout.getvalue()))
        clean_matches = set(match[len(self.tmpdir)+1:]
                            if match.startswith(self.tmpdir)
                            else match
                            for match in matches)
        self.assertEqual({'Expenses/Restaurant/Sub',
                          'Expenses:Restaurant:Sub',
                          'Assets:Extra',
                          'Assets/Extra'}, clean_matches)


class TestScriptMissingOpen(cmptest.TestCase):

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
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['missing-open', filename])

        self.assertEqualEntries("""

            2014-03-03 open Expenses:Restaurant
            2014-04-04 open Expenses:Alcohol

        """, stdout.getvalue())


class TestScriptDisplayContext(cmptest.TestCase):

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
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['display-context', filename])
        # Note: This probably deserves a little more love.
        self.assertTrue(stdout.getvalue())


class TestContext(cmptest.TestCase):

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
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['context', filename, '6'])
        self.assertRegex(stdout.getvalue(), 'Location:')
        self.assertRegex(stdout.getvalue(), '50.02')

    @test_utils.docfile
    def test_context_multiple_files(self, filename):
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

        with tempfile.NamedTemporaryFile('w') as topfile:
            topfile.write(textwrap.dedent("""
                include "{}"
            """.format(filename)))
            topfile.flush()
            with test_utils.capture() as stdout:
                test_utils.run_with_args(doctor.main, ['context', topfile.name,
                                                       '{}:6'.format(filename)])
            self.assertRegex(stdout.getvalue(), 'Location:')
            self.assertRegex(stdout.getvalue(), '50.02')

class TestLinked(cmptest.TestCase):

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
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['linked', filename, '6'])
        self.assertRegex(stdout.getvalue(), 'Apples')
        self.assertRegex(stdout.getvalue(), 'Oranges')
        self.assertEqual(2, len(list(re.finditer(r'/(tmp|var/folders)/.*:\d+:',
                                                 stdout.getvalue()))))

    @test_utils.docfile_extra(contents=test_string)
    def test_linked_multiple_files(self, filename):
        with tempfile.NamedTemporaryFile('w') as topfile:
            topfile.write(textwrap.dedent("""
                include "{}"
            """.format(filename)))
            topfile.flush()
            with test_utils.capture() as stdout:
                test_utils.run_with_args(doctor.main, ['linked', topfile.name,
                                                       '{}:6'.format(filename)])
            self.assertRegex(stdout.getvalue(), 'Apples')
            self.assertRegex(stdout.getvalue(), 'Oranges')
            self.assertEqual(2, len(list(re.finditer(r'/(tmp|var/folders)/.*:\d+:',
                                                     stdout.getvalue()))))

    @test_utils.docfile_extra(contents=test_string)
    def test_linked_explicit_link(self, filename):
        with test_utils.capture() as stdout:
            test_utils.run_with_args(doctor.main, ['linked', filename, '^abc'])
        self.assertRegex(stdout.getvalue(), 'Apples')
        self.assertRegex(stdout.getvalue(), 'Oranges')
        self.assertEqual(2, len(list(re.finditer(r'/(tmp|var/folders)/.*:\d+:',
                                                 stdout.getvalue()))))


if __name__ == '__main__':
    unittest.main()
