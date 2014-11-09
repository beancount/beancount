__author__ = "Martin Blais <blais@furius.ca>"

import re

from beancount.utils import test_utils
from beancount.scripts import query


def search_words(words, line):
    """Search for a sequence of words in a line.

    Args:
      words: A list of strings, the words to look for, or a space-separated string.
      line: A string, the line to search into.
    Returns:
      A MatchObject, or None.
    """
    if isinstance(words, str):
        words = words.split()
    return re.search('.*'.join(r'\b{}\b'.format(word) for word in words), line)


class TestHelpReports(test_utils.TestCase):

    def test_get_list_report_string(self):
        help_string = query.get_list_report_string()
        self.assertTrue(help_string and isinstance(help_string, str))

    def test_get_list_report_string__one_report(self):
        help_string = query.get_list_report_string('print')
        self.assertTrue(help_string and isinstance(help_string, str))

    def test_get_list_report_string__invalid_report(self):
        help_string = query.get_list_report_string('blablabla')
        self.assertEqual(None, help_string)


class TestScriptQuery(test_utils.TestCase):

    @test_utils.docfile
    def test_list_accounts_empty(self, filename):
        ""
        # Check that invocation with just a filename prints something (the list of reports).
        with test_utils.capture() as stdout:
            test_utils.run_with_args(query.main, [filename])
        self.assertTrue(stdout.getvalue())


class TestScriptPositions(test_utils.TestCase):

    @test_utils.docfile
    def test_success(self, filename):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Account3
        2013-01-01 open Equity:Unknown

        2013-04-05 *
          Equity:Unknown
          Assets:Account1     5000 USD

        2013-04-05 *
          Assets:Account1     -3000 USD
          Assets:Account2     30 BOOG {100 USD}

        2013-04-05 *
          Assets:Account1     -1000 USD
          Assets:Account3     800 EUR @ 1.25 USD
        """
        with test_utils.capture() as stdout:
            test_utils.run_with_args(query.main, [filename, 'holdings'])
        output = stdout.getvalue()
        self.assertTrue(search_words('Assets:Account1 1,000.00 USD', output))
        self.assertTrue(search_words('Assets:Account2    30.00 BOOG', output))
        self.assertTrue(search_words('Assets:Account3   800.00 EUR', output))

    @test_utils.docfile
    def test_print_trial(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.capture() as stdout:
            test_utils.run_with_args(query.main, [filename, 'trial'])
        output = stdout.getvalue()
        self.assertLines("""
            Assets:Cash               -50.02 USD
            Equity
            Expenses:Restaurant        50.02 USD
            Income
            Liabilities
        """, output)

    @test_utils.docfile
    def test_print_trial_empty(self, filename):
        ""
        with test_utils.capture() as stdout:
            test_utils.run_with_args(query.main, [filename, 'trial'])

    @test_utils.docfile
    def test_all_prices(self, filename):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Assets:Account1       10 GOOG @ 512.01 USD
          Income:Misc

        2014-02-01 price GOOG 524.02 USD
        2014-02-10 price GOOG 536.03 USD
        """
        with test_utils.capture() as stdout:
            test_utils.run_with_args(query.main, [filename, 'all_prices'])
        output = stdout.getvalue()
        self.assertLines("""
           2014-01-15 price GOOG             512.01 USD
           2014-02-01 price GOOG             524.02 USD
           2014-02-10 price GOOG             536.03 USD
        """, output)

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
            test_utils.run_with_args(query.main, [filename, 'accounts'])

        self.assertLines("""
            Assets:Cash          2013-01-01
            Expenses:Restaurant  2013-01-01
        """, stdout.getvalue())

    @test_utils.docfile
    def test_list_accounts_empty(self, filename):
        ""
        with test_utils.capture() as stdout:
            test_utils.run_with_args(query.main, [filename, 'accounts'])
