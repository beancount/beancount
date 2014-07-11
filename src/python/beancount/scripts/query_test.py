import re

from beancount.utils import test_utils
from beancount.scripts import query


def search_words(words, line):
    if isinstance(words, str):
        words = words.split()
    return re.search('.*'.join(r'\b{}\b'.format(word) for word in words), line)



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
            |-- Assets
            |   `-- Cash               -50.02 USD
            `-- Expenses
                `-- Restaurant          50.02 USD
        """, output)

    @test_utils.docfile
    def test_print_trial_empty(self, filename):
        ""
        with test_utils.capture() as stdout:
            test_utils.run_with_args(query.main, [filename, 'trial'])

    @test_utils.docfile
    def test_prices(self, filename):
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
            test_utils.run_with_args(query.main, [filename, 'prices'])
        output = stdout.getvalue()
        self.assertLines("""
           2014-01-15 price GOOG             512.01 USD

           2014-02-01 price GOOG             524.02 USD

           2014-02-10 price GOOG             536.03 USD
        """, output)
