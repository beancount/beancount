import io
import unittest

from beancount import loader
from beancount.core import realization
from beancount.reports import journal_text


class TestJournalTextRender(unittest.TestCase):

    @loader.loaddoc
    def setUp(self, entries, _, __):
        """
        2014-01-01 open Assets:Checking
        2014-01-01 open Assets:Investing
        2014-01-01 open Assets:Savings
        2014-01-01 open Income:MountainOfMoney
        2014-01-01 open Equity:Opening-Balances

        2014-01-05 pad Assets:Checking Equity:Opening-Balances

        2014-02-10 balance Assets:Checking   100.00 USD

        2014-03-04 ! "Salary"
          Income:MountainOfMoney    -4000.00 USD
          Assets:Checking

        2014-03-05 balance Assets:Checking   4100.00 USD

        2014-03-10 note Assets:Checking "Something to say"
        2014-03-11 document Assets:Checking "/path/to/document.pdf"

        ;; With link.
        2014-03-17 * "Transfer" ^784375fd5a68
          Assets:Checking          -2000 USD
          Assets:Checking          -1500 USD
          Assets:Investing:Cash     3500 USD

        2014-03-25 * "Investment"
          Assets:Investing:Cash      -3000 USD
          Assets:Investing:Stock        30 STOCK {100 USD}

        ;; Failing.
        2014-05-01 balance  Assets:Checking   0.00 USD


        2014-12-31 close Assets:Checking
        """
        self.entries = entries
        real_root = realization.realize(self.entries)
        self.real_account = realization.get(real_root, 'Assets:Checking')
        self.postings = realization.get_postings(self.real_account)

    def test_combinations(self):

        for width in (80, 200):
            for at_cost in True, False:
                for render_balance in True, False:
                    for precision in (1, 2, 3, 5):
                        for verbosity in (journal_text.COMPACT,
                                          journal_text.NORMAL,
                                          journal_text.VERBOSE):
                            for output_format in (journal_text.FORMAT_TEXT,
                                                  journal_text.FORMAT_CSV):
                                oss = io.StringIO()
                                journal_text.text_entries_table(
                                    oss,
                                    self.postings,
                                    width, at_cost, render_balance, precision, verbosity,
                                    output_format)
                                self.assertTrue(oss.getvalue())

    def test_narrow(self):
        oss = io.StringIO()
        with self.assertRaises(ValueError):
            journal_text.text_entries_table(
                oss, self.postings,
                30, False, True, 4, journal_text.NORMAL, journal_text.FORMAT_TEXT)
