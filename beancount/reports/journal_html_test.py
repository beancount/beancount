__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import re
import io
import unittest

from beancount import loader
from beancount.core import realization
from beancount.core import data
from beancount.core import display_context
from beancount.reports import html_formatter
from beancount.reports import journal_html


class TestJournalRender(unittest.TestCase):

    @loader.load_doc(expect_errors=True)
    def setUp(self, entries, _, __):
        """
        2014-01-01 open Assets:Checking
        2014-01-01 open Assets:Investing
        2014-01-01 open Assets:Savings
        2014-01-01 open Income:MountainOfMoney
        2014-01-01 open Equity:Opening-Balances
        2014-01-01 open Assets:Investing:Cash
        2014-01-01 open Assets:Investing:Stock

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

    def test_iterate_html_postings(self):
        formatter = html_formatter.HTMLFormatter(display_context.DEFAULT_DISPLAY_CONTEXT)
        rows = list(journal_html.iterate_html_postings(self.real_account.txn_postings,
                                                       formatter))

        # Check (entry, leg_postings, rowtype, extra_class, flag).
        self.assertEqual([
            (data.Open, 0, 'Open', '', ''),
            (data.Pad, 0, 'Pad', '', ''),
            (data.Transaction, 1, 'Padding', '', 'P'),
            (data.Balance, 0, 'Balance', '', ''),
            (data.Transaction, 1, 'Transaction', 'warning', '!'),
            (data.Balance, 0, 'Balance', '', ''),
            (data.Note, 0, 'Note', '', ''),
            (data.Document, 0, 'Document', '', ''),
            (data.Transaction, 2, 'Transaction', '', '*'),
            (data.Balance, 0, 'Balance', 'fail', ''),
            (data.Close, 0, 'Close', '', ''),
            ], [(type(row.entry),
                 len(row.leg_postings),
                 row.rowtype,
                 row.extra_class,
                 row.flag) for row in rows])

        self.assertTrue(all(re.search(row.rowtype, row.description)
                            for row in rows
                            if not isinstance(row.entry, data.Transaction)))

        self.assertTrue(all(isinstance(row.amount_str, str) for row in rows))
        self.assertTrue(all(isinstance(row.balance_str, str) for row in rows))

    def test_html_entries_table_with_balance(self):
        oss = io.StringIO()
        formatter = html_formatter.HTMLFormatter(display_context.DEFAULT_DISPLAY_CONTEXT)
        journal_html.html_entries_table_with_balance(
            oss, self.real_account.txn_postings, formatter, True)
        html = oss.getvalue()
        self.assertTrue(isinstance(html, str))
        self.assertRegex(html, '<table')

    def test_html_entries_table(self):
        oss = io.StringIO()
        formatter = html_formatter.HTMLFormatter(display_context.DEFAULT_DISPLAY_CONTEXT)
        journal_html.html_entries_table_with_balance(
            oss, self.real_account.txn_postings, formatter, True)
        html = oss.getvalue()
        self.assertTrue(isinstance(html, str))
        self.assertRegex(html, '<table')

    def test_render_links(self):
        html = journal_html.render_links({'132333b32eab', '6e3ac126f337'})
        self.assertRegex(html, '132333b32eab')
        self.assertRegex(html, '6e3ac126f337')


if __name__ == '__main__':
    unittest.main()
