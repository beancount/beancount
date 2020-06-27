__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import io
import unittest

from beancount.core.number import D
from beancount.core.amount import A
from beancount.core import realization
from beancount.core import position
from beancount.core import data
from beancount.reports import journal_text
from beancount import loader


class TestAmountColumnSizer(unittest.TestCase):

    def test_sizer(self):
        sizer = journal_text.AmountColumnSizer('balance')
        sizer.update(D('10'), 'USD')
        sizer.update(D('200'), 'HOOL')
        sizer.update(D('3000.01'), 'HOOLB')
        self.assertEqual(4, sizer.get_number_width())
        self.assertEqual('{0:>10.4f} {1:<5}', sizer.get_format(4))
        self.assertEqual('{balance:<16}', sizer.get_generic_format(4))


class TestJournalRenderPosting(unittest.TestCase):

    number_format = '{} {}'

    def test_render_posting_no_cost(self):
        pos = position.from_string('100 USD')
        str_posting = journal_text.render_posting(
            data.Posting('Assets:Something', pos.units, pos.cost, None,
                         None, None),
            self.number_format)
        self.assertEqual('  Assets:Something                 100 USD',
                         str_posting)

    def test_render_posting_cost(self):
        pos = position.from_string('10 VHT {45.32 USD}')
        str_posting = journal_text.render_posting(
            data.Posting('Assets:Something', pos.units, pos.cost, None,
                         None, None),
            self.number_format)
        self.assertEqual('  Assets:Something                 10 VHT {45.32 USD}',
                         str_posting)

    def test_render_posting_price(self):
        pos = position.from_string('10 VHT')
        str_posting = journal_text.render_posting(
            data.Posting('Assets:Something', pos.units, pos.cost, A('45.32 USD'),
                         None, None),
            self.number_format)
        self.assertEqual('  Assets:Something                 10 VHT @ 45.32 USD',
                         str_posting)

    def test_render_posting_cost_price(self):
        pos = position.from_string('10 VHT {45.32 USD}')
        str_posting = journal_text.render_posting(
            data.Posting('Assets:Something', pos.units, pos.cost, A('47.00 USD'),
                         None, None),
            self.number_format)
        self.assertEqual(
            '  Assets:Something                 10 VHT {45.32 USD} @ 47.00 USD',
            str_posting)


class TestJournalTextRender(unittest.TestCase):

    @loader.load_doc(expect_errors=True)
    def setUp(self, entries, _, __):
        """
        plugin "beancount.plugins.implicit_prices"

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

    def test_get_entry_text_description(self):
        expected_descriptions = [
            'Assets:Checking',
            'Assets:Investing',
            'Assets:Savings',
            'Income:MountainOfMoney',
            'Equity:Opening-Balances',
            '-',
            '(Padding inserted for Balance of 100.00 USD for difference 100.00 USD)',
            'PASS - In Assets:Checking',
            'Salary',
            'PASS - In Assets:Checking',
            'Something to say',
            '/path/to/document.pdf',
            'Transfer',
            'Investment',
            '-',
            'FAIL - In Assets:Checking; expected = 0.00 USD, difference = 600.00 USD',
            'Assets:Checking',
            ]
        for expected_description, entry in zip(expected_descriptions, self.entries):
            self.assertEqual(expected_description,
                             journal_text.get_entry_text_description(entry))

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


if __name__ == '__main__':
    unittest.main()
