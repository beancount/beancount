__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import tempfile
import datetime
import re
import shutil
import subprocess
import unittest

from beancount.core import data
from beancount.utils import test_utils
from beancount.reports import report
from beancount.scripts import example
from beancount.parser import cmptest
from beancount.reports import convert_reports
from beancount import loader


class TestLedgerUtilityFunctions(cmptest.TestCase):

    def test_quote_currency(self):
        test = """
          2014-10-01 * "Buy some stock with local funds"
            Assets:CA:Investment:HOOL          5 HOOL1 {500.00 USD}
            Expenses:Commissions            9.95 USD
        """
        expected = """
          2014-10-01 * "Buy some stock with local funds"
            Assets:CA:Investment:HOOL          5 "HOOL1" {500.00 USD}
            Expenses:Commissions            9.95 USD
        """
        self.assertEqual(expected, convert_reports.quote_currency(test))


class TestLedgerUtilityFunctionsOnPostings(cmptest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, _, __):
        """
          2000-01-01 open Assets:CA:Investment:HOOL
          2000-01-01 open Expenses:Commissions
          2000-01-01 open Assets:CA:Investment:Cash

          2014-10-01 * "Buy some stock with local funds"
            Assets:CA:Investment:HOOL          5 HOOL {500.00 USD}
            Expenses:Commissions            9.95 USD
            Assets:CA:Investment:Cash   -2509.95 USD

          2014-10-02 * "Regular price conversion with fee"
            Assets:CA:Investment:Cash    2500.00 USD
            Expenses:Commissions            9.95 USD
            Assets:CA:Investment:Cash   -2826.84 CAD @ 0.8879 USD

          2014-10-03 * "Buy some stock with foreign currency funds"
            Assets:CA:Investment:HOOL          5 HOOL {520.0 USD}
            Expenses:Commissions            9.95 USD
            Assets:CA:Investment:Cash   -2939.46 CAD @ 0.8879 USD
        """
        self.txns = [entry for entry in entries if isinstance(entry, data.Transaction)]

    def test_postings_by_type(self):
        postings_lists = convert_reports.postings_by_type(self.txns[0])
        self.assertEqual([2, 0, 1], list(map(len, postings_lists)))

        postings_lists = convert_reports.postings_by_type(self.txns[1])
        self.assertEqual([2, 1, 0], list(map(len, postings_lists)))

        postings_lists = convert_reports.postings_by_type(self.txns[2])
        self.assertEqual([1, 1, 1], list(map(len, postings_lists)))

    def test_split_currency_conversions(self):
        converted, _ = convert_reports.split_currency_conversions(self.txns[0])
        self.assertFalse(converted)

        converted, _ = convert_reports.split_currency_conversions(self.txns[1])
        self.assertFalse(converted)

        converted, new_entries = convert_reports.split_currency_conversions(self.txns[2])
        self.assertTrue(converted)
        self.assertEqualEntries("""

          2014-10-03 * "Buy some stock with foreign currency funds (Currency conversion)"
            Assets:CA:Investment:Cash       -2,939.46 CAD @ 0.8879 USD
            Assets:CA:Investment:Cash        2,609.946534 USD

          2014-10-03 * "Buy some stock with foreign currency funds"
            Assets:CA:Investment:HOOL            5 HOOL {520.0 USD}
            Expenses:Commissions                 9.95 USD
            Assets:CA:Investment:Cash       -2,609.946534 USD

        """, new_entries)


def get_ledger_version():
    """Check that we have a sufficient version of Ledger installed.

    Returns:
      A tuple of integer, the Ledger binary version triple, or None,
      if Ledger is not installed or could not be run.
    """
    try:
        pipe = subprocess.Popen(['ledger', '--version'],
                                shell=False,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        stdout, stderr = pipe.communicate()
        match = re.search(r'(\d+)\.(\d+)\.(\d+)', stdout.decode('utf-8'))
        if match:
            return tuple(map(int, match.group(1, 2, 3)))
    except OSError:
        pass
    return None


class TestLedgerConversion(test_utils.TestCase):

    def check_parses_ledger(self, ledger_filename):
        """Assert that the filename parses through Ledger without errors.

        Args:
          filename: A string, the name of the Ledger file.
        """
        version = get_ledger_version()
        if version is None or version < (3, 0, 0):
            self.skipTest('Ledger is not installed or has insufficient version, '
                          'cannot verify conversion; skipping test')

        pipe = subprocess.Popen(['ledger', '-f', ledger_filename, 'bal'],
                                shell=False,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        stdout, stderr = pipe.communicate()
        self.assertEqual(0, pipe.returncode, stderr)

    @test_utils.docfile
    def test_simple(self, filename):
        """
          ;; All supported features exhibited here.

          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash         USD,CAD

          2014-02-15 price HOOL 500.00 USD

          2014-03-02 * "Something"
            Expenses:Restaurant   50.02 USD
            Assets:Cash

          2015-01-01 custom "budget" Expenses:Food  "yearly"  34.43 HRK
        """
        with test_utils.capture() as stdout:
            result = test_utils.run_with_args(report.main, [filename, 'ledger'])
        self.assertEqual(0, result)
        self.assertLines("""

          account Expenses:Restaurant

          account Assets:Cash
            assert commodity == "USD" | commodity == "CAD"

          P 2014-02-15 00:00:00 HOOL                   500.00 USD

          2014-03-02 * Something
            Expenses:Restaurant                                                     50.02 USD
            Assets:Cash                                                            -50.02 USD

        """, stdout.getvalue())

    @test_utils.docfile
    def test_cost_and_foreign_currency(self, filename):
        """
          plugin "beancount.plugins.implicit_prices"

          2014-01-01 open Assets:CA:Investment:HOOL
          2014-01-01 open Expenses:Commissions
          2014-01-01 open Assets:CA:Investment:Cash

          2014-11-02 * "Buy some stock with foreign currency funds"
            Assets:CA:Investment:HOOL          5 HOOL {520.0 USD}
            Expenses:Commissions            9.95 USD
            Assets:CA:Investment:Cash   -2939.46 CAD @ 0.8879 USD
        """
        with test_utils.capture() as stdout:
            result = test_utils.run_with_args(report.main, [filename, 'ledger'])
        self.assertEqual(0, result)
        self.assertLines("""

          account Assets:CA:Investment:HOOL

          account Expenses:Commissions

          account Assets:CA:Investment:Cash

          2014-11-02 * Buy some stock with foreign currency funds
            Assets:CA:Investment:HOOL           5 HOOL {520.0 USD} @ 520.0 USD
            Expenses:Commissions             9.95 USD
            Assets:CA:Investment:Cash    -2939.46 CAD @ 0.8879 USD
            Equity:Rounding             -0.003466 USD

          P 2014-11-02 00:00:00 HOOL    520.0 USD

          P 2014-11-02 00:00:00 CAD    0.8879 USD

        """, stdout.getvalue())

    @test_utils.docfile
    def test_tags_links(self, filename):
        """
          2019-01-25 open Assets:A
          2019-01-25 open Assets:B

          2019-01-25 * "Test tags" #foo ^link2 #bar #baz ^link1
            Assets:A                       10.00 EUR
            Assets:B                      -10.00 EUR
        """
        with test_utils.capture() as stdout:
            result = test_utils.run_with_args(report.main, [filename, 'ledger'])
        self.assertEqual(0, result)
        self.assertLines("""
          account Assets:A

          account Assets:B

          2019-01-25 * Test tags
            ; :bar:baz:foo:
            ; Link: link1, link2
            Assets:A                       10.00 EUR
            Assets:B                      -10.00 EUR
        """, stdout.getvalue())

    def test_example(self):
        with tempfile.NamedTemporaryFile('w',
                                         suffix='.beancount',
                                         encoding='utf-8') as beanfile:
            # Generate an example Beancount file.
            example.write_example_file(datetime.date(1980, 1, 1),
                                       datetime.date(2010, 1, 1),
                                       datetime.date(2014, 1, 1),
                                       True,
                                       file=beanfile)
            beanfile.flush()

            # Convert the file to Ledger format.
            with tempfile.NamedTemporaryFile('w', suffix='.ledger') as lgrfile:
                with test_utils.capture():
                    result = test_utils.run_with_args(
                        report.main, [beanfile.name, '-o', lgrfile.name, 'ledger'])
                self.assertEqual(0, result)

                # FIXME: Use a proper temp dir.
                shutil.copyfile(lgrfile.name, '/tmp/test.ledger')
                self.check_parses_ledger(lgrfile.name)


class TestHLedgerConversion(test_utils.TestCase):

    @test_utils.docfile
    def test_tags_links(self, filename):
        """
          2019-01-25 open Assets:A
          2019-01-25 open Assets:B

          2019-01-25 * "Test tags" #foo ^link2 #bar #baz ^link1
            Assets:A                       10.00 EUR
            Assets:B                      -10.00 EUR
        """
        with test_utils.capture() as stdout:
            result = test_utils.run_with_args(report.main, [filename, 'hledger'])
        self.assertEqual(0, result)
        self.assertLines("""
          ;; Open: 2019-01-25 close Assets:A

          ;; Open: 2019-01-25 close Assets:B

          2019-01-25 * Test tags
            ; bar:, baz:, foo:
            ; Link: link1 link2
            Assets:A                       10.00 EUR
            Assets:B                      -10.00 EUR
        """, stdout.getvalue())

    def test_example(self):
        with tempfile.NamedTemporaryFile('w',
                                         suffix='.beancount',
                                         encoding='utf-8') as beanfile:
            # Generate an example Beancount file.
            example.write_example_file(datetime.date(1980, 1, 1),
                                       datetime.date(2010, 1, 1),
                                       datetime.date(2014, 1, 1),
                                       reformat=True,
                                       file=beanfile)
            beanfile.flush()

            # Convert the file to HLedger format.
            #
            # Note: don't bother parsing for now, just a smoke test to make sure
            # we don't fail on run.
            with tempfile.NamedTemporaryFile('w', suffix='.hledger') as lgrfile:
                with test_utils.capture():
                    result = test_utils.run_with_args(
                        report.main, [beanfile.name, '-o', lgrfile.name, 'hledger'])
                self.assertEqual(0, result)


if __name__ == '__main__':
    unittest.main()
