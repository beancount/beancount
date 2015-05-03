__author__ = "Martin Blais <blais@furius.ca>"

import tempfile
import datetime
import subprocess

from beancount.utils import test_utils
from beancount.scripts import report
from beancount.scripts import example
from beancount.parser import parser
from beancount.parser import cmptest
from beancount.reports import convert_reports


class TestLedgerUtilityFunctions(cmptest.TestCase):

    @parser.parsedoc
    def setUp(self, entries, _, __):
        """
          2014-10-01 * "Buy some stock with local funds"
            Assets:CA:Investment:GOOG          5 GOOG {500.00 USD}
            Expenses:Commissions            9.95 USD
            Assets:CA:Investment:Cash   -2509.95 USD

          2014-10-02 * "Regular price conversion with fee"
            Assets:CA:Investment:Cash    2500.00 USD
            Expenses:Commissions            9.95 USD
            Assets:CA:Investment:Cash   -2939.46 CAD @ 0.8879 USD

          2014-10-03 * "Buy some stock with foreign currency funds"
            Assets:CA:Investment:GOOG          5 GOOG {520.0 USD}
            Expenses:Commissions            9.95 USD
            Assets:CA:Investment:Cash   -2939.46 CAD @ 0.8879 USD
        """
        self.entries = entries

    def test_postings_by_type(self):
        postings_lists = convert_reports.postings_by_type(self.entries[0])
        self.assertEqual([2, 0, 1], list(map(len, postings_lists)))

        postings_lists = convert_reports.postings_by_type(self.entries[1])
        self.assertEqual([2, 1, 0], list(map(len, postings_lists)))

        postings_lists = convert_reports.postings_by_type(self.entries[2])
        self.assertEqual([1, 1, 1], list(map(len, postings_lists)))

    def test_split_currency_conversions(self):
        converted, _ = convert_reports.split_currency_conversions(self.entries[0])
        self.assertFalse(converted)

        converted, _ = convert_reports.split_currency_conversions(self.entries[1])
        self.assertFalse(converted)

        converted, new_entries = convert_reports.split_currency_conversions(self.entries[2])
        self.assertTrue(converted)
        self.assertEqualEntries("""

          2014-10-03 * "Buy some stock with foreign currency funds (Currency conversion)"
            Assets:CA:Investment:Cash       -2,939.46 CAD @ 0.8879 USD
            Assets:CA:Investment:Cash        2,609.946534 USD

          2014-10-03 * "Buy some stock with foreign currency funds"
            Assets:CA:Investment:GOOG            5 GOOG {520.0 USD}
            Expenses:Commissions                 9.95 USD
            Assets:CA:Investment:Cash       -2,609.946534 USD

        """, new_entries)


class TestLedgerConversion(test_utils.TestCase):

    def check_parses_ledger(self, ledger_filename):
        """Assert that the filename parses through Ledger without errors.

        Args:
          filename: A string, the name of the Ledger file.
        """
        try:
            pipe = subprocess.Popen(['ledger', '-f', ledger_filename, 'bal'],
                                    shell=False,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            stdout, stderr = pipe.communicate()
            self.assertEqual(0, pipe.returncode, stderr)
        except OSError as exc:
            if exc.errno == 2:
                self.skipTest('Ledger is not installed, cannot verify conversion')

    @test_utils.docfile
    def test_simple(self, filename):
        """
          ;; All supported features exhibited here.

          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash         USD,CAD

          2014-02-15 price GOOG 500.00 USD

          2014-03-02 * "Something"
            Expenses:Restaurant   50.02 USD
            Assets:Cash

        """
        with test_utils.capture() as stdout:
            result = test_utils.run_with_args(report.main, [filename, 'ledger'])
        self.assertEqual(0, result)
        self.assertLines("""

          account Expenses:Restaurant

          account Assets:Cash
            assert commodity == "USD" | commodity == "CAD"

          P 2014/02/15 00:00:00 GOOG                   500.00 USD

          2014/03/02 * Something
            Expenses:Restaurant                                                     50.02 USD
            Assets:Cash                                                            -50.02 USD

        """, stdout.getvalue())

    @test_utils.docfile
    def test_cost_and_foreign_currency(self, filename):
        """
          2014-01-01 open Assets:CA:Investment:GOOG
          2014-01-01 open Expenses:Commissions
          2014-01-01 open Assets:CA:Investment:Cash

          2014-11-02 * "Buy some stock with foreign currency funds"
            Assets:CA:Investment:GOOG          5 GOOG {520.0 USD}
            Expenses:Commissions            9.95 USD
            Assets:CA:Investment:Cash   -2939.46 CAD @ 0.8879 USD
        """
        with test_utils.capture() as stdout:
            result = test_utils.run_with_args(report.main, [filename, 'ledger'])
        self.assertEqual(0, result)
        self.assertLines("""

          account Assets:CA:Investment:GOOG

          account Expenses:Commissions

          account Assets:CA:Investment:Cash

          2014/11/02 * Buy some stock with foreign currency funds
            Assets:CA:Investment:GOOG           5 GOOG {520.0 USD} @ 520.0 USD
            Expenses:Commissions             9.95 USD
            Assets:CA:Investment:Cash    -2939.46 CAD @ 0.8879 USD
            Equity:Rounding             -0.003466 USD

          P 2014/11/02 00:00:00 GOOG    520.0 USD

          P 2014/11/02 00:00:00 CAD    0.8879 USD

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

                import shutil; shutil.copyfile(lgrfile.name, '/tmp/test.ledger')
                self.check_parses_ledger(lgrfile.name)

class TestHLedgerConversion(test_utils.TestCase):

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
