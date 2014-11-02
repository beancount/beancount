import tempfile
import datetime
import subprocess

from beancount.utils import test_utils
from beancount.scripts import query
from beancount.scripts import example


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
            result = test_utils.run_with_args(query.main, [filename, 'ledger'])
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

    def test_example(self):
        with tempfile.NamedTemporaryFile('w', suffix='.beancount') as beanfile:
            # Generate an example Beancount file.
            example.write_example_file(datetime.date(1980, 1, 1),
                                       datetime.date(2010, 1, 1),
                                       datetime.date(2014, 1, 1),
                                       file=beanfile)
            beanfile.flush()

            # Convert the file to Ledger format.
            with tempfile.NamedTemporaryFile('w', suffix='.ledger') as lgrfile:
                with test_utils.capture() as stdout:
                    result = test_utils.run_with_args(
                        query.main, [beanfile.name, '-o', lgrfile.name, 'ledger'])
                self.assertEqual(0, result)

                self.check_parses_ledger(lgrfile.name)
