__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import textwrap

from beancount import loader
from beancount.parser import cmptest
from beancount.plugins import book_conversions
from beancount.utils import test_utils


class TestBookConversions(cmptest.TestCase):

    @loader.load_doc()
    def test_book_conversions_example(self, entries, errors, __):
        """
          plugin "beancount.plugins.book_conversions" "Assets:Bitcoin,Income:Bitcoin"

          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 * "Buy some bitcoins"
            Assets:Bank          -1000.00 USD
            Assets:Bitcoin       4.333507 BTC @ 230.76 USD

          2015-09-05 * "Buy some more bitcoins"
            Assets:Bank          -1000.00 USD
            Assets:Bitcoin       4.345747 BTC @ 230.11 USD

          2015-09-20 * "Use some bitcoins from two lots"
            Assets:Bitcoin       -6.000000 BTC @ 230.50 USD
            Expenses:Something
        """
        self.assertEqualEntries("""

          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 * "Buy some bitcoins"
            Assets:Bitcoin  4.333507 BTC {230.76 USD} @ 230.76 USD
            Assets:Bank     -1000.00 USD

          2015-09-05 * "Buy some more bitcoins"
            Assets:Bitcoin  4.345747 BTC {230.11 USD} @ 230.11 USD
            Assets:Bank     -1000.00 USD

          2015-09-20 * "Use some bitcoins from two lots"
            Assets:Bitcoin          -4.333507 BTC {230.76 USD} @ 230.50 USD
            Assets:Bitcoin          -1.666493 BTC {230.11 USD} @ 230.50 USD
            Income:Bitcoin         0.47677955 USD
            Expenses:Something  1383.00000000 USD

        """, entries)

    def _convert_and_check_matches(self, entries):
        """Perform the conversion and cross-check output matches with
        extracted matches. We do this to ensure that trades extracted from
        metadata are consistent with those computed at conversion time, for
        all tests.
        """
        entries, errors, matches = book_conversions.book_price_conversions(
            entries, "Assets:Bitcoin", "Income:Bitcoin")
        self.assertFalse(errors)
        trades = book_conversions.extract_trades(entries)
        self.assertEqual(matches, trades)
        return entries

    @loader.load_doc()
    def test_book_conversions_split_augmenting(self, entries, errors, __):
        """
          ;; plugin "beancount.plugins.book_conversions" "Assets:Bitcoin,Income:Bitcoin"

          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 *
            Assets:Bank          -1000.00 USD
            Assets:Bitcoin       4.347826 BTC @ 230.00 USD

          2015-09-20 *
            Assets:Bitcoin       -2.000000 BTC @ 231.00 USD
            Expenses:Something

          2015-09-21 *
            Assets:Bitcoin       -2.000000 BTC @ 232.00 USD
            Expenses:Something

          2015-09-22 *
            Assets:Bitcoin       -0.347826 BTC @ 233.00 USD
            Expenses:Something
        """
        entries = self._convert_and_check_matches(entries)
        self.assertEqualEntries("""

          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 *
            Assets:Bitcoin  4.347826 BTC {230.00 USD} @ 230.00 USD
            Assets:Bank     -1000.00 USD

          2015-09-20 *
            Assets:Bitcoin         -2.000000 BTC {230.00 USD} @ 231.00 USD
            Income:Bitcoin       -2.00000000 USD
            Expenses:Something  462.00000000 USD

          2015-09-21 *
            Assets:Bitcoin         -2.000000 BTC {230.00 USD} @ 232.00 USD
            Income:Bitcoin       -4.00000000 USD
            Expenses:Something  464.00000000 USD

          2015-09-22 *
            Assets:Bitcoin        -0.347826 BTC {230.00 USD} @ 233.00 USD
            Income:Bitcoin      -1.04347800 USD
            Expenses:Something  81.04345800 USD

        """, entries)

    @loader.load_doc()
    def test_book_conversions_split_reducing(self, entries, errors, __):
        """
          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 *
            Assets:Bank           -500.00 USD
            Assets:Bitcoin       2.000000 BTC @ 250.00 USD

          2015-09-05 *
            Assets:Bank           -520.00 USD
            Assets:Bitcoin       2.000000 BTC @ 260.00 USD

          2015-09-06 *
            Assets:Bank           -540.00 USD
            Assets:Bitcoin       2.000000 BTC @ 270.00 USD

          2015-09-20 *
            Assets:Bitcoin       -5.000000 BTC @ 280.00 USD
            Expenses:Something

        """
        entries = self._convert_and_check_matches(entries)
        self.assertEqualEntries("""

          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 *
            Assets:Bitcoin  2.000000 BTC {250.00 USD} @ 250.00 USD
            Assets:Bank      -500.00 USD

          2015-09-05 *
            Assets:Bitcoin  2.000000 BTC {260.00 USD} @ 260.00 USD
            Assets:Bank      -520.00 USD

          2015-09-06 *
            Assets:Bitcoin  2.000000 BTC {270.00 USD} @ 270.00 USD
            Assets:Bank      -540.00 USD

          2015-09-20 *
            Assets:Bitcoin          -2.000000 BTC {250.00 USD} @ 280.00 USD
            Assets:Bitcoin          -2.000000 BTC {260.00 USD} @ 280.00 USD
            Assets:Bitcoin          -1.000000 BTC {270.00 USD} @ 280.00 USD
            Income:Bitcoin      -110.00000000 USD
            Expenses:Something  1400.00000000 USD

        """, entries)

    @loader.load_doc()
    def test_book_conversions_zero_pnl(self, entries, errors, __):
        """
          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 *
            Assets:Bank           -500.00 USD
            Assets:Bitcoin       2.000000 BTC @ 250.00 USD

          2015-09-20 *
            Assets:Bitcoin       -1.500000 BTC @ 250.00 USD
            Expenses:Something

        """
        entries = self._convert_and_check_matches(entries)
        self.assertEqualEntries("""

          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 *
            Assets:Bitcoin  2.000000 BTC {250.00 USD} @ 250.00 USD
              trades: "trade-17a8973230ee"
            Assets:Bank      -500.00 USD

          2015-09-20 *
            Assets:Bitcoin         -1.500000 BTC {250.00 USD} @ 250.00 USD
              trades: "trade-17a8973230ee"
            Expenses:Something  375.00000000 USD

        """, entries)

    @loader.load_doc(expect_errors=True)
    def test_book_conversions_split_partial_failure(self, entries, errors, __):
        """
          plugin "beancount.plugins.book_conversions" "Assets:Bitcoin,Income:Bitcoin"
          plugin "beancount.plugins.auto_accounts"

          2015-09-04 *
            Assets:Bank           -500.00 USD
            Assets:Bitcoin       2.000000 BTC @ 250.00 USD

          2015-09-20 *
            Assets:Bitcoin      -2.000001 BTC @ 280.00 USD
            Expenses:Something
        """
        self.assertRegex(errors[0].message, "Could not match position")

    @loader.load_doc(expect_errors=True)
    def test_book_conversions_split_complete_failure(self, entries, errors, __):
        """
          plugin "beancount.plugins.book_conversions" "Assets:Bitcoin,Income:Bitcoin"
          plugin "beancount.plugins.auto_accounts"

          2015-09-04 *
            Assets:Bank           -500.00 USD
            Assets:Bitcoin       2.000000 BTC @ 250.00 USD

          2015-09-20 *
            Assets:Bitcoin      -2.000000 BTC @ 280.00 USD
            Expenses:Something

          2015-09-21 *
            Assets:Bitcoin      -0.000001 BTC @ 280.00 USD
            Expenses:Something
        """
        self.assertRegex(errors[0].message, "Could not match position")

    @loader.load_doc(expect_errors=True)
    def test_book_conversions_bad_configuration(self, entries, errors, __):
        """
          plugin "beancount.plugins.book_conversions" "Assets:Bitcoin"
        """
        self.assertRegex(errors[0].message, "Invalid configuration")


class TestExtractTradesScript(unittest.TestCase):

    @test_utils.docfile
    def test_extract_trades(self, filename):
        """
          plugin "beancount.plugins.book_conversions" "Assets:Bitcoin,Income:Bitcoin"

          2015-01-01 open Assets:Bitcoin
          2015-01-01 open Income:Bitcoin
          2015-01-01 open Assets:Bank
          2015-01-01 open Expenses:Something

          2015-09-04 *
            Assets:Bank           -750.00 USD
            Assets:Bitcoin       3.000000 BTC @ 250.00 USD

          2015-09-05 *
            Assets:Bank           -780.00 USD
            Assets:Bitcoin       3.000000 BTC @ 260.00 USD

          2015-09-20 *
            Assets:Bitcoin       -2.000000 BTC @ 300.00 USD
            Expenses:Something

          2015-09-21 *
            Assets:Bitcoin       -2.000000 BTC @ 310.00 USD
            Expenses:Something

          2015-09-22 *
            Assets:Bitcoin       -2.000000 BTC @ 330.00 USD
            Expenses:Something
        """
        with test_utils.capture() as stdout:
            test_utils.run_with_args(book_conversions.main, [filename])
        self.assertEqual(textwrap.dedent("""\
           Units  Currency  Cost Currency    Buy Date  Buy Price   Sell Date  Sell Price     P/L
        --------  --------  -------------  ----------  ---------  ----------  ----------  ------
        2.000000       BTC            USD  2015-09-04     250.00  2015-09-20      300.00  100.00
        1.000000       BTC            USD  2015-09-04     250.00  2015-09-21      310.00   60.00
        1.000000       BTC            USD  2015-09-05     260.00  2015-09-21      310.00   50.00
        2.000000       BTC            USD  2015-09-05     260.00  2015-09-22      330.00  140.00
        --------  --------  -------------  ----------  ---------  ----------  ----------  ------
        """), stdout.getvalue())


if __name__ == '__main__':
    unittest.main()
