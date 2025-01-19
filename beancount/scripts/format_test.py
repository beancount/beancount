__copyright__ = "Copyright (C) 2014-2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import textwrap
import unittest

from beancount.scripts import format
from beancount.utils import test_utils


class TestScriptFormat(test_utils.ClickTestCase):
    @test_utils.docfile
    def test_success(self, filename):
        """

        * Section header

        ;; Accounts (comments)
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash

        2014-03-05 balance   Assets:Cash  -50.02 USD

        2014-03-10 * "Something"
          Assets:Other   10 HOOL {500.23} USD ; Bla
          Assets:Cash

        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""

          * Section header

          ;; Accounts (comments)
          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash

          2014-03-02 * "Something"
            Expenses:Restaurant              50.02 USD
            Assets:Cash

          2014-03-05 balance   Assets:Cash  -50.02 USD

          2014-03-10 * "Something"
            Assets:Other                        10 HOOL {500.23} USD ; Bla
            Assets:Cash

        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_align_posting_starts(self, filename):
        """
        2014-03-01 * "Something"
          Expenses:Restaurant   50.01 USD
          Assets:Cash

        2014-03-02 * "Something"
         Expenses:Restaurant    50.02 USD
            Assets:Cash

        2014-03-03 * "Something"
          Expenses:Restaurant   50.03 USD
          Assets:Cash
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""
          2014-03-01 * "Something"
            Expenses:Restaurant  50.01 USD
            Assets:Cash

          2014-03-02 * "Something"
            Expenses:Restaurant  50.02 USD
            Assets:Cash

          2014-03-03 * "Something"
            Expenses:Restaurant  50.03 USD
            Assets:Cash
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_open_only_issue80(self, filename):
        """
        2015-07-16 open Assets:BoA:checking USD
        """
        _result = self.run_with_args(format.main, filename)
        with open(filename, encoding="utf-8") as infile:
            actual = infile.read()
        self.assertEqual(
            """
          2015-07-16 open Assets:BoA:checking USD
        """.strip(),
            actual.strip(),
        )

    @test_utils.docfile
    def test_commas(self, filename):
        """

        * Section header

        ;; Accounts (comments)
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   1,050.02 USD
          Assets:Cash

        2014-03-05 balance   Assets:Cash  -1,050.02 USD

        2014-03-10 * "Something"
          Assets:Other   10 HOOL {5,000.23 USD} ; Bla
          Assets:Cash

        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""

          * Section header

          ;; Accounts (comments)
          2013-01-01 open Expenses:Restaurant
          2013-01-01 open Assets:Cash

          2014-03-02 * "Something"
            Expenses:Restaurant              1,050.02 USD
            Assets:Cash

          2014-03-05 balance   Assets:Cash  -1,050.02 USD

          2014-03-10 * "Something"
            Assets:Other                           10 HOOL {5,000.23 USD} ; Bla
            Assets:Cash

        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_currency_issue146(self, filename):
        """
        1970-01-01 open Equity:Opening-balances
        1970-01-01 open Assets:Investments

        2014-03-31 * "opening"
          Assets:Investments                 1.23 FOO_BAR
          Equity:Opening-balances
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""
          1970-01-01 open Equity:Opening-balances
          1970-01-01 open Assets:Investments

          2014-03-31 * "opening"
            Assets:Investments  1.23 FOO_BAR
            Equity:Opening-balances
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_fixed_width(self, filename):
        """
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test     10.00 USD
          Assets:Test
        """
        result = self.run_with_args(format.main, filename, "--prefix-width=40")
        self.assertEqual(
            textwrap.dedent("""
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test                           10.00 USD
          Assets:Test
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_fixed_column(self, filename):
        """
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test
        2016-08-01 balance Assets:Test  0.00 USD

        2016-08-02 * "" ""
          Expenses:Test     10.00 USD
          Assets:Test
        """
        result = self.run_with_args(format.main, filename, "--currency-column=50")
        self.assertEqual(
            textwrap.dedent("""
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test
        2016-08-01 balance Assets:Test              0.00 USD

        2016-08-02 * "" ""
          Expenses:Test                            10.00 USD
          Assets:Test
        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_metadata_issue400(self, filename):
        """
        2020-01-01 open Assets:Test

        2020-11-10 * Test
          payment_amount: 20.00 EUR
          Assets:Test   10.00 EUR
          Assets:Test  -10.00 EUR
        """
        result = self.run_with_args(format.main, filename, "--currency-column=50")
        self.assertEqual(
            textwrap.dedent("""
        2020-01-01 open Assets:Test

        2020-11-10 * Test
          payment_amount: 20.00 EUR
          Assets:Test                              10.00 EUR
          Assets:Test                             -10.00 EUR
        """),
            result.stdout,
        )

    @unittest.skip(
        "Eventually we will want to support arithmetic expressions. "
        "It will require to invoke the expression parser because "
        "expressions are not guaranteed to be surrounded by matching "
        "parentheses."
    )
    @test_utils.docfile
    def test_arithmetic_expressions(self, filename):
        """
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test     10.0/2 USD
          Assets:Test
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""

        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test     10.0/2 USD
          Assets:Test

        """),
            result.stdout,
        )

    @test_utils.docfile
    def test_parenthesized_binary_expressions(self, filename):
        """
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test     10.0 USD
          Expenses:Test (10.0/2) USD
          Expenses:Test (10.0+2) USD
          Expenses:Test (10.0-2) USD
          Expenses:Test (10.0*2) USD
          Expenses:Test (-10.0*+2) USD
          Expenses:Test (-1,000.0*+2.0) USD
          Assets:Test

        2016-08-03 balance Assets:Test 12.27 USD
        2016-08-03 balance Assets:Test (12.27 + 1.00) USD
        """
        result = self.run_with_args(format.main, filename)
        self.assertEqual(
            textwrap.dedent("""
        2016-08-01 open Expenses:Test
        2016-08-01 open Assets:Test

        2016-08-02 * "" ""
          Expenses:Test                            10.0 USD
          Expenses:Test                        (10.0/2) USD
          Expenses:Test                        (10.0+2) USD
          Expenses:Test                        (10.0-2) USD
          Expenses:Test                        (10.0*2) USD
          Expenses:Test                      (-10.0*+2) USD
          Expenses:Test                 (-1,000.0*+2.0) USD
          Assets:Test

        2016-08-03 balance Assets:Test            12.27 USD
        2016-08-03 balance Assets:Test   (12.27 + 1.00) USD
        """),
            result.stdout,
        )


if __name__ == "__main__":
    unittest.main()
