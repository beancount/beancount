"""
Tests for cmptest base test class.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import re
import textwrap
import unittest

from beancount.core import amount
from beancount.core import data
from beancount.core import position
from beancount.core.number import MISSING
from beancount.parser import booking
from beancount.parser import cmptest
from beancount.parser import parser


class TestCompareTestFunctions(unittest.TestCase):

    def test_read_string_or_entries(self):
        with self.assertRaises(cmptest.TestError) as assctxt:
            cmptest.read_string_or_entries("""

              2014-01-27 * "UNION MARKET"
                Liabilities:US:Amex:BlueCash    -22.02 USD
                Expenses:Food:Grocery

            """)
        self.assertRegex(str(assctxt.exception), "may not use interpolation")

    def test_local_booking(self):
        fileloc = data.new_metadata('<local>', 0)
        date = datetime.date.today()
        txn = data.Transaction(
            fileloc, date, '*', None, "Narration", data.EMPTY_SET, data.EMPTY_SET, [
                data.Posting(
                    'Assets:Something',
                    MISSING,
                    position.CostSpec(MISSING, None, MISSING, MISSING, MISSING, MISSING),
                    MISSING, None, None)])
        expected_txn = data.Transaction(
            fileloc, date, '*', None, "Narration", data.EMPTY_SET, data.EMPTY_SET, [
                data.Posting(
                    'Assets:Something', None,
                    position.Cost(None, None, None, None),
                    None, None, None)])
        actual_txn = cmptest._local_booking(txn)
        self.assertEqual(actual_txn, expected_txn)

        txn = data.Transaction(
            fileloc, date, '*', None, "Narration", data.EMPTY_SET, data.EMPTY_SET, [
                data.Posting(
                    'Assets:Something',
                    amount.Amount(MISSING, MISSING),
                    position.CostSpec(MISSING, None, MISSING, MISSING, MISSING, MISSING),
                    amount.Amount(MISSING, MISSING), None, None)])
        expected_txn = data.Transaction(
            fileloc, date, '*', None, "Narration", data.EMPTY_SET, data.EMPTY_SET, [
                data.Posting(
                    'Assets:Something',
                    amount.Amount(None, None),
                    position.Cost(None, None, None, None),
                    amount.Amount(None, None), None, None)])
        actual_txn = cmptest._local_booking(txn)
        self.assertEqual(actual_txn, expected_txn)


class TestTestCase(cmptest.TestCase):

    ledger_text = textwrap.dedent("""

      2014-01-27 * "UNION MARKET -  HOUSNEW YORK / 131129      GROCERY STORE"
        Liabilities:US:Amex:BlueCash                                           -22.02 USD
        Expenses:Food:Grocery                                                   22.02 USD

      2014-01-30 * "T-MOBILE RECURNG PMTT-MOBILE / 1093555839 828422957        98006"
        Liabilities:US:Amex:BlueCash                                           -73.64 USD
        Expenses:Communications:Phone:TMobile                                   73.64 USD

      2014-01-30 * "AMAZON SERVICES-KIND866-216-107 / PRYETZFD58N DIGITAL"
        Liabilities:US:Amex:BlueCash                                           -12.74 USD
        Expenses:Books                                                          12.74 USD

      2014-02-01 * "LAFAYETTE STREET PARNEW YORK / 5869184     RESTAURANT" "Drinks w/ Jo"
        Liabilities:US:Amex:BlueCash                                           -34.40 USD
        Expenses:Food:Alcohol                                                   34.40 USD

      2014-02-01 * "SPOTIFY USA         31630876101 / 2714214519  WWW.SPOTIFY.COM"
        Liabilities:US:Amex:BlueCash                                            -9.99 USD
        Expenses:Fun:Music                                                       9.99 USD

      2014-02-01 * "BALABOOSTA          214 MULBERR / 2129667366"
        Liabilities:US:Amex:BlueCash                                           -45.65 USD
        Expenses:Food:Restaurant                                                45.65 USD

    """)

    def setUp(self):
        entries, parse_errors, options_map = parser.parse_string(self.ledger_text)
        self.entries, booking_errors = booking.book(entries, options_map)
        self.assertFalse(parse_errors)
        self.assertFalse(booking_errors)

    def test_assertEqualEntries(self):
        entries = self.entries
        self.assertEqualEntries(entries, self.ledger_text)
        self.assertEqualEntries(self.ledger_text, entries)

        # Try out various modifications and ensure comparison fails.
        mod_ledger_text, _ = re.subn(r' \* ', ' ! ', self.ledger_text, 1)
        with self.assertRaises(AssertionError):
            self.assertEqualEntries(entries, mod_ledger_text)

        mod_ledger_text, _ = re.subn(
            r'UNION MARKET', 'WHOLE FOODS MARKET', self.ledger_text, 1)
        with self.assertRaises(AssertionError):
            self.assertEqualEntries(entries, mod_ledger_text)

        mod_ledger_text, _ = re.subn(r'2014-01-27', '2014-01-28', self.ledger_text, 1)
        with self.assertRaises(AssertionError):
            self.assertEqualEntries(entries, mod_ledger_text)

        mod_ledger_text, _ = re.subn(r'73.64 USD', '73.65 USD', self.ledger_text, 1)
        with self.assertRaises(AssertionError):
            self.assertEqualEntries(entries, mod_ledger_text)

        mod_ledger_text, _ = re.subn(r'73.64 USD', '73.64 CAD', self.ledger_text, 1)
        with self.assertRaises(AssertionError):
            self.assertEqualEntries(entries, mod_ledger_text)

        mod_ledger_text, _ = re.subn(
            r'Expenses:Food:Grocery', 'Expenses:Food:Groceries', self.ledger_text, 1)
        with self.assertRaises(AssertionError):
            self.assertEqualEntries(entries, mod_ledger_text)

    def test_assertIncludesEntries(self):
        entries = self.entries

        # Check that non-strict inclusion succeeds.
        self.assertIncludesEntries(entries, self.ledger_text)
        self.assertIncludesEntries(self.ledger_text, entries)

        first_two = '\n'.join(self.ledger_text.splitlines()[0:10])
        self.assertIncludesEntries(first_two, entries)

        last_three = '\n'.join(self.ledger_text.splitlines()[-13:])
        self.assertIncludesEntries(last_three, entries)

        with self.assertRaises(AssertionError):
            self.assertIncludesEntries("""

              2014-02-01 * "PLAY"
                Liabilities:US:Amex:BlueCash     -9.99 USD
                Expenses:Fun:Music                9.99 USD

            """, entries)

        self.assertIncludesEntries("""

          2014-01-30 * "AMAZON SERVICES-KIND866-216-107 / PRYETZFD58N DIGITAL"
            Liabilities:US:Amex:BlueCash        -12.74 USD
            Expenses:Books                       12.74 USD

        """, entries)


    def test_assertExcludesEntries(self):
        entries = self.entries

        # Check that exclusion of all entries fails.
        with self.assertRaises(AssertionError):
            self.assertExcludesEntries(entries, self.ledger_text)
        with self.assertRaises(AssertionError):
            self.assertExcludesEntries(self.ledger_text, entries)

        # Check that partial exclusion of all entries fails.
        first_two = '\n'.join(self.ledger_text.splitlines()[0:10])
        with self.assertRaises(AssertionError):
            self.assertExcludesEntries(first_two, entries)

        self.assertExcludesEntries("""

          2014-02-01 * "PLAY"
            Liabilities:US:Amex:BlueCash     -9.99 USD
            Expenses:Fun:Music                9.99 USD

        """, entries)

        with self.assertRaises(AssertionError):
            self.assertExcludesEntries("""

              2014-01-30 * "AMAZON SERVICES-KIND866-216-107 / PRYETZFD58N DIGITAL"
                Liabilities:US:Amex:BlueCash             -12.74 USD
                Expenses:Books                            12.74 USD

            """, entries)


if __name__ == '__main__':
    unittest.main()
