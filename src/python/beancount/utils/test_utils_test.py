"""
Tests for test utilities.
"""
import textwrap
import unittest
import re

from beancount.utils import test_utils
from beancount.parser import parser


class TestTestUtils(test_utils.TestCase):

    ledger_text = textwrap.dedent("""

      2014-01-27 * "UNION MARKET -  HOUSNEW YORK / 131129      GROCERY STORE"
        Liabilities:US:Amex:BlueCash                                           -22.02 USD
        Expenses:Food:Grocery

      2014-01-30 * "T-MOBILE RECURNG PMTT-MOBILE / 1093555839 828422957        98006"
        Liabilities:US:Amex:BlueCash                                           -73.64 USD
        Expenses:Communications:Phone:TMobile

      2014-01-30 * "AMAZON SERVICES-KIND866-216-107 / PRYETZFD58N DIGITAL" |
        Liabilities:US:Amex:BlueCash                                           -12.74 USD
        Expenses:Books

      2014-02-01 * "LAFAYETTE STREET PARNEW YORK / 5869184     RESTAURANT" | "Drinks w/ Jo"
        Liabilities:US:Amex:BlueCash                                           -34.40 USD
        Expenses:Food:Alcohol

      2014-02-01 * "SPOTIFY USA         31630876101 / 2714214519  WWW.SPOTIFY.COM"
        Liabilities:US:Amex:BlueCash                                            -9.99 USD
        Expenses:Fun:Music

      2014-02-01 * "BALABOOSTA          214 MULBERR / 2129667366"
        Liabilities:US:Amex:BlueCash                                           -45.65 USD
        Expenses:Food:Restaurant

    """)

    def test_assertEqualEntries(self):
        entries, _, __ = parser.parse_string(self.ledger_text)
        self.assertEqualEntries(entries, self.ledger_text)
        self.assertEqualEntries(self.ledger_text, entries)

        # Try out various modifications and ensure comparison fails.
        mod_ledger_text, _ = re.subn(r' \* ', ' F ', self.ledger_text, 1)
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

    def test_assertEqualEntries(self):
        entries, _, __ = parser.parse_string(self.ledger_text)

        # Check that non-strict inclusion succeeds.
        self.assertIncludesEntries(entries, self.ledger_text)
        self.assertIncludesEntries(self.ledger_text, entries)

        first_two = '\n'.join(self.ledger_text.splitlines()[0:10])
        self.assertIncludesEntries(first_two, entries)

        last_three = '\n'.join(self.ledger_text.splitlines()[-13:])
        self.assertIncludesEntries(first_two, entries)

        with self.assertRaises(AssertionError):
            self.assertIncludesEntries("""

              2014-02-01 * "GOOGLE PLAY"
                Liabilities:US:Amex:BlueCash     -9.99 USD
                Expenses:Fun:Music

            """, entries)

        self.assertIncludesEntries("""

          2014-01-30 * "AMAZON SERVICES-KIND866-216-107 / PRYETZFD58N DIGITAL" |
            Liabilities:US:Amex:BlueCash                                           -12.74 USD
            Expenses:Books

        """, entries)






# FIXME: Missing many functions to be tested still.
__incomplete__ = True
