"""
Tests for test utilities.
"""
import textwrap
import unittest
import re
import os
import sys
from os import path

from beancount.utils import test_utils
from beancount.parser import parser


class TestTestUtils(unittest.TestCase):

    def test_run_with_args(self):
        sentinel = []
        def main():
            sentinel.append(sys.argv)
        test_utils.run_with_args(main, ['a', 'b', 'c'])
        self.assertEqual(1, len(sentinel))
        sys_argv = sentinel[0]
        self.assertTrue(sys_argv[0].endswith('beancount/utils/test_utils_test.py'))
        self.assertEqual(['a', 'b', 'c'], sys_argv[1:])

    def test_tempdir(self):
        with test_utils.tempdir() as tempdir:
            open(path.join(tempdir, 'file1'), 'w')
            os.mkdir(path.join(tempdir, 'directory'))
            open(path.join(tempdir, 'directory', 'file2'), 'w')
        self.assertFalse(path.exists(tempdir))
        self.assertFalse(path.exists(path.join(tempdir, 'file1')))
        self.assertFalse(path.exists(path.join(tempdir, 'directory')))

    def test_capture(self):
        text = "b9baaa0c-0f0a-47db-bffc-a00c6f4ac1db"
        with test_utils.capture() as output:
            print(text)
        self.assertEqual(text + "\n", output.getvalue())

    @test_utils.docfile
    def test_docfile(self, filename):
        "7f9034b1-51e7-420c-ac6b-945b5c594ebf"
        self.assertEqual("7f9034b1-51e7-420c-ac6b-945b5c594ebf",
                         open(filename).read())


class TestTestCase(test_utils.TestCase):

    def test_assertLines(self):
        self.assertLines("""
           43c62bff-8504-44ea-b5c0-afa218a7a973
           95ef1cc4-0016-4452-9f4e-1a053db2bc83
        """, """

             43c62bff-8504-44ea-b5c0-afa218a7a973
               95ef1cc4-0016-4452-9f4e-1a053db2bc83

        """)

        with self.assertRaises(AssertionError):
            self.assertLines("""
               43c62bff-8504-44ea-b5c0-afa218a7a973
            """, """
                683f111f-f921-4db3-a3e8-daae344981e8
            """)

    def test_assertOutput(self):
        with self.assertOutput("""
           3165efbc-c775-4503-be13-06b7167697a9
        """):
            print('3165efbc-c775-4503-be13-06b7167697a9')

        with self.assertRaises(AssertionError):
            with self.assertOutput("""
               3165efbc-c775-4503-be13-06b7167697a9
            """):
                print('78d58502a15e')


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
