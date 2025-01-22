"""Unit tests for treeify tool."""

__copyright__ = "Copyright (C) 2014-2017, 2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import subprocess
import sys
import textwrap
import unittest

PROGRAM = __file__.replace("_test.py", ".py")
DEBUG = 0


def treeify(string, options=None):
    """Run treeify on the string.

    Args:
      string: The input string to feed treeify.
      options: An optional list of options for the subprogram.
    Returns:
      The treeified string.
    """
    pipe = subprocess.Popen(
        [sys.executable, PROGRAM] + (options or []),
        shell=False,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    output, errors = pipe.communicate(string.encode("utf-8"))
    return (
        pipe.returncode,
        output.decode("utf-8") if output else "",
        errors.decode("utf-8") if errors else "",
    )


class TestTreeifyBase(unittest.TestCase):
    maxDiff = 8192

    def treeify(self, string, expect_errors=False, options=None):
        """Run treeify on the given string and assert no errors.

        Args:
          string: A string, the input contents to run on.
          expect_errors: A boolean, true if we should expect there to be errors.
          options: An optional list of options for the subprogram.
        Returns:
          The converted output string. This fails the test if there were any
          errors.
        """
        returncode, output, errors = treeify(string, options)
        actual_errors = returncode != 0 or bool(errors)
        if actual_errors != expect_errors:
            if expect_errors:
                self.fail("Missing expected errors")
            else:
                self.fail("Unexpected errors: {}".format(errors))
        return output

    def treeify_equal(self, string, expected, expect_errors=False, options=None):
        """Assert an expected treeification result.

        Args:
          string: An expected input contents string to treeify.
          expected: A string, the expected treeified output.
          expect_errors: A boolean, true if we should expect there to be errors.
          options: An optional list of options for the subprogram.
        Returns:
          The actual output string. This fails the test if there the expected
          output differed from the actual.
        """
        input_ = textwrap.dedent(string)
        output = self.treeify(input_, expect_errors, options)
        expected = textwrap.dedent(expected)
        if DEBUG:
            print("-(input)----------------------------------")
            print(input_)
            print("-(output)---------------------------------")
            print(output)
            print("-(expected)-------------------------------")
            print(expected)
            print("------------------------------------------")
        self.assertEqual(expected.splitlines(), output.splitlines())
        return output


class TestTreeify(TestTreeifyBase):
    def test_simple(self):
        self.treeify_equal(
            """\
          2014-12-25 Assets:US:BofA:Checking                        5,545.01 USD
          2014-11-11 Assets:US:Federal:PreTax401k
          2014-10-04 Assets:US:Hooli:Vacation                         332.64 VACHR
          2014-11-07 Assets:US:Investment:Cash                     26,500.00 USD
          2014-12-15 Assets:US:Vanguard:Cash                           -0.07 USD
          2014-12-10 Assets:US:Vanguard:RGAGX                         174.22 RGAGX
          2014-10-19 Assets:US:Vanguard:VBMPX                         189.03 VBMPX
          2014-10-17 Equity:Opening-Balances                       -3,188.28 USD
          2014-12-12 Expenses:Food:Groceries                        6,483.71 USD
          2014-12-06 Expenses:Food:Restaurant                      10,990.74 USD
          2014-11-30 Expenses:Health:Dental:Insurance                 208.80 USD
          2014-11-09 Expenses:Health:Life:GroupTermLife             1,751.04 USD
          2014-12-07 Expenses:Health:Medical:Insurance              1,971.36 USD
          2014-10-12 Expenses:Health:Vision:Insurance               3,045.60 USD
          2014-12-28 Expenses:Home:Electricity                      2,080.00 USD
          2014-10-22 Expenses:Home:Internet                         2,560.22 USD
        """,
            """\
                     |-- Assets
                     |   `-- US
                     |       |-- BofA
          2014-12-25 |       |   `-- Checking                       5,545.01 USD
                     |       |-- Federal
          2014-11-11 |       |   `-- PreTax401k
                     |       |-- Hooli
          2014-10-04 |       |   `-- Vacation                         332.64 VACHR
                     |       |-- Investment
          2014-11-07 |       |   `-- Cash                          26,500.00 USD
                     |       `-- Vanguard
          2014-12-15 |           |-- Cash                              -0.07 USD
          2014-12-10 |           |-- RGAGX                            174.22 RGAGX
          2014-10-19 |           `-- VBMPX                            189.03 VBMPX
                     |-- Equity
          2014-10-17 |   `-- Opening-Balances                      -3,188.28 USD
                     `-- Expenses
                         |-- Food
          2014-12-12     |   |-- Groceries                          6,483.71 USD
          2014-12-06     |   `-- Restaurant                        10,990.74 USD
                         |-- Health
                         |   |-- Dental
          2014-11-30     |   |   `-- Insurance                        208.80 USD
                         |   |-- Life
          2014-11-09     |   |   `-- GroupTermLife                  1,751.04 USD
                         |   |-- Medical
          2014-12-07     |   |   `-- Insurance                      1,971.36 USD
                         |   `-- Vision
          2014-10-12     |       `-- Insurance                      3,045.60 USD
                         `-- Home
          2014-12-28         |-- Electricity                        2,080.00 USD
          2014-10-22         `-- Internet                           2,560.22 USD
        """,
        )

    def test_empty_string(self):
        self.treeify_equal(
            """\
        """,
            """\
        """,
            True,
        )

    def test_no_columns(self):
        self.treeify_equal(
            """\

          Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
          eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
          minim veniam, quis nostrud exercitation ullamco laboris nisi ut
          aliquip ex ea commodo consequat. Duis aute irure dolor in
          reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
          pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
          culpa qui officia deserunt mollit anim id est laborum.

        """,
            """\

          Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
          eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
          minim veniam, quis nostrud exercitation ullamco laboris nisi ut
          aliquip ex ea commodo consequat. Duis aute irure dolor in
          reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
          pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
          culpa qui officia deserunt mollit anim id est laborum.

        """,
            True,
        )

    def test_flush_left(self):
        self.treeify_equal(
            """\
          Assets:US:Vanguard:Cash                           -0.07 USD
          Assets:US:Vanguard:RGAGX                         174.22 RGAGX
          Assets:US:Vanguard:VBMPX                         189.03 VBMPX
          Equity:Opening-Balances                       -3,188.28 USD
          Expenses:Food:Groceries                        6,483.71 USD
          Expenses:Food:Restaurant                      10,990.74 USD
          Expenses:Health:Dental:Insurance                 208.80 USD
          Expenses:Health:Life:GroupTermLife             1,751.04 USD
        """,
            """\
          |-- Assets
          |   `-- US
          |       `-- Vanguard
          |           |-- Cash                              -0.07 USD
          |           |-- RGAGX                            174.22 RGAGX
          |           `-- VBMPX                            189.03 VBMPX
          |-- Equity
          |   `-- Opening-Balances                      -3,188.28 USD
          `-- Expenses
              |-- Food
              |   |-- Groceries                          6,483.71 USD
              |   `-- Restaurant                        10,990.74 USD
              `-- Health
                  |-- Dental
                  |   `-- Insurance                        208.80 USD
                  `-- Life
                      `-- GroupTermLife                  1,751.04 USD
        """,
            False,
        )

    def test_flush_right(self):
        self.treeify_equal(
            """\
          2014-01-01 Assets:US:Vanguard:Cash
          2014-01-01 Assets:US:Vanguard:RGAGX
          2014-01-01 Assets:US:Vanguard:VBMPX
          2014-01-01 Equity:Opening-Balances
          2014-01-01 Expenses:Food:Groceries
          2014-01-01 Expenses:Food:Restaurant
          2014-01-01 Expenses:Health:Dental:Insurance
          2014-01-01 Expenses:Health:Life:GroupTermLife
        """,
            """\
                     |-- Assets
                     |   `-- US
                     |       `-- Vanguard
          2014-01-01 |           |-- Cash
          2014-01-01 |           |-- RGAGX
          2014-01-01 |           `-- VBMPX
                     |-- Equity
          2014-01-01 |   `-- Opening-Balances
                     `-- Expenses
                         |-- Food
          2014-01-01     |   |-- Groceries
          2014-01-01     |   `-- Restaurant
                         `-- Health
                             |-- Dental
          2014-01-01         |   `-- Insurance
                             `-- Life
          2014-01-01             `-- GroupTermLife
        """,
            False,
        )

    def test_two_columns(self):
        self.treeify_equal(
            """\
          2014-01-01 Assets:US:Vanguard:Cash              | Assets:US:Vanguard:Cash            XX
          2014-01-01 Assets:US:Vanguard:RGAGX             | Assets:US:Vanguard:RGAGX           XX
          2014-01-01 Assets:US:Vanguard:VBMPX             | Assets:US:Vanguard:VBMPX           XX
          2014-01-01 Equity:Opening-Balances              | Equity:Opening-Balances            XX
          2014-01-01 Expenses:Food:Groceries              | Expenses:Food:Groceries            XX
          2014-01-01 Expenses:Food:Restaurant             | Expenses:Food:Restaurant           XX
          2014-01-01 Expenses:Health:Dental:Insurance     | Expenses:Health:Dental:Insurance   XX
          2014-01-01 Expenses:Health:Life:GroupTermLife   | Expenses:Health:Life:GroupTermLife XX
        """,
            """\
                     |-- Assets
                     |   `-- US
                     |       `-- Vanguard
          2014-01-01 |           |-- Cash                 | Assets:US:Vanguard:Cash            XX
          2014-01-01 |           |-- RGAGX                | Assets:US:Vanguard:RGAGX           XX
          2014-01-01 |           `-- VBMPX                | Assets:US:Vanguard:VBMPX           XX
                     |-- Equity
          2014-01-01 |   `-- Opening-Balances             | Equity:Opening-Balances            XX
                     `-- Expenses
                         |-- Food
          2014-01-01     |   |-- Groceries                | Expenses:Food:Groceries            XX
          2014-01-01     |   `-- Restaurant               | Expenses:Food:Restaurant           XX
                         `-- Health
                             |-- Dental
          2014-01-01         |   `-- Insurance            | Expenses:Health:Dental:Insurance   XX
                             `-- Life
          2014-01-01             `-- GroupTermLife        | Expenses:Health:Life:GroupTermLife XX
        """,
            False,
        )

    def test_overlapping_column(self):
        self.treeify_equal(
            """\
          Assets:US:Vanguard:Cash            100.00 USD
          Assets:US:Vanguard:RGAGX           100.00 USD
          Assets:US:Vanguard:VBMPX           100.00 USD
          Equity:Opening-Balances          100.00 USD <- overlapping
          Expenses:Food:Groceries            100.00 USD
          Expenses:Food:Restaurant           100.00 USD
          Expenses:Health:Dental:Insurance   100.00 USD
          Expenses:Health:Life:GroupTermLife 100.00 USD
        """,
            """\
          Assets:US:Vanguard:Cash            100.00 USD
          Assets:US:Vanguard:RGAGX           100.00 USD
          Assets:US:Vanguard:VBMPX           100.00 USD
          Equity:Opening-Balances          100.00 USD <- overlapping
          Expenses:Food:Groceries            100.00 USD
          Expenses:Food:Restaurant           100.00 USD
          Expenses:Health:Dental:Insurance   100.00 USD
          Expenses:Health:Life:GroupTermLife 100.00 USD
        """,
            True,
        )

    def test_parents(self):
        self.treeify_equal(
            """\
          Assets:US                          100.00 USD
          Assets:US:Vanguard                 101.00 USD
          Assets:US:Vanguard:VBMPX           102.00 USD
        """,
            """\
          `-- Assets
              `-- US                         100.00 USD
                  `-- Vanguard               101.00 USD
                      `-- VBMPX              102.00 USD
        """,
            False,
        )

    def test_unsorted(self):
        # Check when the same parent comes around multiple times (the column
        # values aren't sorted properly).
        self.treeify_equal(
            """\
          Assets:US:Vanguard:VBMPX           100.00 USD
          Assets:CA:Bank:Checking            101.00 USD
          Assets:US:Vanguard:VBMPX           102.00 USD
          Assets:ES:Caja                     103.00 USD
          Assets:US:Vanguard:VBMPX           104.00 USD
        """,
            """\
         `-- Assets
             |-- US
             |   `-- Vanguard
             |       `-- VBMPX              100.00 USD
             |-- CA
             |   `-- Bank
             |       `-- Checking           101.00 USD
             |-- US
             |   `-- Vanguard
             |       `-- VBMPX              102.00 USD
             |-- ES
             |   `-- Caja                   103.00 USD
             `-- US
                 `-- Vanguard
                     `-- VBMPX              104.00 USD
        """,
            False,
        )

    def test_consecutive(self):
        self.treeify_equal(
            """\
          Assets:US:Vanguard:VBMPX           100.00 USD
          Assets:US:Vanguard:VBMPX           100.00 USD
          Assets:US:Vanguard:VBMPX           102.00 USD
          Assets:US:Vanguard:VBMPX           104.00 USD
        """,
            """\
          `-- Assets
              `-- US
                  `-- Vanguard
                      `-- VBMPX              100.00 USD
                                             100.00 USD
                                             102.00 USD
                                             104.00 USD
        """,
            False,
        )

    def test_noise_before(self):
        self.treeify_equal(
            """\
          Account                         Balance
          ---------------------------------------------
          Assets:US:Vanguard:VBMPX     100.00 USD
          Assets:US:Vanguard:VBMPX     101.00 USD
          Expenses:Food:Groceries      102.00 USD
        """,
            """\
          Account                         Balance
          ---------------------------------------------
          |-- Assets
          |   `-- US
          |       `-- Vanguard
          |           `-- VBMPX        100.00 USD
          |                            101.00 USD
          `-- Expenses
              `-- Food
                  `-- Groceries        102.00 USD
        """,
            False,
        )

    def test_noise_after(self):
        self.treeify_equal(
            """\
          Assets:US:Vanguard:VBMPX     100.00 USD
          Assets:US:Vanguard:VBMPX     101.00 USD
          Expenses:Food:Groceries      102.00 USD
          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          Not matching: yep, not matching.
        """,
            """\
          |-- Assets
          |   `-- US
          |       `-- Vanguard
          |           `-- VBMPX        100.00 USD
          |                            101.00 USD
          `-- Expenses
              `-- Food
                  `-- Groceries        102.00 USD
          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          Not matching: yep, not matching.
        """,
            False,
        )

    def test_noise_middle_between_nodes(self):
        self.treeify_equal(
            """\
          Assets:US:Vanguard:VBMPX     100.00 USD
          Assets:US:Vanguard:VBMPX     101.00 USD
          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          Expenses:Food:Groceries      102.00 USD
        """,
            """\
          |-- Assets
          |   `-- US
          |       `-- Vanguard
          |           `-- VBMPX        100.00 USD
          |                            101.00 USD
          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          `-- Expenses
              `-- Food
                  `-- Groceries        102.00 USD
        """,
            False,
        )

    def test_noise_middle_same_node(self):
        self.treeify_equal(
            """\
          Assets:US:Vanguard:VBMPX     100.00 USD
          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          Assets:US:Vanguard:VBMPX     101.00 USD
          Expenses:Food:Groceries      102.00 USD
        """,
            """\
          |-- Assets
          |   `-- US
          |       `-- Vanguard
          |           `-- VBMPX        100.00 USD
          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          |                            101.00 USD
          `-- Expenses
              `-- Food
                  `-- Groceries        102.00 USD
        """,
            False,
        )

    def test_noise_middle_parent_child(self):
        self.treeify_equal(
            """\
          Assets:US:Vanguard           100.00 USD
          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          Assets:US:Vanguard:VBMPX     101.00 USD
          Expenses:Food:Groceries      102.00 USD
        """,
            """\
          |-- Assets
          |   `-- US
          |       `-- Vanguard         100.00 USD
          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          |           `-- VBMPX        101.00 USD
          `-- Expenses
              `-- Food
                  `-- Groceries        102.00 USD
        """,
            False,
        )

    def test_filenames(self):
        self.treeify_equal(
            """\
          -rwxr-xr-x    1 blais   5000  44708 Sep 27 13:54 ./beancount/scripts/example.py
          -rw-r--r--    1 blais   5000    678 Sep 21 12:11 ./beancount/scripts/example_test.py
          -rwxr-xr-x    1 blais   5000   3321 Sep 23 14:30 ./beancount/scripts/format.py
          -rw-r--r--    1 blais   5000   1341 Sep 23 14:30 ./beancount/scripts/format_test.py
          -rwxr-xr-x    1 blais   5000   8714 Sep 20 13:09 ./beancount/scripts/query.py
          -rw-r--r--    1 blais   5000   4796 Sep  3 07:26 ./beancount/scripts/query_test.py
          drwxr-xr-x   18 blais   5000    612 Sep 29 22:18 ./beancount/utils
          -rw-r--r--    1 blais   5000     48 Jun 15 11:53 ./beancount/utils/__init__.py
          -rw-r--r--    1 blais   5000   1464 Sep  3 07:26 ./beancount/utils/bisect_key.py
          -rw-r--r--    1 blais   5000   1330 Sep  3 07:26 ./beancount/utils/bisect_key_test.py
          -rw-r--r--    1 blais   5000   1203 Aug 17 13:06 ./beancount/utils/file_utils.py
          -rw-r--r--    1 blais   5000   1130 Aug 17 13:06 ./beancount/utils/file_utils_test.py
          -rw-r--r--    1 blais   5000   8377 Sep 15 06:53 ./beancount/utils/misc_utils.py
          -rw-r--r--    1 blais   5000   4783 Sep 13 15:17 ./beancount/utils/misc_utils_test.py
        """,
            """\
                                                           `-- .
                                                               `-- beancount
                                                                   |-- scripts
          -rwxr-xr-x    1 blais   5000  44708 Sep 27 13:54         |   |-- example.py
          -rw-r--r--    1 blais   5000    678 Sep 21 12:11         |   |-- example_test.py
          -rwxr-xr-x    1 blais   5000   3321 Sep 23 14:30         |   |-- format.py
          -rw-r--r--    1 blais   5000   1341 Sep 23 14:30         |   |-- format_test.py
          -rwxr-xr-x    1 blais   5000   8714 Sep 20 13:09         |   |-- query.py
          -rw-r--r--    1 blais   5000   4796 Sep  3 07:26         |   `-- query_test.py
          drwxr-xr-x   18 blais   5000    612 Sep 29 22:18         `-- utils
          -rw-r--r--    1 blais   5000     48 Jun 15 11:53             |-- __init__.py
          -rw-r--r--    1 blais   5000   1464 Sep  3 07:26             |-- bisect_key.py
          -rw-r--r--    1 blais   5000   1330 Sep  3 07:26             |-- bisect_key_test.py
          -rw-r--r--    1 blais   5000   1203 Aug 17 13:06             |-- file_utils.py
          -rw-r--r--    1 blais   5000   1130 Aug 17 13:06             |-- file_utils_test.py
          -rw-r--r--    1 blais   5000   8377 Sep 15 06:53             |-- misc_utils.py
          -rw-r--r--    1 blais   5000   4783 Sep 13 15:17             `-- misc_utils_test.py
        """,
            False,
            options=["--pattern=([^ ]+)(/[^ ]+)+", "--split=/"],
        )

    def test_filenames_tree(self):
        self.treeify_equal(
            """\
          -rwxr-xr-x    1 blais   5000  44708 Sep 27 13:54 ./beancount
          -rw-r--r--    1 blais   5000    678 Sep 21 12:11 ./beancount/scripts
          -rwxr-xr-x    1 blais   5000   3321 Sep 23 14:30 ./beancount/scripts/format.py
        """,
            """\
                                                           `-- .
          -rwxr-xr-x    1 blais   5000  44708 Sep 27 13:54     `-- beancount
          -rw-r--r--    1 blais   5000    678 Sep 21 12:11         `-- scripts
          -rwxr-xr-x    1 blais   5000   3321 Sep 23 14:30             `-- format.py
        """,
            False,
            options=["--pattern=([^ ]+)(/[^ ]+)+", "--split=/"],
        )

    def test_width_wider(self):
        # The treeified column should be wider as it needs to.
        self.treeify_equal(
            """\
          Equity:B       100.00 USD
          Equity:B:C     101.00 USD
          Equity:B:C:D   102.00 USD
        """,
            """\
          `-- Equity
              `-- B           100.00 USD
                  `-- C       101.00 USD
                      `-- D   102.00 USD
        """,
            False,
        )

    def test_width_narrower(self):
        # The treeified column should be of the same width even though it does
        # not need to.
        self.treeify_equal(
            """\
          Assets:Bcdefg                100.00 USD
          Assets:Bcdefg:Cdefgh         101.00 USD
          Assets:Bcdefg:Cdefgh:Defghij 102.00 USD
        """,
            """\
          `-- Assets
              `-- Bcdefg               100.00 USD
                  `-- Cdefgh           101.00 USD
                      `-- Defghij      102.00 USD
        """,
            False,
        )

    def test_whitespace(self):
        # Test with columns that have whitespace in them.
        self.treeify_equal(
            "Assets:Bank:DE:Bankname:Tagesgeld         3.57 EUR\n"
            "Equity:Opening-Balances                  -3.57 EUR\n"
            "Expenses                          \n"
            "Income                            \n"
            "Liabilities                       \n",
            """\
             |-- Assets
             |   `-- Bank
             |       `-- DE
             |           `-- Bankname
             |               `-- Tagesgeld             3.57 EUR
             |-- Equity
             |   `-- Opening-Balances                 -3.57 EUR
             |-- Expenses
             |-- Income
             `-- Liabilities
        """,
            False,
        )


if __name__ == "__main__":
    unittest.main()
