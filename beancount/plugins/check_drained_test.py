__copyright__ = "Copyright (C) 2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.parser import cmptest


class TestCheckCloseEmpty(cmptest.TestCase):
    @loader.load_doc()
    def test_check_drained__absence(self, entries, errors, options_map):
        """
        2018-01-01 open Assets:Something:Cash
        2018-01-01 open Income:Donations
        2018-01-01 open Expenses:Splurge

        2018-02-16 * "Do something"
          Income:Donations
          Assets:Something:Cash    1.00 USD

        2019-01-01 close Assets:Something:Cash
        """
        self.assertFalse(errors)

    @loader.load_doc()
    def test_check_drained__seen_currencies(self, entries, _, options_map):
        """
        plugin "beancount.plugins.check_drained"

        2018-01-01 open Assets:Something:Cash
        2018-01-01 open Income:Donations
        2018-01-01 open Expenses:Splurge

        2018-02-16 * "Do something"
          Income:Donations
          Assets:Something:Cash    1.00 USD

        2018-02-17 * "Do something in another currency"
          Income:Donations
          Assets:Something:Cash    1.00 CAD

        2018-12-25 * "Spend it all"
          Assets:Something:Cash   -1.00 USD
          Assets:Something:Cash   -1.00 CAD
          Expenses:Splurge

        2019-01-01 close Assets:Something:Cash
        """
        self.assertEqual(7 + 2, len(entries))
        self.assertIncludesEntries(
            """
          2019-01-02 balance Assets:Something:Cash  0 USD
          2019-01-02 balance Assets:Something:Cash  0 CAD
        """,
            entries,
        )

    @loader.load_doc()
    def test_check_drained__declared_currencies(self, entries, _, options_map):
        """
        plugin "beancount.plugins.check_drained"

        2018-01-01 open Assets:Something:Cash  USD,CAD
        2018-01-01 open Income:Donations
        2018-01-01 open Expenses:Splurge

        2019-01-01 close Assets:Something:Cash
        """
        self.assertEqual(4 + 2, len(entries))
        self.assertIncludesEntries(
            """
          2019-01-02 balance Assets:Something:Cash  0 USD
          2019-01-02 balance Assets:Something:Cash  0 CAD
        """,
            entries,
        )

    @loader.load_doc(expect_errors=True)
    def test_check_drained__error(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.check_drained"

        2018-01-01 open Assets:Something:Cash  USD,CAD
        2018-01-01 open Income:Donations
        2018-01-01 open Expenses:Splurge

        2018-02-16 * "Do something"
          Income:Donations
          Assets:Something:Cash    1.00 USD

        2019-01-01 close Assets:Something:Cash
        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Balance failed for .*: expected 0 USD")

    @loader.load_doc()
    def test_check_drained__skip_preexisting(self, entries, _, options_map):
        """
        plugin "beancount.plugins.check_drained"

        2018-01-01 open Assets:Something:Cash  USD,CAD
        2018-01-01 open Income:Donations
        2018-01-01 open Expenses:Splurge

        2018-02-16 * "Do something"
          Income:Donations
          Assets:Something:Cash    1.00 USD

        2019-01-01 balance Assets:Something:Cash   1.00 USD

        2019-01-01 close Assets:Something:Cash
        """
        self.assertEqual(6 + 1, len(entries))
        self.assertIncludesEntries(
            """
          2019-01-02 balance Assets:Something:Cash  0 CAD
        """,
            entries,
        )
        self.assertExcludesEntries(
            """
          2019-01-01 balance Assets:Something:Cash  0 USD
          2019-01-02 balance Assets:Something:Cash  0 USD
        """,
            entries,
        )

    @loader.load_doc(expect_errors=True)
    def test_check_drained__same_day(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.check_drained"

        2018-01-01 open Assets:Something:Cash  USD,CAD
        2018-01-01 open Income:Donations
        2018-01-01 open Expenses:Splurge

        2018-02-16 * "Do something"
          Income:Donations
          Assets:Something:Cash    1.00 USD

        2018-02-16 close Assets:Something:Cash
        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Balance failed for .*: expected 0 USD")

    @loader.load_doc(expect_errors=True)
    def test_check_drained__not_balance_sheet(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.check_drained"

        2018-01-01 open Liabilities:Credit
        2018-01-01 open Income:Donations
        2018-01-01 open Expenses:Splurge

        2018-02-16 * "Do something"
          Income:Donations               -2.00 USD
          Liabilities:Credit              1.00 USD
          Expenses:Splurge                1.00 USD

        2018-03-01 close Liabilities:Credit
        2018-03-01 close Income:Donations
        2018-03-01 close Expenses:Splurge
        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Balance failed for .*Liabilities:Credit")

    @loader.load_doc()
    def test_check_drained__with_cost_basis(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.check_drained"

        2018-01-01 open Assets:Investing
        2018-01-01 open Income:Somewhere
        2018-01-01 open Expenses:Somewhere

        2018-02-16 * "Do something"
          Income:Somewhere               -1000.00 USD
          Assets:Investing              25 HOOL {40.00 USD}

        2018-02-16 * "Do something"
          Expenses:Somewhere               1100.00 USD
          Assets:Investing              -25 HOOL {40.00 USD}
          Income:Somewhere            -100.00 USD

        2018-03-01 close Assets:Investing
        """
        self.assertEqual(6 + 1, len(entries))
        self.assertIncludesEntries(
            """
              2018-03-02 balance Assets:Investing       0 HOOL
        """,
            entries,
        )


if __name__ == "__main__":
    unittest.main()
