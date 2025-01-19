__copyright__ = "Copyright (C) 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.core import data
from beancount.parser import cmptest
from beancount.plugins import currency_accounts


class TestCurrencyTradingAccounts(cmptest.TestCase):
    @loader.load_doc()
    def test_selection(self, entries, errors, _):
        """
        plugin "beancount.plugins.currency_accounts" ""

        2018-01-01 open Assets:Checking
        2018-01-01 open Income:Salary

        2018-03-01 * ""
          Assets:Checking    100.00 USD
          Income:Salary     -100.00 USD

        2018-03-02 * "" #processed
          Assets:Checking    1200.00 CAD
          Income:Salary     -1000.00 USD @ 1.2 CAD
        """
        self.assertFalse(errors)
        for entry in data.filter_txns(entries):
            self.assertTrue(
                (currency_accounts.META_PROCESSED in entry.meta)
                == ("processed" in entry.tags)
            )

    @loader.load_doc()
    def test_currency_conversion(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.currency_accounts" ""

        2018-01-01 open Assets:Checking
        2018-01-01 open Income:Salary

        2018-03-02 * "" #processed
          Assets:Checking    1200.00 CAD
          Income:Salary     -1000.00 USD @ 1.2 CAD
        """
        self.assertFalse(errors)
        self.assertEqualEntries(
            """

          2018-01-01 open Assets:Checking
          2018-01-01 open Income:Salary
          2018-01-01 open Equity:CurrencyAccounts:CAD
          2018-01-01 open Equity:CurrencyAccounts:USD

          2018-03-02 * "" #processed
            currency_accounts_processed: TRUE
            Assets:Checking    1200.00 CAD
            Income:Salary     -1000.00 USD
            Equity:CurrencyAccounts:CAD  -1200.00 CAD
            Equity:CurrencyAccounts:USD   1000.00 USD

        """,
            entries,
        )

    @loader.load_doc()
    def test_custom_base_account(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.currency_accounts" "Assets:TradingAccounts"

        2018-01-01 open Assets:Checking
        2018-01-01 open Income:Salary

        2018-03-02 * "" #processed
          Assets:Checking    1200.00 CAD
          Income:Salary     -1000.00 USD @ 1.2 CAD
        """
        self.assertFalse(errors)
        self.assertEqualEntries(
            """

          2018-01-01 open Assets:Checking
          2018-01-01 open Income:Salary
          2018-01-01 open Assets:TradingAccounts:CAD
          2018-01-01 open Assets:TradingAccounts:USD

          2018-03-02 * "" #processed
            currency_accounts_processed: TRUE
            Assets:Checking    1200.00 CAD
            Income:Salary     -1000.00 USD
            Assets:TradingAccounts:CAD  -1200.00 CAD
            Assets:TradingAccounts:USD   1000.00 USD

        """,
            entries,
        )

    @loader.load_doc()
    def test_with_costs_ignored(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.currency_accounts" ""

        2018-01-01 open Assets:Invest
        2018-01-01 open Assets:Cash
        2018-01-01 open Income:Profits

        2018-03-01 * ""
          Assets:Invest    2 HOOL {1000.00 USD}
          Assets:Cash     -2000.00 USD

        2018-03-02 * ""
          Assets:Invest   -2 HOOL {1000.00 USD} @ 1010.00 USD
          Assets:Cash      2020.00 USD
          Income:Profits    -20.00 USD

        """
        self.assertFalse(errors)
        self.assertEqualEntries(
            """

          2018-01-01 open Assets:Invest
          2018-01-01 open Assets:Cash
          2018-01-01 open Income:Profits

          2018-03-01 * ""
            Assets:Invest    2 HOOL {1000.00 USD, 2018-03-01}
            Assets:Cash     -2000.00 USD

          2018-03-02 * ""
            Assets:Invest   -2 HOOL {1000.00 USD, 2018-03-01} @ 1010.00 USD
            Assets:Cash      2020.00 USD
            Income:Profits    -20.00 USD

        """,
            entries,
        )

    @loader.load_doc()
    def test_with_costs_and_price(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.currency_accounts" ""

        2018-01-01 open Assets:Invest
        2018-01-01 open Assets:Cash
        2018-01-01 open Income:Profits

        2018-03-03 * ""
          Assets:Invest    1 HOOL {800.00 USD}
          Assets:Cash      1200.00 CAD @ 0.8 USD
        """
        self.assertFalse(errors)
        self.assertEqualEntries(
            """

          2018-01-01 open Assets:Invest
          2018-01-01 open Assets:Cash
          2018-01-01 open Income:Profits
          2018-01-01 open Equity:CurrencyAccounts:CAD
          2018-01-01 open Equity:CurrencyAccounts:USD

          2018-03-03 * ""
            currency_accounts_processed: TRUE
            Assets:Invest    1 HOOL {800.00 USD}
            Assets:Cash      1200.00 CAD
            Equity:CurrencyAccounts:USD   -800.00 USD
            Equity:CurrencyAccounts:CAD  -1200.00 CAD

        """,
            entries,
        )

    @loader.load_doc()
    def test_two_groups_already_balanced(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.currency_accounts" ""

        2018-01-01 open Assets:Checking
        2018-01-01 open Income:Salary
        2018-01-01 open Assets:Vacation
        2018-01-01 open Income:Vacation

        2018-03-01 * ""
          Assets:Checking    100.00 USD
          Income:Salary     -100.00 USD
          Assets:Vacation    1.23 VACHR
          Income:Vacation   -1.23 VACHR

        """
        self.assertFalse(errors)
        self.assertEqualEntries(
            """

          2018-01-01 open Assets:Checking
          2018-01-01 open Income:Salary
          2018-01-01 open Assets:Vacation
          2018-01-01 open Income:Vacation

          2018-03-01 * ""
            currency_accounts_processed: TRUE
            Assets:Checking    100.00 USD
            Income:Salary     -100.00 USD
            Assets:Vacation    1.23 VACHR
            Income:Vacation   -1.23 VACHR

        """,
            entries,
        )

    @loader.load_doc()
    def test_residual_not_conversion(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.currency_accounts" ""

        2018-01-01 open Assets:Invest
        2018-01-01 open Assets:Match
        2018-01-01 open Assets:Cash
        2018-01-01 open Income:Profits
        2018-01-01 open Expenses:Match

        2018-03-03 * ""
          Assets:Invest          24.14353 VGI {98.37 USD}
          Assets:Cash            -2375.00 USD
          Assets:Match              -2375 IRAUSD
          Expenses:Match             2375 IRAUSD

        2018-03-03 * ""
          Assets:Invest          24.14353 VGI {98.37 USD} @ 98.37 USD
          Assets:Cash            -2375.00 USD
          Assets:Match              -2375 IRAUSD
          Expenses:Match             2375 IRAUSD

        """
        self.assertFalse(errors)

        # FIXME: Ouch! Some of the residual seeps through here. This needs fixing too.
        self.assertEqualEntries(
            """

          2018-01-01 open Assets:Invest
          2018-01-01 open Assets:Match
          2018-01-01 open Assets:Cash
          2018-01-01 open Income:Profits
          2018-01-01 open Expenses:Match
          2018-01-01 open Equity:CurrencyAccounts:USD

          2018-03-03 * ""
            Assets:Invest          24.14353 VGI {98.37 USD}
            Assets:Cash            -2375.00 USD
            Assets:Match              -2375 IRAUSD
            Expenses:Match             2375 IRAUSD

          2018-03-03 * ""
            currency_accounts_processed: TRUE
            Assets:Invest          24.14353 VGI {98.37 USD}
            Assets:Cash            -2375.00 USD
            Assets:Match              -2375 IRAUSD
            Expenses:Match             2375 IRAUSD
            Equity:CurrencyAccounts:USD  0.0009539 USD   ;; Ouch!

        """,
            entries,
        )


if __name__ == "__main__":
    unittest.main()
