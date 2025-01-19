"""
Test various options.
"""

__copyright__ = "Copyright (C) 2014-2017, 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core import account_types
from beancount.parser import options
from beancount.parser import parser


class TestOptions(unittest.TestCase):
    def test_get_account_types(self):
        options_ = options.OPTIONS_DEFAULTS.copy()
        result = options.get_account_types(options_)
        expected = account_types.AccountTypes(
            assets="Assets",
            liabilities="Liabilities",
            equity="Equity",
            income="Income",
            expenses="Expenses",
        )
        self.assertEqual(expected, result)

    def test_get_previous_accounts(self):
        options_ = options.OPTIONS_DEFAULTS.copy()
        result = options.get_previous_accounts(options_)
        self.assertEqual(3, len(result))
        self.assertTrue(all(isinstance(x, str) for x in result))

    def test_get_current_accounts(self):
        options_ = options.OPTIONS_DEFAULTS.copy()
        result = options.get_current_accounts(options_)
        self.assertEqual(2, len(result))
        self.assertTrue(all(isinstance(x, str) for x in result))

    def test_list_options(self):
        options_doc = options.list_options()
        self.assertTrue(isinstance(options_doc, str))


class TestAccountTypeOptions(unittest.TestCase):
    @parser.parse_doc()
    def test_custom_account_names__success(self, entries, errors, options_map):
        """
        option "name_assets" "Actif"
        option "name_liabilities" "Passif"
        option "name_equity" "Capital"
        option "name_income" "Revenu"
        option "name_expenses" "Dépenses"

        2014-01-04 open Actif:CA:RBC:CompteChèques
        2014-01-04 open Passif:CA:RBC:CarteDeCrédit
        2014-01-04 open Capital:Ouverture
        2014-01-04 open Revenu:Salaire
        2014-01-04 open Dépenses:Bistrot
        """
        self.assertFalse(errors)
        self.assertEqual(5, len(entries))

    @parser.parse_doc(expect_errors=True)
    def test_custom_account_names__invalid_root(self, entries, errors, options_map):
        """
        option "name_assets" "Foo:Bar"
        """
        self.assertRegex(
            errors[0].message,
            "Error for option 'name_assets': Invalid root account name: 'Foo:Bar'",
        )

    @parser.parse_doc(expect_errors=True)
    def test_custom_account_names__invalid_leaf(self, entries, errors, options_map):
        """
        option "account_previous_balances" "invalid"
        """
        self.assertRegex(
            errors[0].message,
            "Error for option 'account_previous_balances': Invalid leaf account name: 'invalid'",
        )

    @parser.parse_doc()
    def test_custom_account_names__success_reset(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:CA:RBC:Checking

        option "name_assets" "Actif"

        2014-01-04 open Actif:CA:RBC:CompteChèques
        """
        self.assertFalse(errors)
        self.assertEqual(2, len(entries))

    @parser.parse_doc(expect_errors=True)
    def test_custom_account_names__basic_fail(self, entries, errors, options_map):
        """
        2014-01-04 open Actif:CA:RBC:CompteChèques
        2014-01-04 open Passif:CA:RBC:CarteDeCrédit
        """
        self.assertEqual(2, len(entries))
        self.assertEqual(2, len(errors))
        for error in errors:
            self.assertRegex(error.message, "Invalid account name")

    @parser.parse_doc(expect_errors=True)
    def test_custom_account_names__fail_invalid_order(self, entries, errors, options_map):
        """
        2014-01-04 open Actif:CA:RBC:CompteChèques

        option "name_assets" "Actif"
        """
        self.assertEqual(1, len(entries))
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Invalid account name")

    @parser.parse_doc(expect_errors=True)
    def test_custom_account_names__fail_invalid_other(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:CA:RBC:Checking

        option "name_assets" "Actif"

        2014-01-04 open Assets:CA:RBC:Checking
        """
        self.assertEqual(2, len(entries))
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Invalid account name")


class TestValidateOptions(unittest.TestCase):
    @parser.parse_doc(expect_errors=True)
    def test_validate__plugin_processing_mode__invalid(self, entries, errors, options_map):
        """
        option "plugin_processing_mode" "i-dont-exist"
        """
        self.assertTrue(errors)


if __name__ == "__main__":
    unittest.main()
