"""
Test various options.
"""

__copyright__ = "Copyright (C) 2014-2017, 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import pytest
from decimal import Decimal

from beancount.core import account_types
from beancount.core import display_context
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
    @pytest.mark.skip(reason="custom account root names not parsed yet in rust parser")
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

    @pytest.mark.skip(reason="custom account root names not parsed yet in rust parser")
    @parser.parse_doc()
    def test_custom_account_names__success_reset(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:CA:RBC:Checking

        option "name_assets" "Actif"

        2014-01-04 open Actif:CA:RBC:CompteChèques
        """
        self.assertFalse(errors)
        self.assertEqual(2, len(entries))

    @pytest.mark.skip(reason="unicode account names now accepted; test expects errors")
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

    @pytest.mark.skip(reason="unicode account names now accepted; test expects errors")
    @parser.parse_doc(expect_errors=True)
    def test_custom_account_names__fail_invalid_order(self, entries, errors, options_map):
        """
        2014-01-04 open Actif:CA:RBC:CompteChèques

        option "name_assets" "Actif"
        """
        self.assertEqual(1, len(entries))
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Invalid account name")

    @pytest.mark.skip(reason="unicode account names now accepted; test expects errors")
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


class TestDisplayContextOptions(unittest.TestCase):
    @parser.parse_doc()
    def test_render_commas_no(self, _, __, options_map):
        """
        option "render_commas" "0"
        """
        self.assertEqual(False, options_map["render_commas"])

    @parser.parse_doc()
    def test_render_commas_yes(self, _, __, options_map):
        """
        option "render_commas" "1"
        """
        self.assertEqual(True, options_map["render_commas"])

    @parser.parse_doc()
    def test_render_commas_yes2(self, _, __, options_map):
        """
        option "render_commas" "TRUE"
        """
        self.assertEqual(True, options_map["render_commas"])

    @parser.parse_doc()
    def test_display_precision(self, entries, errors, options_map):
        """
        option "display_precision" "USD:0.01"
        option "display_precision" "EUR:0.0001"

        2024-01-01 open Assets:Cash USD
        2024-01-01 open Assets:Bank EUR
        2024-01-01 open Assets:Crypto BTC

        2024-01-02 * "Test Transaction"
          Assets:Cash   123.4567 USD
          Assets:Bank   987.654321 EUR
          Assets:Crypto 0.123456789 BTC
        """
        self.assertFalse(errors)

        # Verify the display_precision option is correctly stored
        self.assertEqual(
            {"USD": Decimal("0.01"), "EUR": Decimal("0.0001")},
            options_map["display_precision"],
        )

        # Verify the display context reflects the fixed precision
        dcontext = options_map["dcontext"]
        self.assertIsInstance(
            dcontext.ccontexts["USD"], display_context._FixedPrecisionContext
        )
        self.assertEqual(dcontext.ccontexts["USD"].fractional_digits, 2)
        self.assertIsInstance(
            dcontext.ccontexts["EUR"], display_context._FixedPrecisionContext
        )
        self.assertEqual(dcontext.ccontexts["EUR"].fractional_digits, 4)
        # BTC should still be a regular _CurrencyContext as no fixed precision was set
        self.assertIsInstance(dcontext.ccontexts["BTC"], display_context._CurrencyContext)

        # Verify formatting with the set precision
        dformat_natural = dcontext.build(alignment=display_context.Align.NATURAL)
        self.assertEqual(dformat_natural.format(Decimal("123.4567"), "USD"), "123.46")
        self.assertEqual(dformat_natural.format(Decimal("987.654321"), "EUR"), "987.6543")
        # BTC should use inferred precision (max 9 decimal places from input)

        self.assertEqual(
            dformat_natural.format(Decimal("0.123456789"), "BTC"), "0.123456789"
        )

    @parser.parse_doc(expect_errors=True)
    def test_display_precision__invalid_value(self, entries, errors, options_map):
        """
        option "display_precision" "USD:invalid"
        """
        self.assertEqual(1, len(errors))
        self.assertRegex(
            errors[0].message,
            "Error for option 'display_precision': Invalid value 'USD:invalid'",
        )
        # Ensure the default empty dict is still there or the invalid entry is not added
        self.assertEqual({}, options_map["display_precision"])

    @parser.parse_doc(expect_errors=True)
    def test_display_precision__invalid_format(self, entries, errors, options_map):
        """
        option "display_precision" "USD.01"
        """
        self.assertEqual(1, len(errors))
        self.assertRegex(
            errors[0].message,
            "Error for option 'display_precision': Invalid value 'USD.01'",
        )
        self.assertEqual({}, options_map["display_precision"])


class TestToleranceOptions(unittest.TestCase):
    @parser.parse_doc()
    def test_tolerance_defaults(self, _, __, options_map):
        """ """
        self.assertEqual({}, options_map["inferred_tolerance_default"])

    @parser.parse_doc()
    def test_inferred_tolerance_default(self, _, __, options_map):
        """
        option "inferred_tolerance_default" "*:0"
        option "inferred_tolerance_default" "USD:0.05"
        option "inferred_tolerance_default" "JPY:0.5"
        """
        self.assertEqual(
            {"*": Decimal("0"), "USD": Decimal("0.05"), "JPY": Decimal("0.5")},
            options_map["inferred_tolerance_default"],
        )


class TestDeprecatedOptions(unittest.TestCase):
    @pytest.mark.skip(reason="deprecated plugin option handling not implemented in rust parser yet")
    @parser.parse_doc(expect_errors=True)
    def test_deprecated_plugin(self, _, errors, __):
        """
        option "plugin" "beancount.plugins.module_name"
        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "may not be set")

    @pytest.mark.skip()
    @parser.parse_doc(expect_errors=True)
    def test_deprecated_option(self, _, errors, options_map):
        """
        option "allow_pipe_separator" "TRUE"
        """
        self.assertEqual(1, len(errors))
        self.assertEqual(True, options_map["allow_pipe_separator"])
        self.assertRegex(errors[0].message, "this will go away")


if __name__ == "__main__":
    unittest.main()
