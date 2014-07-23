"""
Test various options.
"""
import unittest

from beancount.parser import parsedoc
from beancount.parser import options
from beancount.core import account_types


class TestOptions(unittest.TestCase):

    def test_get_account_types(self):
        options_ = options.DEFAULT_OPTIONS.copy()
        result = options.get_account_types(options_)
        expected = account_types.AccountTypes(assets='Assets',
                                              liabilities='Liabilities',
                                              equity='Equity',
                                              income='Income',
                                              expenses='Expenses')
        self.assertEqual(expected, result)

    def test_get_previous_accounts(self):
        options_ = options.DEFAULT_OPTIONS.copy()
        result = options.get_previous_accounts(options_)
        self.assertEqual(3, len(result))
        self.assertTrue(all(isinstance(x, str) for x in result))

    def test_get_current_accounts(self):
        options_ = options.DEFAULT_OPTIONS.copy()
        result = options.get_current_accounts(options_)
        self.assertEqual(2, len(result))
        self.assertTrue(all(isinstance(x, str) for x in result))

    def test_list_options(self):
        options_doc = options.list_options()
        self.assertTrue(isinstance(options_doc, str))

class TestAccountTypeOptions(unittest.TestCase):

    @parsedoc
    def test_custom_account_names__success(self, entries, errors, options_map):
        """
          option "name_assets" "Actif"
          option "name_liabilities" "Passif"
          option "name_equity" "Capital"
          option "name_income" "Revenu"
          option "name_expenses" "Depenses"

          2014-01-04 open Actif:CA:RBC:CompteCheques
          2014-01-04 open Passif:CA:RBC:CarteDeCredit
          2014-01-04 open Capital:Ouverture
          2014-01-04 open Revenu:Salaire
          2014-01-04 open Depenses:Bistrot
        """
        self.assertFalse(errors)
        self.assertEqual(5, len(entries))

    @parsedoc
    def test_custom_account_names__success_reset(self, entries, errors, options_map):
        """
          2014-01-01 open Assets:CA:RBC:Checking

          option "name_assets" "Actif"

          2014-01-04 open Actif:CA:RBC:CompteCheques
        """
        self.assertFalse(errors)
        self.assertEqual(2, len(entries))

    @parsedoc
    def test_custom_account_names__basic_fail(self, entries, errors, options_map):
        """
          2014-01-04 open Actif:CA:RBC:CompteCheques
          2014-01-04 open Passif:CA:RBC:CarteDeCredit
        """
        self.assertTrue(errors)
        self.assertEqual(2, len(entries))

    @parsedoc
    def test_custom_account_names__fail_invalid_order(self, entries, errors, options_map):
        """
          2014-01-04 open Actif:CA:RBC:CompteCheques

          option "name_assets" "Actif"
        """
        self.assertTrue(errors)
        self.assertEqual(1, len(entries))
