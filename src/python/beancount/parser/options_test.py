"""
Test various options.
"""
import unittest

from beancount.parser import parsedoc
from beancount.parser import options
from beancount.core import account_types


class TestOptions(unittest.TestCase):

    @parsedoc
    def test_custom_account_names(self, entries, errors, options_map):
        """
          option "name_assets" "Actif"
          option "name_liabilities" "Passif"
          option "name_equity" "Equite"
          option "name_income" "Revenu"
          option "name_expenses" "Depense"

          2014-01-04 open Actif:CA:RBC:CompteCheques
          2014-01-04 open Passif:CA:RBC:CarteDeCredit
        """
        self.assertFalse(errors)
        self.assertEqual(2, len(entries))

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
