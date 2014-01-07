"""
Test various options.
"""
import unittest
import textwrap
import inspect
import sys

from beancount.parser import parsedoc
from beancount.parser import parser, options
from beancount.core.data import Transaction, Balance, Open, Close, Pad, Event, Price, Note
from beancount.core.data import format_entry
from beancount.core.account import Account


class TestOptions(unittest.TestCase):

    @parsedoc
    def test_custom_account_names(self, entries, errors, options):
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
