__author__ = "Martin Blais <blais@furius.ca>"

import re
import unittest

from beancount import loader


class TestLeafOnly(unittest.TestCase):

    @loader.loaddoc
    def test_leaf_only(self, _, errors, __):
        """
            option "plugin" "beancount.plugins.leafonly"

            2011-01-01 open Expenses:Food
            2011-01-01 open Expenses:Food:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Food:Restaurant   1.00 USD
              Assets:Other              -1.00 USD

            2011-05-17 * "Something"
              Expenses:Food         1.00 USD ;; Offending posting.
              Assets:Other         -1.00 USD

        """
        self.assertEqual(1, len(errors))
        self.assertTrue(re.search('Expenses:Food', errors[0].message))

    @loader.loaddoc
    def test_leaf_only(self, _, errors, __):
        """
            option "plugin" "beancount.plugins.leafonly"

            ;;; 2011-01-01 open Expenses:Food
            2011-01-01 open Expenses:Food:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Food         1.00 USD ;; Offending posting.
              Assets:Other         -1.00 USD

        """
        # Issue #5: If you have a non-leaf posting on an account that doesn't
        # exist, the leafonly plugin raises an AttributeError if there is no
        # Open directive. The problem is that 'open_entry' is None.
        self.assertEqual(2, len(errors))
        for error in errors:
            self.assertTrue(re.search('Expenses:Food', error.message))
