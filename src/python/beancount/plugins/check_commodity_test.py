__author__ = "Martin Blais <blais@furius.ca>"

import re
import unittest

from beancount import loader


class TestCheckCommodity(unittest.TestCase):

    @loader.loaddoc
    def test_check_commodity_transaction(self, _, errors, __):
        """
            option "plugin" "beancount.plugins.check_commodity"

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 USD
              Assets:Other         -1.00 USD

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 CAD
              Assets:Other         -1.00 USD @ 1.00 CAD
        """
        self.assertEqual(2, len(errors))


    @loader.loaddoc
    def test_check_commodity_balance(self, _, errors, __):
        """
            option "plugin" "beancount.plugins.check_commodity"

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 USD
              Assets:Other         -1.00 USD

            2012-01-01 balance Expenses:Restaurant   0.00 CAD

        """
        self.assertEqual(1, len(errors))
        self.assertTrue(re.search('Expenses:Restaurant', errors[0].message))
