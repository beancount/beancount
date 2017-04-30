__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader


class TestOneCommodity(unittest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_one_commodity_transaction(self, _, errors, __):
        """
            plugin "beancount.plugins.onecommodity"

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 USD
              Assets:Other         -1.00 USD

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 CAD
              Assets:Other         -1.00 USD @ 1.00 CAD

        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, 'Expenses:Restaurant')


    @loader.load_doc(expect_errors=True)
    def test_one_commodity_balance(self, _, errors, __):
        """
            plugin "beancount.plugins.onecommodity"

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 USD
              Assets:Other         -1.00 USD

            2012-01-01 balance Expenses:Restaurant   0.00 CAD

        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, 'Expenses:Restaurant')
