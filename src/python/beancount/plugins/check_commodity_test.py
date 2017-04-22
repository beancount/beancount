__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.plugins import check_commodity


class TestCheckCommodity(unittest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_check_commodity_transaction(self, _, errors, __):
        """
            plugin "beancount.plugins.check_commodity"

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 USD
              Assets:Other         -1.00 USD

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 CAD
              Assets:Other         -1.00 USD @ 1.00 CAD
        """
        self.assertEqual([check_commodity.CheckCommodityError] * 2,
                         list(map(type, errors)))


    @loader.load_doc(expect_errors=True)
    def test_check_commodity_used_in_balance_only(self, _, errors, __):
        """
            plugin "beancount.plugins.check_commodity"

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 USD
              Assets:Other         -1.00 USD

            2012-01-01 balance Expenses:Restaurant   0.00 CAD

        """
        self.assertEqual([check_commodity.CheckCommodityError] * 2,
                         list(map(type, errors)))

    @loader.load_doc()
    def test_check_commodity_okay(self, _, errors, __):
        """
            plugin "beancount.plugins.check_commodity"

            2000-01-01 commodity USD
            2000-01-01 commodity CAD

            2011-01-01 open Expenses:Restaurant
            2011-01-01 open Assets:Other

            2011-05-17 * "Something"
              Expenses:Restaurant   1.00 USD
              Assets:Other         -1.00 USD

            2012-01-01 balance Expenses:Restaurant   0.00 CAD

        """
        self.assertFalse(errors)
