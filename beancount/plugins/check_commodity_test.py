__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.plugins import check_commodity


class TestCheckCommodity(unittest.TestCase):

    @loader.load_doc()
    def test_get_commodity_map_ex(self, entries, _, __):
        """
            2011-01-01 commodity BAZ

            2011-01-01 open Expenses:Test  TEST1, TEST2, TEST7
            2011-01-01 open Expenses:Other
            2011-01-01 open Assets:Test
            2011-01-01 open Assets:Investments
            2011-01-01 open Equity:Opening-Balances

            2011-05-17 * "Something"
              foo: TEST3
              Expenses:Other  1.00 TEST4
                bar: TEST5
              Assets:Test

            2011-05-17 * "Something"
              Assets:Investments  5.00 FOO @ 1.00 TEST6
              Assets:Test

            2011-05-18 pad Assets:Test Equity:Opening-Balances
            2011-05-19 balance Assets:Test -1.00 TEST7

            2011-05-20 price BAR 1.00 TEST6
        """
        commodities = check_commodity.get_commodity_map_ex(entries, metadata=True)
        self.assertEqual(commodities.keys(), {'TEST1', 'TEST2', 'TEST3', 'TEST4', 'TEST5',
                                              'TEST6', 'TEST7', 'FOO', 'BAR', 'BAZ'})

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


if __name__ == '__main__':
    unittest.main()
