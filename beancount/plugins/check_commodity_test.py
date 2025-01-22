__copyright__ = "Copyright (C) 2014-2017, 2019-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import textwrap
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
        occurrences, commodities = check_commodity.get_commodity_map_ex(
            entries, metadata=True
        )
        self.assertEqual(
            {value for _, value in occurrences},
            {"TEST1", "TEST2", "TEST3", "TEST4", "TEST5", "TEST6", "TEST7", "FOO", "BAR"},
        )
        self.assertEqual(commodities.keys(), {"BAZ"})

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
        self.assertEqual([check_commodity.CheckCommodityError] * 2, list(map(type, errors)))

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
        self.assertEqual([check_commodity.CheckCommodityError] * 2, list(map(type, errors)))

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

    def test_check_commodity_ignore(self):
        for account_re, currency_re in [
            (".*", ".*"),
            ("Assets:Options", ".*"),
            (".*", "QQQ_.*"),
            ("Assets:.*Options", r"QQQ_[0-9]{6}[CP][0-9]+"),
        ]:
            _, errors, __ = loader.load_string(
                textwrap.dedent("""
                plugin "beancount.plugins.check_commodity" "{'ACCOUNT': 'CURRENCY'}"

                2000-01-01 commodity USD

                2011-01-01 open Assets:Cash
                2011-01-01 open Assets:Options

                2011-05-17 * "Something"
                  Assets:Options     -100 QQQ_041621C341 {1.470 USD}
                  Assets:Cash      147.00 USD

                2011-05-17 * "Deposit - for posting metadata"
                  related: QQQ_041621C341
                  Assets:Options     -200.00
                    related: QQQ_041621C341
                  Assets:Cash      200.00 USD

                2011-05-19 price QQQ_041621C341   1.33 USD
            """)
                .replace("ACCOUNT", account_re)
                .replace("CURRENCY", currency_re)
            )
            self.assertFalse(errors)


if __name__ == "__main__":
    unittest.main()
