__copyright__ = "Copyright (C) 2018-2019, 2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.parser import cmptest


class TestCheckClosing(cmptest.TestCase):
    @loader.load_doc(expect_errors=True)
    def test_check_closing(self, entries, _, options_map):
        """
        plugin "beancount.plugins.check_closing"

        2018-02-16 open Assets:US:Brokerage:Main:Cash
        2018-02-16 open Assets:US:Brokerage:Main:Options
        2018-02-16 open Expenses:Financial:Commissions
        2018-02-16 open Expenses:Financial:Fees
        2018-02-16 open Income:US:Brokerage:Main:PnL

        2018-02-16 * "SOLD -14 QQQ 100 16 FEB 18 160 CALL @5.31"
          Assets:US:Brokerage:Main:Options     -1400 QQQ180216C160 {2.70 USD} @ 5.31 USD
            closing: TRUE
          Expenses:Financial:Commissions       17.45 USD
          Expenses:Financial:Fees               0.42 USD
          Assets:US:Brokerage:Main:Cash      7416.13 USD
          Income:US:Brokerage:Main:PnL      -3654.00 USD
        """
        self.assertEqualEntries(
            """

          2018-02-16 open Assets:US:Brokerage:Main:Cash
          2018-02-16 open Assets:US:Brokerage:Main:Options
          2018-02-16 open Expenses:Financial:Commissions
          2018-02-16 open Expenses:Financial:Fees
          2018-02-16 open Income:US:Brokerage:Main:PnL

          2018-02-16 * "SOLD -14 QQQ 100 16 FEB 18 160 CALL @5.31"
            Assets:US:Brokerage:Main:Options     -1400 QQQ180216C160 {2.70 USD} @ 5.31 USD
            Expenses:Financial:Commissions       17.45 USD
            Expenses:Financial:Fees               0.42 USD
            Assets:US:Brokerage:Main:Cash      7416.13 USD
            Income:US:Brokerage:Main:PnL      -3654.00 USD

          2018-02-17 balance Assets:US:Brokerage:Main:Options  0 QQQ180216C160

        """,
            entries,
        )


if __name__ == "__main__":
    unittest.main()
