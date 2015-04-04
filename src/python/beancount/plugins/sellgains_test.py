__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from beancount import loader
from beancount.plugins import check_commodity
from beancount.parser import printer


class TestSellGains(unittest.TestCase):

    @loader.loaddoc
    def test_sellgains_success(self, entries, errors, options_map):
        """
        option "plugin" "beancount.ops.auto_accounts"
        option "plugin" "beancount.plugins.sellgains"

        1999-07-31 * "Sell"
          Assets:US:Company:ESPP          -81 ADSK {26.3125 USD} @ 26.4375 USD
          Assets:US:Company:Cash      2141.36 USD
          Expenses:Financial:Fees        0.08 USD
          Income:US:Company:ESPP:PnL
        """
        self.assertEqual([], errors)

    @loader.loaddoc
    def test_sellgains_fail(self, entries, errors, options_map):
        """
        option "plugin" "beancount.ops.auto_accounts"
        option "plugin" "beancount.plugins.sellgains"

        1999-07-31 * "Sell"
          Assets:US:Company:ESPP          -81 ADSK {26.3125 USD} @ 26.4375 USD
          Assets:US:Company:Cash      2141.36 USD
          Income:US:Company:ESPP:PnL
        """
        self.assertTrue(errors)
