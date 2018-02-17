__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.plugins import sellgains
from beancount.ops import validation
from beancount.parser import printer


class TestSellGains(unittest.TestCase):

    @loader.load_doc()
    def test_sellgains_success(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.auto_accounts"
        plugin "beancount.plugins.sellgains"

        1999-07-31 * "Sell"
          Assets:US:Company:ESPP          -81 ADSK {26.3125 USD} @ 26.4375 USD
          Assets:US:Company:Cash      2141.36 USD
          Expenses:Financial:Fees        0.08 USD
          Income:US:Company:ESPP:PnL
        """
        printer.print_errors(errors)
        self.assertEqual([], errors)

    @loader.load_doc(expect_errors=True)
    def test_sellgains_fail_balance(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.auto_accounts"
        plugin "beancount.plugins.sellgains"

        1999-07-31 * "Sell"
          Assets:US:Company:ESPP          -81 ADSK {26.3125 USD} @ 26.4375 USD
          Assets:US:Company:Cash      2141.36 USD
          Expenses:Financial:Fees        1.08 USD
          Income:US:Company:ESPP:PnL   -11.13 USD
        """
        self.assertEqual([sellgains.SellGainsError], list(map(type, errors)))

    @loader.load_doc(expect_errors=True)
    def test_sellgains_fail_imbalance(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.auto_accounts"
        plugin "beancount.plugins.sellgains"

        1999-07-31 * "Sell"
          Assets:US:Company:ESPP          -81 ADSK {26.3125 USD} @ 26.4375 USD
          Assets:US:Company:Cash      2141.36 USD
          Income:US:Company:ESPP:PnL   -11.13 USD
        """
        self.assertEqual([sellgains.SellGainsError,
                          validation.ValidationError], list(map(type, errors)))

    @loader.load_doc()
    def test_sellgains_other_currency(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.auto_accounts"
        plugin "beancount.plugins.sellgains"

        1999-07-31 * "Sell"
          Assets:US:Company:ESPP          -80 ADSK {26.50 USD} @ 27.50 USD
          Expenses:Commissions           9.95 USD
          Assets:US:Company:Cash      2433.39 CAD @ 0.9000 USD
          Income:US:Company:ESPP:PnL   -80.00 USD
        """
        self.assertEqual([], list(map(type, errors)))

    @loader.load_doc()
    def test_sellgains_zero_price(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.auto_accounts"
        plugin "beancount.plugins.sellgains"

        1999-07-31 * "Sell"
          Assets:US:Broker:Options     -8000 VTI180216C200 {2.50 USD} @ 0 USD
          Income:US:Company:ESPP:PnL    20000 USD
        """
        self.assertEqual([], list(map(type, errors)))
