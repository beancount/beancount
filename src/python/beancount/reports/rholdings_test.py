import unittest

from beancount.reports import rholdings
from beancount.reports import table
from beancount.loader import loaddoc


class TestReportHoldings(unittest.TestCase):

    @loaddoc
    def setUp(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Bank1
        2014-01-01 open Assets:Bank2
        2014-01-01 open Assets:Bank3
        2014-01-01 open Income:Something

        2014-05-31 *
          Assets:Bank1         100 MSFT {200.01 USD}
          Income:Something

        2014-05-31 *
          Assets:Bank2         1000 INR @ 200 USD
          Income:Something
        """
        self.entries = entries
        self.options_map = options_map

    # Basically just call these functions below, to exercise them.
    # Very basic tests, but still worthwhile. Running the code is a minimum.

    def test_get_assets_holdings(self):
        holdings_list, price_map = rholdings.get_assets_holdings(self.entries, self.options_map)
        self.assertTrue(isinstance(holdings_list, list))
        self.assertTrue(isinstance(price_map, dict))

    def test_report_holdings(self):
        table_ = rholdings.report_holdings(self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_holdings_aggregated(self):
        table_ = rholdings.report_holdings_aggregated(None, self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

        table_ = rholdings.report_holdings_aggregated('USD', self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_holdings_byaccount(self):
        table_ = rholdings.report_holdings_byaccount('USD', self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_holdings_relative(self):
        table_ = rholdings.report_holdings_relative(None, self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

        table_ = rholdings.report_holdings_relative('USD', self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_currency_exposure(self):
        table_ = rholdings.report_currency_exposure(self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_networth(self):
        table_ = rholdings.report_networth(self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))
