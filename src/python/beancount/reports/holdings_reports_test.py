import unittest
import io

from beancount.reports import holdings_reports
from beancount.reports import table
from beancount.ops import holdings
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
        holdings_list, price_map = holdings_reports.get_assets_holdings(self.entries,
                                                                 self.options_map)
        self.assertTrue(isinstance(holdings_list, list))
        self.assertTrue(isinstance(price_map, dict))

    def test_report_holdings(self):
        table_ = holdings_reports.report_holdings(None, False, self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

        table_ = holdings_reports.report_holdings('USD', False, self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_holdings_bycommodity(self):
        table_ = holdings_reports.report_holdings_bycommodity(None, False,
                                                       self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

        table_ = holdings_reports.report_holdings_bycommodity('USD', False,
                                                       self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_holdings_byaccount(self):
        table_ = holdings_reports.report_holdings_byaccount(None, False,
                                                     self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

        table_ = holdings_reports.report_holdings_byaccount('USD', False,
                                                     self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_holdings_bycurrency(self):
        table_ = holdings_reports.report_holdings_bycurrency(None, False,
                                                      self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

        table_ = holdings_reports.report_holdings_bycurrency('USD', False,
                                                      self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_networth(self):
        table_ = holdings_reports.report_networth(self.entries, self.options_map)
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_load_from_csv(self):
        oss = io.StringIO()
        table_ = holdings_reports.report_holdings(None, False, self.entries, self.options_map)
        table.table_to_csv(table_, file=oss)
        iss = io.StringIO(oss.getvalue())
        holdings_list = list(holdings_reports.load_from_csv(iss))
        self.assertEqual(2, len(holdings_list))
        self.assertTrue(isinstance(holdings_list, list))
        self.assertTrue(isinstance(holdings_list[0], holdings.Holding))


# Add appropriate tests for all combinations.
__incomplete__ = True
