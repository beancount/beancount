import io
import collections
import textwrap
import unittest

from beancount.reports import rholdings
from beancount.reports import table
from beancount.loader import loaddoc


class TestReportHoldings(unittest.TestCase):

    @loaddoc
    def get_entries(self, entries, _, options_map):
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
        return entries, options_map

    # Basically just call these functions below, to exercise them.
    # Very basic tests, but still worthwhile. Running the code is a minimum.

    def test_report_holdings(self):
        table_ = rholdings.report_holdings(*self.get_entries())
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_holdings_aggregated(self):
        table_ = rholdings.report_holdings_aggregated(None, *self.get_entries())
        self.assertTrue(isinstance(table_, table.TableReport))

        table_ = rholdings.report_holdings_aggregated('USD', *self.get_entries())
        self.assertTrue(isinstance(table_, table.TableReport))

    def test_report_holdings_relative(self):
        table_ = rholdings.report_holdings_relative(None, *self.get_entries())
        self.assertTrue(isinstance(table_, table.TableReport))

        table_ = rholdings.report_holdings_relative('USD', *self.get_entries())
        self.assertTrue(isinstance(table_, table.TableReport))
