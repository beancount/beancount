__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import io

from beancount.reports import holdings_reports
from beancount.reports import table
from beancount.ops import holdings
from beancount import loader


class TestHoldingsReports(unittest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, errors, options_map):
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
        self.errors = errors
        self.options_map = options_map

    # Basically just call these functions below, to exercise them.
    # Very basic tests, but still worthwhile. Running the code is a minimum.

    def test_get_assets_holdings(self):
        holdings_list, price_map = holdings_reports.get_assets_holdings(self.entries,
                                                                        self.options_map)
        self.assertTrue(isinstance(holdings_list, list))
        self.assertTrue(isinstance(price_map, dict))

    def test_report_holdings(self):
        for args in [[],
                     ['--currency=USD'],
                     ['--by=commodity'],
                     ['--by=account'],
                     ['--by=currency']]:
            report_ = holdings_reports.HoldingsReport.from_args(args)
            for format_ in report_.get_supported_formats():
                if format_ == 'beancount' and args:
                    continue
                output = report_.render(self.entries, self.errors, self.options_map,
                                        format_)
                self.assertTrue(output)

    def test_report_networth(self):
        report_ = holdings_reports.NetWorthReport.from_args([])
        for format_ in report_.get_supported_formats():
            output = report_.render(self.entries, self.errors, self.options_map, format_)
            self.assertTrue(output)

    def test_load_from_csv(self):
        oss = io.StringIO()
        table_ = holdings_reports.report_holdings(
            None, False, self.entries, self.options_map)
        table.table_to_csv(table_, file=oss)
        iss = io.StringIO(oss.getvalue())
        holdings_list = list(holdings_reports.load_from_csv(iss))
        self.assertEqual(2, len(holdings_list))
        self.assertTrue(isinstance(holdings_list, list))
        self.assertTrue(isinstance(holdings_list[0], holdings.Holding))
