__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import io

from beancount.core.number import Decimal
from beancount.reports import holdings_reports
from beancount.utils import table
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


class TestMultiCurrencyNetWorthCalculation(unittest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, errors, options_map):
        """
        option "operating_currency" "CHF"
        option "operating_currency" "USD"

        2014-01-01 open Assets:Bank1
        2014-01-01 open Income:Something

        2014-05-31 *
          Assets:Bank1         100 CAD
          Income:Something

        2014-05-31 price CAD  1.2 CHF
        2014-05-31 price USD  5 CHF
        2014-05-31 price USD  0.5 CAD
        """
        self.entries = entries
        self.errors = errors
        self.options_map = options_map

    def test_calculate_net_worths(self):
        net_worths = holdings_reports.calculate_net_worths(self.entries,
                                                           self.options_map)
        self.assertTrue(isinstance(net_worths, list))
        self.assertListEqual(
            [('CHF', Decimal('120.0')), ('USD', Decimal('200'))], net_worths)

if __name__ == '__main__':
    unittest.main()
