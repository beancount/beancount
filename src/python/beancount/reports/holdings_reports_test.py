__author__ = "Martin Blais <blais@furius.ca>"

import functools
import unittest
import io

from beancount.core.amount import D
from beancount.reports import holdings_reports
from beancount.reports import table
from beancount.ops import holdings
from beancount import loader


class TestHoldingsReports(unittest.TestCase):

    @loader.loaddoc
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

    def test_report_export_portfolio(self):
        report_ = holdings_reports.ExportPortfolioReport.from_args([])
        format_ = 'ofx'
        output = report_.render(self.entries, self.errors, self.options_map, format_)
        self.assertTrue(output)







EE = holdings_reports.ExportEntry

class TestCommodityClassifications(unittest.TestCase):

    @loader.loaddoc
    def test_export_stock_declared(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          ticker: "NASDAQ:AAPL"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, _ = holdings_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            EE('NASDAQ:AAPL', D('2'), D('410.00'), False, ''),
            ], exported)

    @loader.loaddoc
    def test_export_mutfund_declared(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity RBF1005
          ticker: "MUTF_CA:RBF1005"

        2015-02-08 *
          Assets:Investing           10.2479 RBF1005 {10.00 USD}
          Equity:Opening-Balances
        """
        exported, _ = holdings_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            EE('MUTF_CA:RBF1005', D('10.2479'), D('10.00'), True, ''),
            ], exported)


    @loader.loaddoc
    def test_export_undeclared(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, _ = holdings_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            ], exported)

    @loader.loaddoc
    def test_export_omitted(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances

        """
        exported, _ = holdings_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            ], exported)


    @loader.loaddoc
    def test_export_convert_declared(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          ticker: "cash"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, _ = holdings_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            EE('CASH:USD', D('810.00'), D('810.00'), False, ''),
            ], exported)

    @loader.loaddoc
    def test_export_convert_cash(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity USD
          ticker: "cash"

        2015-02-08 *
          Assets:Investing           100.00 USD
          Equity:Opening-Balances
        """
        exported, _ = holdings_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            EE('CASH:USD', D('100.00'), D('100.00'), False, ''),
            ], exported)

    @loader.loaddoc
    def test_export_convert_cash_foreign(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity CAD
          ticker: "cash"

        2015-02-08 *
          Assets:Investing           100.00 CAD
          Equity:Opening-Balances
        """
        exported, _ = holdings_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            EE('CASH:CAD', D('100.00'), D('100.00'), False, ''),
            ], exported)
