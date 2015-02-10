__author__ = "Martin Blais <blais@furius.ca>"

import functools
import unittest
import io

from beancount.reports import holdings_reports
from beancount.reports import table
from beancount.ops import holdings
from beancount.core import getters
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


class TestCommodityClassifications(unittest.TestCase):

    def classify(fun):
        @loader.loaddoc
        @functools.wraps(fun)
        def wrapped(self, entries, unused_errors, options_map):
            holdings_list, _ = holdings_reports.get_assets_holdings(entries, options_map)
            commodities_map = getters.get_commodity_map(entries)
            action_holdings = holdings_reports.classify_holdings_for_export(holdings_list,
                                                                            commodities_map)
            return fun(self, action_holdings)
        return wrapped

    @classify
    def test_classify_explicit_symbol(self, action_holdings):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "NASDAQ:AAPL"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        self.assertEqual(1, len(action_holdings))
        self.assertEqual("NASDAQ:AAPL", action_holdings[0][0])

    @classify
    def test_classify_explicit_cash(self, action_holdings):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "CASH"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        self.assertEqual(1, len(action_holdings))
        self.assertEqual("CASH", action_holdings[0][0])

    @classify
    def test_classify_explicit_ignore(self, action_holdings):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "IGNORE"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        self.assertEqual(1, len(action_holdings))
        self.assertEqual("IGNORE", action_holdings[0][0])

    @classify
    def test_classify_ticker(self, action_holdings):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          ticker: "NASDAQ:AAPL"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        self.assertEqual(1, len(action_holdings))
        self.assertEqual("NASDAQ:AAPL", action_holdings[0][0])

    @classify
    def test_classify_implicit(self, action_holdings):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        self.assertEqual(1, len(action_holdings))
        self.assertEqual("AAPL", action_holdings[0][0])

    @classify
    def test_classify_money(self, action_holdings):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity VMMXX
          ticker: "MUTF:VMMXX"
          export: "MONEY"

        2015-02-08 *
          Assets:Investing           100 VMMXX {1.00 USD}
          Equity:Opening-Balances
        """
        self.assertEqual(1, len(action_holdings))
        self.assertEqual("MUTF:VMMXX", action_holdings[0][0])

    @loader.loaddoc
    def test_get_money_instruments(self, entries, errors, options_map):
        """
        1900-01-01 commodity VMMXX
          quote: USD
          ticker: "MUTF:VMMXX"
          export: "MONEY"

        1900-01-01 commodity IGI806
          quote: CAD
          export: "MONEY"
        """
        commodities_map = getters.get_commodity_map(entries)
        self.assertEqual({'USD': 'MUTF:VMMXX',
                          'CAD': 'IGI806'},
                         holdings_reports.get_money_instruments(commodities_map))
