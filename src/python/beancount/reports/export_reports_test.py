__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import functools

from beancount.core.number import D
from beancount.core import getters
from beancount.reports import export_reports
from beancount.reports import holdings_reports
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

    def test_report_export_portfolio(self):
        report_ = export_reports.ExportPortfolioReport.from_args([])
        format_ = 'ofx'
        output = report_.render(self.entries, self.errors, self.options_map, format_)
        self.assertTrue(output)


class TestCommodityClassifications(unittest.TestCase):

    def classify(fun):
        @loader.load_doc()
        @functools.wraps(fun)
        def wrapped(self, entries, unused_errors, options_map):
            holdings_list, _ = holdings_reports.get_assets_holdings(entries, options_map)
            commodities_map = getters.get_commodity_map(entries)
            action_holdings = export_reports.classify_holdings_for_export(holdings_list,
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
          export: "NASDAQ:AAPL"

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
          export: "MUTF:VMMXX (MONEY:USD)"

        2015-02-08 *
          Assets:Investing           100 VMMXX {1.00 USD}
          Equity:Opening-Balances
        """
        self.assertEqual(1, len(action_holdings))
        self.assertEqual("MUTF:VMMXX", action_holdings[0][0])

    @loader.load_doc()
    def test_get_money_instruments(self, entries, errors, options_map):
        """
        1900-01-01 commodity VMMXX
          export: "MUTF:VMMXX (MONEY:USD)"

        1900-01-01 commodity IGI806
          export: "(MONEY:CAD)"
        """
        commodities_map = getters.get_commodity_map(entries)
        self.assertEqual({'USD': 'MUTF:VMMXX',
                          'CAD': 'IGI806'},
                         export_reports.get_money_instruments(commodities_map))


EE = export_reports.ExportEntry

class TestCommodityExport(unittest.TestCase):

    def export(self, entries, options_map, aggregate_by_commodity=False):
        (exported,
         converted,
         holdings_ignored) = export_reports.export_holdings(entries, options_map, False,
                                                            aggregate_by_commodity)
        return ([e._replace(holdings=None) for e in exported],
                [e._replace(holdings=None) for e in converted],
                holdings_ignored)

    @loader.load_doc()
    def test_export_implicit_stock(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "NASDAQ:AAPL"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertEqual([
            EE('NASDAQ:AAPL', 'USD', D('2'), D('410.00'), False, '', None),
            ], exported)

    @loader.load_doc()
    def test_export_implicit_mutfund(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity RBF1005
          export: "MUTF_CA:RBF1005"

        2015-02-08 *
          Assets:Investing           10.2479 RBF1005 {10.00 CAD}
          Equity:Opening-Balances
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertEqual([
            EE('MUTF_CA:RBF1005', 'CAD', D('10.2479'), D('10.00'), True, '', None),
            ], exported)

    @loader.load_doc()
    def test_export_implicit_unspecified(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertEqual([
            EE('AAPL', 'USD', D('2'), D('410.00'), False, '', None),
            ], exported)

    @loader.load_doc()
    def test_export_implicit_absent(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertEqual([
            EE('AAPL', 'USD', D('2'), D('410.00'), False, '', None),
            ], exported)

    @loader.load_doc()
    def test_export_explicit_stock(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "NASDAQ:AAPL"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertEqual([
            EE('NASDAQ:AAPL', 'USD', D('2'), D('410.00'), False, '', None),
            ], exported)

    @loader.load_doc()
    def test_export_cash_at_cost(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "CASH"

        2000-01-01 commodity VMMXX
          export: "MUTF:VMMXX (MONEY:USD)"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances

        2015-02-09 price AAPL  410.00 USD
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertEqual([
            EE('MUTF:VMMXX', 'USD', D('820.00'), D('1.00'), True, '', None),
            ], converted)

    @loader.load_doc()
    def test_export_cash_at_price(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "CASH"

        2000-01-01 commodity VMMXX
          export: "MUTF:VMMXX (MONEY:USD)"

        2015-02-08 *
          Assets:Investing           2 AAPL
          Equity:Opening-Balances

        2015-02-09 price AAPL 410.00 USD
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertEqual([
            EE('MUTF:VMMXX', 'USD', D('820.00'), D('1.00'), True, '', None),
            ], converted)

    @loader.load_doc()
    def test_export_cash_ignored_no_money(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity JPY
          export: "CASH"

        2000-01-01 commodity VMMXX
          export: "MUTF:VMMXX (MONEY:USD)"

        2015-02-08 *
          Assets:Investing           2000.00 JPY
          Equity:Opening-Balances
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertFalse(exported)
        self.assertFalse(converted)
        self.assertEqual(1, len(ignored))

    @loader.load_doc()
    def test_export_ignored(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "IGNORE"

        2000-01-01 commodity VMMXX
          export: "MUTF:VMMXX (MONEY:USD)"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, converted, ignored = self.export(entries, options_map)
        self.assertFalse(exported)
        self.assertFalse(converted)
        self.assertEqual(1, len(ignored))

    @loader.load_doc()
    def test_export_aggregate(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          export: "NASDAQ:AAPL"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances

        2015-03-08 *
          Assets:Investing           3 AAPL {400.00 USD}
          Equity:Opening-Balances

        2015-04-08 *
          Assets:Investing           1 HOOL {500.00 USD}
          Equity:Opening-Balances

        """
        exported, converted, ignored = self.export(entries, options_map,
                                                   aggregate_by_commodity=True)
        self.assertListEqual([
            ('NASDAQ:AAPL', 'USD', D('5'), D('404.00'), False, '', None),
            ('HOOL', 'USD', D('1'), D('500.00'), False, '', None),
            ], exported)
        self.assertFalse(converted)
        self.assertFalse(ignored)
