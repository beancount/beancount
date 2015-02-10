__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from beancount.core.amount import D
from beancount.reports import export_reports
from beancount import loader


EE = export_reports.ExportEntry

class TestCommodityExport(unittest.TestCase):

    def export(self, entries, options_map):
        (exported,
         converted,
         holdings_ignored) = export_reports.export_holdings(entries, options_map, False)
        return ([e._replace(holdings=None) for e in exported] +
                [e._replace(holdings=None) for e in converted])

    @loader.loaddoc
    def test_export_stock(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          ticker: "NASDAQ:AAPL"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        self.assertEqual([
            EE('NASDAQ:AAPL', 'USD', D('2'), D('410.00'), False, '', None),
            ], self.export(entries, options_map))

    @loader.loaddoc
    def test_export_mutfund(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity RBF1005
          ticker: "MUTF_CA:RBF1005"

        2015-02-08 *
          Assets:Investing           10.2479 RBF1005 {10.00 CAD}
          Equity:Opening-Balances
        """
        self.assertEqual([
            EE('MUTF_CA:RBF1005', 'CAD', D('10.2479'), D('10.00'), True, '', None),
            ], self.export(entries, options_map))



    @loader.loaddoc
    def test_export_cash(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity USD
          ticker: "MUTF_CA:RBF1005"

        2015-02-08 *
          Assets:Investing           10.2479 RBF1005 {10.00 CAD}
          Equity:Opening-Balances
        """
        self.assertEqual([
            EE('MUTF_CA:RBF1005', 'CAD', D('10.2479'), D('10.00'), True, '', None),
            ], self.export(entries, options_map))

# """
# at cost to operating ccy
#   Assets:US:LendingClub:FundsLent                    23863.09 LENCLUB            1.00 USD
#
# at cost to non-operating ccy
#   Assets:AU:SIL                                45000.00 SILSTK             0.07 AUD
#
# at price to operating ccy
#   Assets:US:Points:BofARewards                        1088.00 HSBCPTS            0.01 USD
#
# at price to non-operating ccy
#   Assets:US:Employer:Massage                          300.00 RUB                0.50 AUD
#
# with no price
#   Assets:US:Points:AmericanAirlines                    833.00 MILES             0.00 MILES
#
# just cash
#   Assets:CA:PayPal                                     953.18 CAD                0.00 CAD
#   Assets:Cash                                         1378.54 USD                0.00 USD
#   Assets:Cash:Foreign                                 3000.00 JPY                0.00 JPY
# """

    @loader.loaddoc
    def __test_export_undeclared(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, _ = export_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            ], exported)

    @loader.loaddoc
    def __test_export_omitted(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances

        """
        exported, _ = export_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            ], exported)


    @loader.loaddoc
    def __test_export_convert_declared(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity AAPL
          ticker: "cash"

        2015-02-08 *
          Assets:Investing           2 AAPL {410.00 USD}
          Equity:Opening-Balances
        """
        exported, _ = export_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            EE('CASH:USD', D('810.00'), D('810.00'), False, ''),
            ], exported)

    @loader.loaddoc
    def __test_export_convert_cash(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity USD
          ticker: "cash"

        2015-02-08 *
          Assets:Investing           100.00 USD
          Equity:Opening-Balances
        """
        exported, _ = export_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            EE('CASH:USD', D('100.00'), D('100.00'), False, ''),
            ], exported)

    @loader.loaddoc
    def __test_export_convert_cash_foreign(self, entries, unused_errors, options_map):
        """
        2000-01-01 open Assets:Investing
        2000-01-01 open Equity:Opening-Balances

        2000-01-01 commodity CAD
          ticker: "cash"

        2015-02-08 *
          Assets:Investing           100.00 CAD
          Equity:Opening-Balances
        """
        exported, _ = export_reports.export_holdings(entries, options_map, False)
        self.assertEqual([
            EE('CASH:CAD', D('100.00'), D('100.00'), False, ''),
            ], exported)
