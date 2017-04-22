__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import itertools
import unittest
import datetime

from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core.amount import A
from beancount.core import position
from beancount.core import data
from beancount.ops import holdings
from beancount.core import prices
from beancount import loader


class TestHoldings(unittest.TestCase):

    maxDiff = 4096

    @loader.load_doc()
    def test_get_final_holdings(self, entries, _, __):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Account3
        2013-01-01 open Assets:Cash
        2013-01-01 open Liabilities:Loan
        2013-01-01 open Equity:Unknown

        2013-04-05 *
          Equity:Unknown
          Assets:Cash			50000 USD

        2013-04-01 *
          Assets:Account1             15 HOOL {518.73 USD}
          Assets:Cash

        2013-04-02 *
          Assets:Account1             10 HOOL {523.46 USD}
          Assets:Cash

        2013-04-03 *
          Assets:Account1             -4 HOOL {518.73 USD}
          Assets:Cash

        2013-04-02 *
          Assets:Account2            20 ITOT {85.195 USD}
          Assets:Cash

        2013-04-03 *
          Assets:Account3             50 HOOL {540.00 USD} @ 560.00 USD
          Assets:Cash

        2013-04-10 *
          Assets:Cash			5111 USD
          Liabilities:Loan
        """
        holdings_list = holdings.get_final_holdings(entries)

        holdings_list = sorted(map(tuple, holdings_list))
        expected_values = [
            ('Assets:Account1', D('10'), 'HOOL', D('523.46'), 'USD',
             D('5234.60'), None, None, None),
            ('Assets:Account1', D('11'), 'HOOL', D('518.73'), 'USD',
             D('5706.03'), None, None, None),
            ('Assets:Account2', D('20'), 'ITOT', D('85.195'), 'USD',
             D('1703.900'), None, None, None),
            ('Assets:Account3', D('50'), 'HOOL', D('540.00'), 'USD',
             D('27000.00'), None, None, None),
            ('Assets:Cash', D('15466.470'), 'USD', None, 'USD',
             D('15466.470'), D('15466.470'), None, None),
            ('Equity:Unknown', D('-50000'), 'USD', None, 'USD',
             D('-50000'), D('-50000'), None, None),
            ('Liabilities:Loan', D('-5111'), 'USD', None, 'USD',
             D('-5111'), D('-5111'), None, None),
        ]
        self.assertEqual(expected_values, holdings_list)

        # Try with some account type restrictions.
        holdings_list = holdings.get_final_holdings(entries, ('Assets', 'Liabilities'))
        holdings_list = sorted(map(tuple, holdings_list))
        expected_values = [holding
                           for holding in expected_values
                           if (holding[0].startswith('Assets') or
                               holding[0].startswith('Liabilities'))]
        self.assertEqual(expected_values, holdings_list)

        # Try with some account type restrictions.
        holdings_list = holdings.get_final_holdings(entries, ('Assets',))
        holdings_list = sorted(map(tuple, holdings_list))
        expected_values = [holding
                           for holding in expected_values
                           if holding[0].startswith('Assets')]
        self.assertEqual(expected_values, holdings_list)

    @loader.load_doc()
    def test_get_final_holdings__check_no_aggregates(self, entries, _, __):
        """
        plugin "beancount.plugins.unrealized" "Unrealized"

        2013-01-01 open Assets:Investment   HOOL
        2013-01-01 open Assets:Cash         USD

        2013-04-01 *
          Assets:Investment             15 HOOL {518.73 USD}
          Assets:Cash

        2013-06-01 price HOOL  600.00 USD
        """
        holdings_list = holdings.get_final_holdings(entries)

        # Ensure that there is no Unrealized balance or sub-account.
        self.assertEqual({'Assets:Cash', 'Assets:Investment'},
                         set(holding.account for holding in holdings_list))

    @loader.load_doc()
    def test_get_final_holdings_with_prices(self, entries, _, __):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Account3
        2013-01-01 open Assets:Cash
        2013-01-01 open Equity:Unknown

        2013-04-05 *
          Equity:Unknown
          Assets:Cash			50000 USD

        2013-04-01 *
          Assets:Account1             15 HOOL {518.73 USD}
          Assets:Cash

        2013-06-01 price HOOL  578.02 USD

        """
        price_map = prices.build_price_map(entries)
        holdings_list = holdings.get_final_holdings(entries,
                                                    ('Assets', 'Liabilities'),
                                                    price_map)

        holdings_list = sorted(map(tuple, holdings_list))
        expected_values = [
            ('Assets:Account1', D('15'), 'HOOL', D('518.73'), 'USD',
             D('7780.95'), D('8670.30'),
             D('578.02'), datetime.date(2013, 6, 1)),
            ('Assets:Cash', D('42219.05'), 'USD', None, 'USD',
             D('42219.05'), D('42219.05'), None, None),
            # Notice no Equity account.
        ]
        self.assertEqual(expected_values, holdings_list)

    @loader.load_doc()
    def test_get_final_holdings__zero_position(self, entries, _, __):
        """
        1970-01-01 open Assets:Stocks:NYA
        1970-01-01 open Expenses:Financial:Commissions
        1970-01-01 open Assets:Current
        1970-01-01 open Income:Dividends:NYA

        2012-07-02 ! "I received 1 new share in dividend, without paying"
          Assets:Stocks:NYA 1 NYA {0 EUR}
          Income:Dividends:NYA -0 EUR

        2014-11-13 balance Assets:Stocks:NYA 1 NYA
        """
        price_map = prices.build_price_map(entries)
        holdings_list = holdings.get_final_holdings(entries,
                                                    ('Assets', 'Liabilities'),
                                                    price_map)
        self.assertEqual(1, len(holdings_list))
        self.assertEqual('EUR', holdings_list[0].cost_currency)

    @loader.load_doc()
    def test_get_commodities_at_date(self, entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Account3
        2013-01-01 open Assets:Cash
        2013-01-01 open Liabilities:Loan
        2013-01-01 open Equity:Unknown

        2000-01-01 commodity HOOL
          quote: USD

        2000-01-01 commodity ITOT
          ticker: "NYSEARCA:ITOT"

        2013-03-05 *
          Equity:Unknown
          Assets:Cash			50000 USD

        2013-04-01 *
          Assets:Account1             15 HOOL {518.73 USD}
          Assets:Cash

        2013-04-02 *
          Assets:Account1             10 HOOL {523.46 USD}
          Assets:Cash

        2013-04-03 *
          Assets:Account1             -4 HOOL {518.73 USD}
          Assets:Cash

        2013-04-02 *
          Assets:Account2            20 ITOT {85.195 USD}
          Assets:Cash

        2013-04-03 *
          Assets:Account3             50 HOOL {540.00 USD} @ 560.00 USD
          Assets:Cash

        2013-04-10 *
          Assets:Cash			5111 USD
          Liabilities:Loan
        """
        commodities = holdings.get_commodities_at_date(entries, options_map)
        self.assertEqual([('HOOL', 'USD', 'USD', None),
                          ('ITOT', 'USD', None, 'NYSEARCA:ITOT'),
                          ('USD', 'USD', None, None)],
                         commodities)

        commodities = holdings.get_commodities_at_date(entries, options_map,
                                                       date=datetime.date(2013, 4, 2))
        self.assertEqual([('HOOL', 'USD', 'USD', None),
                          ('USD', 'USD', None, None)],
                         commodities)


    def test_aggregate_holdings_list(self):
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('10'), 'HOOL', D('518.73'), 'USD',
             D('5187.30'), D('5780.20'), D('578.02'), datetime.date(2014, 2, 1)),
            ('Assets:Account2', D('20'), 'HOOL', D('519.24'), 'USD',
             D('10384.80'), D('11622.00'), D('581.10'), datetime.date(2014, 2, 15)),
        ]))
        expected_holding = holdings.Holding(
            'Assets', D('30'), 'HOOL', D('519.07'), 'USD',
            D('15572.10'), D('17402.20'), D('580.0733333333333333333333333'), None)
        self.assertEqual(expected_holding, holdings.aggregate_holdings_list(test_holdings))

        # Test with zero units.
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Acc1', D('10'), 'HOOL', D('300'), 'USD', D('3000'),
             None, None, None),
            ('Assets:Acc1', D('-10'), 'HOOL', D('400'), 'USD', D('-4000'),
             None, None, None),
        ]))
        expected_holding = holdings.Holding(
            'Assets:Acc1', D('0'), 'HOOL', None, 'USD', D('-1000'), None, None, None)
        self.assertEqual(expected_holding, holdings.aggregate_holdings_list(test_holdings))

        # Test with aggregate holdings with no cost nor price price.
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('10'), 'HOOL', None, 'USD',
             D('5187.30'), D('5780.20'), None, None),
            ('Assets:Account2', D('20'), 'HOOL', None, 'USD',
             D('10384.80'), D('11622.00'), None, None),
        ]))
        expected_holding = holdings.Holding(
            'Assets', D('30'), 'HOOL', D('519.07'), 'USD',
            D('15572.10'), D('17402.20'), D('580.0733333333333333333333333'), None)
        self.assertEqual(expected_holding, holdings.aggregate_holdings_list(test_holdings))

    def test_aggregate_holdings_by__commodity(self):
        # Note: Two different prices on HOOL on purpose.
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Cash', D('101.11'), 'USD', None, None,
             None, None, None, None),

            ('Assets:Account1', D('10'), 'HOOL', D('518.73'), 'USD',
             D('5187.30'), D('5780.20'), D('578.02'), datetime.date(2014, 2, 1)),
            ('Assets:Account2', D('20'), 'HOOL', D('519.24'), 'USD',
             D('10384.80'), D('11622.00'), D('581.10'), datetime.date(2014, 2, 15)),

            ('Assets:Account1', D('10'), 'AAPL', D('593.27'), 'USD',
             D('5932.70'), D('6000.10'), D('600.01'), datetime.date(2014, 3, 1)),
        ]))
        expected_holdings = sorted(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('10'), 'AAPL', D('593.27'), 'USD',
             D('5932.70'), D('6000.10'), D('600.01'), datetime.date(2014, 3, 1)),

            ('Assets', D('30'), 'HOOL', D('519.07'), 'USD',
             D('15572.10'), D('17402.20'), D('580.0733333333333333333333333'), None),

            ('Assets:Cash', D('101.11'), 'USD', None, None,
             None, None, None, None),
        ]))
        self.assertEqual(expected_holdings,
                         holdings.aggregate_holdings_by(test_holdings,
                                                        lambda holding: holding.currency))

    def test_aggregate_holdings_by__account(self):
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Cash', D('101.11'), 'USD', None, None,
             None, None, None, None),

            ('Assets:Account1', D('10'), 'HOOL', D('518.73'), 'USD',
             D('5187.30'), D('5780.20'), D('578.02'), datetime.date(2014, 2, 1)),
            ('Assets:Account2', D('20'), 'HOOL', D('519.24'), 'USD',
             D('10384.80'), D('11622.00'), D('581.10'), datetime.date(2014, 2, 15)),

            ('Assets:Account1', D('10'), 'AAPL', D('593.27'), 'USD',
             D('5932.70'), D('6000.10'), D('600.01'), datetime.date(2014, 3, 1)),
        ]))
        expected_holdings = sorted(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('0'), '*', D('556.00'), 'USD',
             D('11120.00'), D('11780.30'), D('589.015'), None),

            ('Assets:Account2', D('20'), 'HOOL', D('519.24'), 'USD',
             D('10384.80'), D('11622.00'), D('581.10'), datetime.date(2014, 2, 15)),

            ('Assets:Cash', D('101.11'), 'USD', None, None,
             None, None, None, None),
        ]))
        self.assertEqual(expected_holdings,
                         holdings.aggregate_holdings_by(test_holdings,
                                                        lambda holding: holding.account))

    def test_aggregate_holdings__same_price_same_date(self):
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('10'), 'HOOL', D('500'), 'USD', D('5000'), D('6000'),
             D('600'), datetime.date(2014, 2, 1)),
            ('Assets:Account1', D('20'), 'HOOL', D('530'), 'USD', D('10600'), D('12000'),
             D('600'), datetime.date(2014, 2, 1)),
        ]))
        expected_holdings = sorted(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('30'), 'HOOL', D('520'), 'USD', D('15600'), D('18000'),
             D('600'), datetime.date(2014, 2, 1)),
        ]))
        self.assertEqual(expected_holdings,
                         holdings.aggregate_holdings_by(test_holdings,
                                                        lambda holding: holding.account))

    def test_aggregate_holdings__diff_price_same_date(self):
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('10'), 'HOOL', D('500'), 'USD', D('5000'), D('6000'),
             D('600'), datetime.date(2014, 2, 1)),
            ('Assets:Account1', D('20'), 'HOOL', D('530'), 'USD', D('10600'), D('12000'),
             D('630'), datetime.date(2014, 2, 1)),
        ]))
        # Price is recalculated from the market value, date is maintained.
        expected_holdings = sorted(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('30'), 'HOOL', D('520'), 'USD', D('15600'), D('18000'),
             D('600'), datetime.date(2014, 2, 1)),
        ]))
        self.assertEqual(expected_holdings,
                         holdings.aggregate_holdings_by(test_holdings,
                                                        lambda holding: holding.account))

    def test_aggregate_holdings__same_price_diff_date(self):
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('10'), 'HOOL', D('500'), 'USD', D('5000'), D('6000'),
             D('600'), datetime.date(2014, 2, 1)),
            ('Assets:Account1', D('20'), 'HOOL', D('530'), 'USD', D('10600'), D('12000'),
             D('600'), datetime.date(2014, 2, 2)),
        ]))
        # Date is cleared.
        expected_holdings = sorted(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('30'), 'HOOL', D('520'), 'USD', D('15600'), D('18000'),
             D('600'), None),
        ]))
        self.assertEqual(expected_holdings,
                         holdings.aggregate_holdings_by(test_holdings,
                                                        lambda holding: holding.account))

    def test_aggregate_holdings__diff_price_diff_date(self):
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('10'), 'HOOL', D('500'), 'USD', D('5000'), D('6000'),
             D('610'), datetime.date(2014, 2, 1)),
            ('Assets:Account1', D('20'), 'HOOL', D('530'), 'USD', D('10600'), D('12000'),
             D('600'), datetime.date(2014, 2, 2)),
        ]))
        # Price is recalculated from the market value, date is maintained.
        expected_holdings = sorted(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('30'), 'HOOL', D('520'), 'USD', D('15600'), D('18000'),
             D('600'), None),
        ]))
        self.assertEqual(expected_holdings,
                         holdings.aggregate_holdings_by(test_holdings,
                                                        lambda holding: holding.account))

    @loader.load_doc()
    def test_convert_to_currency(self, entries, _, __):
        """
        2013-01-01 price CAD 1.1 USD
        ; We don't include a price point for NOK. It's unknown.
        ; 2013-01-01 open Assets:Account2
        """
        test_holdings = list(itertools.starmap(holdings.Holding, [

            # ------------ cost currency == target currency
            # currency != target currency
            (None, D('100.00'), 'IVV', D('200'), 'USD', D('10'), D('11'), D('12'), None),
            # currency == target currency
            (None, D('100.00'), 'USD', D('200'), 'USD', D('10'), D('11'), D('12'), None),
            # currency == None
            (None, D('100.00'), None, D('200'), 'USD', D('10'), D('11'), D('12'), None),

            # ------------ cost currency == other currency
            # cost currency available in price map
            (None, D('100.00'), 'XSP', D('200'), 'CAD', D('10'), D('11'), D('12'), None),
            # cost currency not available in price map
            (None, D('100.00'), 'AGF', D('200'), 'NOK', D('10'), D('11'), D('12'), None),
            # currency == target currency, available in price map
            (None, D('100.00'), 'USD', D('200'), 'CAD', D('10'), D('11'), D('12'), None),
            # currency == target currency, not available in price map
            (None, D('100.00'), 'USD', D('200'), 'NOK', D('10'), D('11'), D('12'), None),
            # cost currency available in price map, and currency == None
            (None, D('100.00'), None, D('200'), 'CAD', D('10'), D('11'), D('12'), None),

            # ------------ cost currency == None
            # currency available in price map
            (None, D('100.00'), 'CAD', D('1.2'), None, D('10'), D('11'), D('12'), None),
            # currency available in price map, with no values (should be filled in)
            (None, D('100.00'), 'CAD', D('1.2'), None, None, None, None, None),
            # currency not available in price map
            (None, D('100.00'), 'EUR', D('1.2'), None, D('10'), D('11'), D('12'), None),
            # currency = target currency
            (None, D('100.00'), 'USD', D('1.2'), None, D('10'), D('11'), D('12'), None),

            ]))

        price_map = prices.build_price_map(entries)
        converted_holdings = holdings.convert_to_currency(price_map, 'USD', test_holdings)
        expected_holdings = list(itertools.starmap(holdings.Holding, [
            (None, D('100.00'), 'IVV', D('200'), 'USD', D('10'), D('11'), D('12'), None),
            (None, D('100.00'), 'USD', D('200'), 'USD', D('10'), D('11'), D('12'), None),
            (None, D('100.00'), None, D('200'), 'USD', D('10'), D('11'), D('12'), None),
            (None, D('100.00'), 'XSP', D('220.0'), 'USD', D('11.0'), D('12.1'), D('13.2'),
             None),
            (None, D('100.00'), 'AGF', None, None, None, None, None, None),
            (None, D('100.00'), 'USD', D('220.0'), 'USD', D('11.0'), D('12.1'), D('13.2'),
             None),
            (None, D('100.00'), 'USD', None, None, None, None, None, None),
            (None, D('100.00'), None, D('220.0'), 'USD', D('11.0'), D('12.1'), D('13.2'),
             None),
            (None, D('100.00'), 'CAD', D('1.32'), 'USD', D('11.0'), D('12.1'), D('13.2'),
             None),
            (None, D('100.00'), 'CAD', D('1.32'), 'USD', D('110.0'), D('110.0'), None,
             None),
            (None, D('100.00'), 'EUR', None, None, None, None, None, None),
            (None, D('100.00'), 'USD', D('1.2'), 'USD', D('10'), D('11'), D('12'), None),
            ]))
        self.assertEqual(expected_holdings, converted_holdings)

        # Fail elegantly if the currency itself is None.
        none_holding = holdings.Holding(None, D('100.00'), None, D('200'), None,
                                        None, None, None, None)
        with self.assertRaises(ValueError):
            converted_holdings = holdings.convert_to_currency(price_map, 'USD',
                                                              [none_holding])

    def test_reduce_relative(self):
        # Test with a few different cost currencies.
        test_holdings = list(itertools.starmap(holdings.Holding, [
            (None, D('1'), 'BLA', D('200'), 'USD', D('10'), D('1000'), D('1100'), None),
            (None, D('1'), 'BLA', D('200'), 'USD', D('10'), D('3000'), D('300'), None),
            (None, D('1'), 'BLA', D('200'), 'CAD', D('10'), D('500'), D('600'), None),
            ]))
        converted_holdings = holdings.reduce_relative(test_holdings)
        expected_holdings = list(itertools.starmap(holdings.Holding, [
            (None, D('1'), 'BLA', D('200'), 'USD', D('0.5'), D('0.75'), D('300'), None),
            (None, D('1'), 'BLA', D('200'), 'USD', D('0.5'), D('0.25'), D('1100'), None),
            (None, D('1'), 'BLA', D('200'), 'CAD', D('1'), D('1'), D('600'), None),
            ]))
        self.assertEqual(expected_holdings, converted_holdings)


        # Test with a single cost currency (and some Nones), ensure the total is 100%.
        test_holdings = list(itertools.starmap(holdings.Holding, [
            (None, D('1'), 'BLA', D('200'), 'USD', D('10'), D('1000'), D('1100'), None),
            (None, D('1'), 'BLA', D('200'), 'USD', D('10'), D('3000'), D('300'), None),
            (None, D('1'), 'BLA', D('200'), None, None, None, D('600'), None),
            ]))
        converted_holdings = holdings.reduce_relative(test_holdings)
        expected_holdings = list(itertools.starmap(holdings.Holding, [
            (None, D('1'), 'BLA', D('200'), 'USD', D('0.5'), D('0.75'), D('300'), None),
            (None, D('1'), 'BLA', D('200'), 'USD', D('0.5'), D('0.25'), D('1100'), None),
            (None, D('1'), 'BLA', D('200'), None, None, None, D('600'), None),
            ]))
        self.assertEqual(expected_holdings, converted_holdings)
        self.assertEqual(D('1'), sum(holding.market_value or ZERO
                                     for holding in converted_holdings))

    def test_scale_holding(self):
        test_holding = holdings.Holding(
            'Assets:US:Checking', D('100'), 'MSFT', D('54.34'), 'USD',
            D('5434.00'), D('6000.00'), D('60'), datetime.date(2012, 5, 2))
        expected_holding = holdings.Holding(
            'Assets:US:Checking', D('70.0'), 'MSFT', D('54.34'), 'USD',
            D('3803.80'), D('4200.00'), D('60'), datetime.date(2012, 5, 2))
        self.assertEqual(expected_holding, holdings.scale_holding(test_holding, D('0.7')))

    def test_holding_to_position(self):
        test_holding = holdings.Holding(
            'Assets:US:Checking', D('100'), 'MSFT', D('54.34'), 'USD',
            D('5434.00'), D('6000.00'), D('60'), datetime.date(2012, 5, 2))
        actual_position = holdings.holding_to_position(test_holding)
        expected_position = position.from_string('100 MSFT {54.34 USD}')
        self.assertEqual(expected_position, actual_position)

        test_holding = holdings.Holding(
            'Assets:US:Checking', D('100'), 'USD', None, None,
            None, None, None, datetime.date(2012, 5, 2))
        actual_position = holdings.holding_to_position(test_holding)
        expected_position = position.from_string('100.00 USD')
        self.assertEqual(expected_position, actual_position)

    def test_holding_to_posting(self):
        test_holding = holdings.Holding(
            'Assets:US:Checking', D('100'), 'MSFT', D('54.34'), 'USD',
            D('5434.00'), D('6000.00'), D('60'), datetime.date(2012, 5, 2))

        posting = holdings.holding_to_posting(test_holding)
        self.assertTrue(isinstance(posting, data.Posting))

        expected_position = position.from_string('100 MSFT {54.34 USD}')
        self.assertEqual(expected_position, position.Position(posting.units, posting.cost))

        expected_price = A('60.00 USD')
        self.assertEqual(expected_price, posting.price)
