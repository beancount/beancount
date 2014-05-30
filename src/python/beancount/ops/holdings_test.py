import itertools
import unittest
import datetime

from beancount.core.amount import to_decimal
from beancount.ops import holdings
from beancount.ops import prices
from beancount.parser import parsedoc


D = to_decimal


class TestHoldings(unittest.TestCase):

    @parsedoc
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
          Assets:Account1             15 GOOG {518.73 USD}
          Assets:Cash

        2013-04-02 *
          Assets:Account1             10 GOOG {523.46 USD}
          Assets:Cash

        2013-04-03 *
          Assets:Account1             -4 GOOG {518.73 USD}
          Assets:Cash

        2013-04-02 *
          Assets:Account2            20 ITOT {85.l95 USD}
          Assets:Cash

        2013-04-03 *
          Assets:Account3             50 GOOG {540.00 USD} @ 560.00 USD
          Assets:Cash

        2013-04-10 *
          Assets:Cash			5111 USD
          Liabilities:Loan
        """
        holdings_list = holdings.get_final_holdings(entries)

        holdings_list = sorted(map(tuple, holdings_list))
        expected_values = [
            ('Assets:Account1', D('10'), 'GOOG', D('523.46'), 'USD',
             D('5234.60'), None, None, None),
            ('Assets:Account1', D('11'), 'GOOG', D('518.73'), 'USD',
             D('5706.03'), None, None, None),
            ('Assets:Account3', D('50'), 'GOOG', D('540.00'), 'USD',
             D('27000.00'), None, None, None),
            ('Assets:Cash', D('17170.37'), 'USD', None, None,
             None, None, None, None),
            ('Equity:Unknown', D('-50000'), 'USD', None, None,
             None, None, None, None),
            ('Liabilities:Loan', D('-5111'), 'USD', None, None,
             None, None, None, None),
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

    @parsedoc
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
          Assets:Account1             15 GOOG {518.73 USD}
          Assets:Cash

        2013-06-01 price GOOG  578.02 USD

        """
        price_map = prices.build_price_map(entries)
        holdings_list = holdings.get_final_holdings(entries,
                                                    ('Assets', 'Liabilities'),
                                                    price_map)

        holdings_list = sorted(map(tuple, holdings_list))
        expected_values = [
            ('Assets:Account1', D('15'), 'GOOG', D('518.73'), 'USD',
             D('7780.95'), D('8670.30'),
             D('578.02'), datetime.date(2013, 6, 1)),
            ('Assets:Cash', D('42219.05'), 'USD', None, None, None, None, None, None),
            # Notice no Equity account.
        ]
        self.assertEqual(expected_values, holdings_list)

    def test_aggregate_holdings_list(self):
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Account1', D('10'), 'GOOG', D('518.73'), 'USD',
             D('5187.30'), D('5780.20'), D('578.02'), datetime.date(2014, 2, 1)),
            ('Assets:Account2', D('20'), 'GOOG', D('519.24'), 'USD',
             D('10384.80'), D('11622.00'), D('581.10'), datetime.date(2014, 2, 15)),
        ]))

        expected_holding = holdings.Holding(
            None, D('30'), 'GOOG', D('519.07'), 'USD',
            D('15572.10'), D('17402.20'), D('580.0733333333333333333333333'), None)
        self.assertEqual(expected_holding, holdings.aggregate_holdings_list(test_holdings))

    def test_aggregate_by_base_quote(self):
        # Note: Two different prices on GOOG on purpose.
        test_holdings = list(itertools.starmap(holdings.Holding, [
            ('Assets:Cash', D('101.11'), 'USD', None, None,
             None, None, None, None),

            ('Assets:Account1', D('10'), 'GOOG', D('518.73'), 'USD',
             D('5187.30'), D('5780.20'), D('578.02'), datetime.date(2014, 2, 1)),
            ('Assets:Account2', D('20'), 'GOOG', D('519.24'), 'USD',
             D('10384.80'), D('11622.00'), D('581.10'), datetime.date(2014, 2, 15)),

            ('Assets:Account1', D('10'), 'AAPL', D('593.27'), 'USD',
             D('5932.70'), D('6000.10'), D('600.01'), datetime.date(2014, 3, 1)),
        ]))
        expected_holdings = list(itertools.starmap(holdings.Holding, [
            (None, D('10'), 'AAPL', D('593.27'), 'USD',
             D('5932.70'), D('6000.10'), D('600.01'), None),

            (None, D('30'), 'GOOG', D('519.07'), 'USD',
             D('15572.10'), D('17402.20'), D('580.0733333333333333333333333'), None),

            (None, D('101.11'), 'USD', None, None,
             None, None, None, None),
        ]))
        self.assertEqual(expected_holdings, holdings.aggregate_by_base_quote(test_holdings))


# Just missing a test for reduce_relative().
__incomplete__ = True
