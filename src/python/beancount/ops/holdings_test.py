import unittest
import pprint
import datetime

from beancount.core.amount import to_decimal
from beancount.ops import holdings
from beancount.ops import prices
from beancount.parser import parsedoc


D = to_decimal


class TestPositionEntries(unittest.TestCase):

    @parsedoc
    def test_get_final_holdings(self, entries, _, __):
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
        """
        holdings_list = holdings.get_final_holdings(entries)

        fields = 'account number currency cost_number cost_currency book_value'.split()
        holdings_list = sorted(list(getattr(holding, field) for field in fields)
                                for holding in holdings_list)
        expected_values = [
            ['Assets:Account1', D('10'), 'GOOG', D('523.46'), 'USD', D('5234.60')],
            ['Assets:Account1', D('11'), 'GOOG', D('518.73'), 'USD', D('5706.03')],
            ['Assets:Account3', D('50'), 'GOOG', D('540.00'), 'USD', D('27000.00')],
            ['Assets:Cash', D('12059.37'), 'USD', None, None, None],
            ['Equity:Unknown', D('-50000'), 'USD', None, None, None]
        ]
        self.assertEqual(expected_values, holdings_list)


    @parsedoc
    def test_add_prices_to_holdings(self, entries, _, __):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Cash

        2013-03-01 price GOOG 521.02 USD
        2013-05-01 price GOOG 578.92 USD

        2013-04-01 *
          Assets:Account1             15 GOOG {518.73 USD}
          Assets:Cash
        """
        holdings_list = holdings.get_final_holdings(entries)
        price_map = prices.build_price_map(entries)
        holdings_list = holdings.add_prices_to_holdings(holdings_list, price_map)

        expected_values = [
            holdings.Holding('Assets:Account1',
                             D('15'), 'GOOG', D('518.73'), 'USD', D('7780.95'), D('8683.80'),
                             D('578.92'), datetime.date(2013, 5, 1)),
            holdings.Holding('Assets:Cash',
                             D('-7780.95'), 'USD', None, None, None, D('-7780.95'),
                             D('1'), None),
        ]
        self.assertEqual(expected_values, holdings_list)





# Maybe convert this to a single function, that accepts optionally a price_map
# and immediately inserts the prices if specified. This would be faster and simpler I think.

# Maybe rename bean-positions to bean-holdings



# def get_holdings_as_dataframe(entries, price_map):
__incomplete__ = True
