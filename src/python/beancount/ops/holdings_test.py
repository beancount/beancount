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

        holdings_list = sorted(map(tuple, holdings_list))
        expected_values = [
            ('Assets:Account1', D('10'), 'GOOG', D('523.46'), 'USD', D('5234.60'), None, None, None),
            ('Assets:Account1', D('11'), 'GOOG', D('518.73'), 'USD', D('5706.03'), None, None, None),
            ('Assets:Account3', D('50'), 'GOOG', D('540.00'), 'USD', D('27000.00'), None, None, None),
            ('Assets:Cash', D('12059.37'), 'USD', None, None, None, None, None, None),
            ('Equity:Unknown', D('-50000'), 'USD', None, None, None, None, None, None),
        ]
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
        holdings_list = holdings.get_final_holdings(entries, price_map)

        holdings_list = sorted(map(tuple, holdings_list))
        expected_values = [
            ('Assets:Account1', D('15'), 'GOOG', D('518.73'), 'USD', D('7780.95'), D('8670.30'),
             D('578.02'), datetime.date(2013, 6, 1)),
            ('Assets:Cash', D('42219.05'), 'USD', None, None, None, None, None, None),
            ('Equity:Unknown', D('-50000'), 'USD', None, None, None, None, None, None),
        ]
        self.assertEqual(expected_values, holdings_list)


# Maybe rename bean-positions to bean-holdings
# Add function from web.py, see in positions_byinstrument()
__incomplete__ = True
