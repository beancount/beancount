import unittest
import pprint
import datetime

from beancount.core.amount import to_decimal, Decimal
from beancount.ops import holdings
from beancount.parser import parsedoc


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
        holdings_ = holdings.get_final_holdings(entries)

        fields = 'account currency cost_currency number cost_number'.split()
        holdings_list = sorted(list(holding[field] for field in fields)
                                for holding in holdings_)
        expected_values = [
            ['Assets:Account1', 'GOOG', 'USD', Decimal('10'), Decimal('523.46')],
            ['Assets:Account1', 'GOOG', 'USD', Decimal('11'), Decimal('518.73')],
            ['Assets:Account3', 'GOOG', 'USD', Decimal('50'), Decimal('540.00')],
            ['Assets:Cash', 'USD', None, Decimal('12059.37'), None],
            ['Equity:Unknown', 'USD', None, Decimal('-50000'), None]
        ]
        self.assertEqual(expected_values, holdings_list)


    def test_add_prices_to_holdings(self):
        ## FIXME: TODO
        pass # positions.get_priced_positions(entries, price_map)

# def get_positions_as_dataframe(entries, price_map):



__incomplete__ = True
