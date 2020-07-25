"""Tests for price finding routines.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount.ops import find_prices
from beancount import loader


class TestFromFile(unittest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, _, __):
        """
        2000-01-10 open Assets:US:Investments:QQQ
        2000-01-10 open Assets:CA:Investments:XSP
        2000-01-10 open Assets:Cash
        2000-01-10 open Assets:External
        2000-01-10 open Expenses:Foreign

        2010-01-01 commodity USD
        2010-01-01 commodity QQQ
        2010-01-01 commodity XSP
        2010-01-01 commodity AMTKPTS

        2015-02-06 *
          Assets:Cash                     1505.00 USD
          Assets:External                -1000.00 GBP @ 1.5050 USD

        2015-02-08 price GBP  1.5050 USD
        2015-02-08 price GBP  1.8301 CAD

        2015-04-13 *
          Assets:US:Investments:QQQ             3 QQQ {107.48 USD}
          Assets:Cash

        2015-05-22 *
          Assets:Cash                     1000.00 USD @ 1.2283 CAD
          Assets:External                -1228.30 CAD

        2015-09-04 *
          Assets:US:Investments:QQQ             2 QQQ {101.16 USD}
          Assets:Cash

        2015-11-07 *
          Assets:CA:Investments:XSP             2 XSP {24.28 CAD}
          Assets:Cash

        2015-12-02 *
          Assets:CA:Investments:XSP            -2 XSP {24.28 CAD} @ 27.00 CAD
          Assets:Cash

        2015-12-03 price XSP   27.32 USD

        ;; Because neither of these currencies remain, they should not be there.
        2015-06-22 *
          Assets:Cash                         1000.00 EUR @ 140.004 JPY
          Expenses:Foreign                    -140004 JPY

        2015-10-13 *
          Assets:Cash                        -1000.00 EUR @ 140.004 JPY
          Expenses:Foreign                     140004 JPY
        """
        self.entries = entries

    def test_find_currencies_converted(self):
        currencies = find_prices.find_currencies_converted(self.entries, None)
        self.assertEqual({('GBP', 'USD'), ('USD', 'CAD'), ('EUR', 'JPY')}, currencies)

        currencies = find_prices.find_currencies_converted(self.entries,
                                                           datetime.date(2015, 5, 1))
        self.assertEqual({('GBP', 'USD')}, currencies)

        currencies = find_prices.find_currencies_converted(self.entries,
                                                           datetime.date(2015, 1, 15))
        self.assertEqual(set(), currencies)

    def test_find_currencies_at_cost(self):
        currencies = find_prices.find_currencies_at_cost(self.entries)
        self.assertEqual({('XSP', 'CAD'), ('QQQ', 'USD')}, currencies)

    def test_find_currencies_priced(self):
        currencies = find_prices.find_currencies_priced(self.entries)
        self.assertEqual({('XSP', 'USD'), ('GBP', 'USD'), ('GBP', 'CAD')}, currencies)

        currencies = find_prices.find_currencies_priced(self.entries,
                                                        datetime.date(2015, 12, 1))
        self.assertEqual({('GBP', 'USD'), ('GBP', 'CAD')}, currencies)

    def test_find_balance_currencies(self):
        currencies = find_prices.find_balance_currencies(self.entries, None)
        self.assertEqual({('QQQ', 'USD'),
                          ('GBP', 'USD'), ('GBP', 'CAD'), ('USD', 'CAD')}, currencies)

        currencies = find_prices.find_balance_currencies(self.entries,
                                                         datetime.date(2015, 12, 1))
        self.assertEqual({('XSP', 'CAD'),
                          ('QQQ', 'USD'), ('GBP', 'USD'), ('GBP', 'CAD'), ('USD', 'CAD')},
                         currencies)

        currencies = find_prices.find_balance_currencies(self.entries,
                                                         datetime.date(2015, 11, 1))
        self.assertEqual({('QQQ', 'USD'),
                          ('GBP', 'USD'), ('GBP', 'CAD'), ('USD', 'CAD')}, currencies)

        currencies = find_prices.find_balance_currencies(self.entries,
                                                         datetime.date(2015, 2, 1))
        self.assertEqual(set(), currencies)


if __name__ == '__main__':
    unittest.main()
