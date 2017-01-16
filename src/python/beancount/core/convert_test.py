"""Unit tests for conversion functions.
"""

__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount.core.number import MISSING
from beancount.core.number import D
from beancount.core.amount import A
from beancount.core.data import create_simple_posting as P
from beancount.core.data import create_simple_posting_with_cost as PCost
from beancount.core.position import Cost
from beancount.core.position import Position
from beancount.core import convert
from beancount.core import inventory
from beancount.core import prices
from beancount.core import data
from beancount import loader


def build_price_map_util(date_currency_price_tuples):
    """Build a partial price-map just for testing.

    Args:
      date_currency_price_tuples: A list of (datetime.date, currency-string,
        price-Amount) tuples to fill in the database with.
    Returns:
      A price_map, as per build_price_map().
    """
    return prices.build_price_map(
        [data.Price(None, date, currency, price)
         for date, currency, price in date_currency_price_tuples])


class TestPositionConversions(unittest.TestCase):
    """Test conversionos to units, cost, weight and market-value for Position objects."""

    def _pos(self, units, cost=None, price=None):
        # Note: 'price' is only used in Posting class which derives from this test.
        self.assertFalse(price)
        return Position(units, cost)

    #
    # Units
    #

    def test_units(self):
        units = A("100 HOOL")
        self.assertEqual(units, convert.get_units(
            self._pos(units,
                      Cost(D("514.00"), "USD", None, None))))
        self.assertEqual(units, convert.get_units(
            self._pos(units, None)))

    #
    # Cost
    #

    def test_cost__empty(self):
        self.assertEqual(A("100 HOOL"), convert.get_cost(
            self._pos(A("100 HOOL"), None)))

    def test_cost__not_empty(self):
        self.assertEqual(A("51400.00 USD"), convert.get_cost(
            self._pos(A("100 HOOL"),
                      Cost(D("514.00"), "USD", None, None))))

    def test_cost__missing(self):
        self.assertEqual(A("100 HOOL"), convert.get_cost(
            self._pos(A("100 HOOL"),
                      Cost(MISSING, "USD", None, None))))

    #
    # Weight
    #

    def test_weight__no_cost(self):
        self.assertEqual(A("100 HOOL"), convert.get_weight(
            self._pos(A("100 HOOL"), None)))

    def test_weight__with_cost(self):
        self.assertEqual(A("51400.00 USD"), convert.get_weight(
            self._pos(A("100 HOOL"),
                      Cost(D("514.00"), "USD", None, None))))

    def test_weight__with_cost_missing(self):
        self.assertEqual(A("100 HOOL"), convert.get_weight(
            self._pos(A("100 HOOL"),
                      Cost(MISSING, "USD", None, None))))

    def test_old_test(self):
        # Entry without cost, without price.
        posting = P(None, "Assets:Bank:Checking", "105.50", "USD")
        self.assertEqual(A("105.50 USD"),
                         convert.get_weight(posting))

        # Entry without cost, with price.
        posting = posting._replace(price=A("0.90 CAD"))
        self.assertEqual(A("94.95 CAD"),
                         convert.get_weight(posting))

        # Entry with cost, without price.
        posting = PCost(None, "Assets:Bank:Checking", "105.50", "USD", "0.80", "EUR")
        self.assertEqual(A("84.40 EUR"),
                         convert.get_weight(posting))

        # Entry with cost, and with price (the price should be ignored).
        posting = posting._replace(price=A("2.00 CAD"))
        self.assertEqual(A("84.40 EUR"),
                         convert.get_weight(posting))


    #
    # Value
    #

    PRICE_MAP_EMPTY = build_price_map_util([])

    PRICE_MAP_HIT = build_price_map_util([
        (datetime.date(2016, 2, 1), "HOOL", A("530.00 USD")),
        (datetime.date(2016, 2, 1), "USD", A("1.2 CAD")),
    ])


    def test_value__no_currency(self):
        pos = self._pos(A("100 HOOL"), None)
        self.assertEqual(A("100 HOOL"),
                         convert.get_value(pos, self.PRICE_MAP_EMPTY))
        self.assertEqual(A("100 HOOL"),
                         convert.get_value(pos, self.PRICE_MAP_HIT))

    def test_value__currency_from_cost(self):
        pos = self._pos(A("100 HOOL"), Cost(D("514.00"), "USD", None, None))
        self.assertEqual(A("53000.00 USD"),
                         convert.get_value(pos, self.PRICE_MAP_HIT))
        self.assertEqual(A("100 HOOL"),
                         convert.get_value(pos, self.PRICE_MAP_EMPTY))


    #
    # Conversion to another currency.
    #

    def test_convert_position__success(self):
        pos = self._pos(A("100 HOOL"), Cost(D("514.00"), "USD", None, None))
        self.assertEqual(A("53000.00 USD"),
                         convert.convert_position(pos, "USD", self.PRICE_MAP_HIT))

    def test_convert_position__miss_but_same_currency(self):
        pos = self._pos(A("100 HOOL"), Cost(D("514.00"), "USD", None, None))
        self.assertEqual(A("100 HOOL"),
                         convert.convert_position(pos, "USD", self.PRICE_MAP_EMPTY))

    def test_convert_position__miss_and_miss_rate_to_rate(self):
        pos = self._pos(A("100 HOOL"), Cost(D("514.00"), "USD", None, None))
        self.assertEqual(A("100 HOOL"),
                         convert.convert_position(pos, "JPY", self.PRICE_MAP_HIT))

    PRICE_MAP_RATEONLY = build_price_map_util([
        (datetime.date(2016, 2, 1), "USD", A("1.2 CAD")),
    ])

    def test_convert_position__miss_and_miss_value_rate(self):
        pos = self._pos(A("100 HOOL"), Cost(D("514.00"), "USD", None, None))
        self.assertEqual(A("100 HOOL"),
                         convert.convert_position(pos, "CAD", self.PRICE_MAP_RATEONLY))

    def test_convert_position__miss_and_miss_both(self):
        pos = self._pos(A("100 HOOL"), Cost(D("514.00"), "USD", None, None))
        self.assertEqual(A("100 HOOL"),
                         convert.convert_position(pos, "CAD", self.PRICE_MAP_EMPTY))

    def test_convert_position__miss_and_success_on_implieds(self):
        pos = self._pos(A("100 HOOL"), Cost(D("514.00"), "USD", None, None))
        self.assertEqual(A("63600.00 CAD"),
                         convert.convert_position(pos, "CAD", self.PRICE_MAP_HIT))

    #
    # Conversion of amounts to another currency.
    #

    def test_convert_amount__fail(self):
        amt = A("127.00 USD")
        self.assertEqual(amt,
                         convert.convert_amount(amt, "CAD", self.PRICE_MAP_EMPTY))

    def test_convert_amount__success(self):
        amt = A("127.00 USD")
        self.assertEqual(A("152.40 CAD"),
                         convert.convert_amount(amt, "CAD", self.PRICE_MAP_HIT))

    def test_convert_amount__noop(self):
        amt = A("127.00 USD")
        self.assertEqual(amt,
                         convert.convert_amount(amt, "USD", self.PRICE_MAP_EMPTY))
        self.assertEqual(amt,
                         convert.convert_amount(amt, "USD", self.PRICE_MAP_HIT))

    @loader.load_doc()
    def test_convert_amount_with_date(self, entries, _, __):
        """
        2013-01-01 price  USD  1.20 CAD
        2014-01-01 price  USD  1.25 CAD
        2015-01-01 price  USD  1.30 CAD
        """
        price_map = prices.build_price_map(entries)
        for date, exp_amount in [
                (None, A('130 CAD')),
                (datetime.date(2015, 1, 1), A('130 CAD')),
                (datetime.date(2014, 12, 31), A('125 CAD')),
                (datetime.date(2014, 1, 1), A('125 CAD')),
                (datetime.date(2013, 12, 31), A('120 CAD')),
                (datetime.date(2013, 1, 1), A('120 CAD')),
                (datetime.date(2012, 12, 31), A('100 USD')),
                ]:
            self.assertEqual(exp_amount,
                             convert.convert_amount(A('100 USD'), 'CAD', price_map, date))


class TestPostingConversions(TestPositionConversions):
    """Test conversions to units, cost, weight and market-value for Posting objects."""

    def _pos(self, units, cost=None, price=None):
        # Create a Posting instance instead of a Position.
        return data.Posting("Assets:AccountA", units, cost, price, None, None)

    def test_weight_with_cost_and_price(self):
        self.assertEqual(A("51400.00 USD"), convert.get_weight(
            self._pos(A("100 HOOL"),
                      Cost(D("514.00"), "USD", A("530.00 USD"), None))))

    def test_weight_with_only_price(self):
        self.assertEqual(A("53000.00 USD"), convert.get_weight(
            self._pos(A("100 HOOL"), None, A("530.00 USD"))))

    def test_value__currency_from_price(self):
        pos = self._pos(A("100 HOOL"), None, A("520.00 USD"))
        self.assertEqual(A("53000.00 USD"),
                         convert.get_value(pos, self.PRICE_MAP_HIT))
        self.assertEqual(A("100 HOOL"),
                         convert.get_value(pos, self.PRICE_MAP_EMPTY))

    def test_convert_position__currency_from_price(self):
        pos = self._pos(A("100 HOOL"), None, A("99999 USD"))
        self.assertEqual(A("63600.00 CAD"),
                         convert.convert_position(pos, "CAD", self.PRICE_MAP_HIT))


class TestMarketValue(unittest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, _, __):
        """
        2013-06-01 price  USD  1.01 CAD
        2013-06-05 price  USD  1.05 CAD
        2013-06-06 price  USD  1.06 CAD
        2013-06-07 price  USD  1.07 CAD
        2013-06-10 price  USD  1.10 CAD

        2013-06-01 price  HOOL  101.00 USD
        2013-06-05 price  HOOL  105.00 USD
        2013-06-06 price  HOOL  106.00 USD
        2013-06-07 price  HOOL  107.00 USD
        2013-06-10 price  HOOL  110.00 USD

        2013-06-01 price  AAPL  91.00 USD
        2013-06-05 price  AAPL  95.00 USD
        2013-06-06 price  AAPL  96.00 USD
        2013-06-07 price  AAPL  97.00 USD
        2013-06-10 price  AAPL  90.00 USD
        """
        self.price_map = prices.build_price_map(entries)

    def test_no_change(self):
        balances = inventory.from_string('100 USD')
        market_value = balances.reduce(convert.get_value, self.price_map,
                                       datetime.date(2013, 6, 6))
        self.assertEqual(inventory.from_string('100 USD'), market_value)

    def test_other_currency(self):
        balances = inventory.from_string('100 CAD')
        market_value = balances.reduce(convert.get_value, self.price_map,
                                       datetime.date(2013, 6, 6))
        self.assertEqual(inventory.from_string('100 CAD'), market_value)

    def test_mixed_currencies(self):
        balances = inventory.from_string('100 USD, 90 CAD')
        market_value = balances.reduce(convert.get_value, self.price_map,
                                       datetime.date(2013, 6, 6))
        self.assertEqual(inventory.from_string('100 USD, 90 CAD'), market_value)

    def test_stock_single(self):
        balances = inventory.from_string('5 HOOL {0.01 USD}')
        market_value = balances.reduce(convert.get_value, self.price_map,
                                       datetime.date(2013, 6, 6))
        self.assertEqual(inventory.from_string('530 USD'), market_value)

    def test_stock_many_lots(self):
        balances = inventory.from_string('2 HOOL {0.01 USD}, 3 HOOL {0.02 USD}')
        market_value = balances.reduce(convert.get_value, self.price_map,
                                       datetime.date(2013, 6, 6))
        self.assertEqual(inventory.from_string('530 USD'), market_value)

    def test_stock_different_ones(self):
        balances = inventory.from_string('2 HOOL {0.01 USD}, 2 AAPL {0.02 USD}')
        market_value = balances.reduce(convert.get_value, self.price_map,
                                       datetime.date(2013, 6, 6))
        self.assertEqual(inventory.from_string('404 USD'), market_value)

    def test_stock_not_found(self):
        balances = inventory.from_string('2 MSFT {0.01 USD}')
        market_value = balances.reduce(convert.get_value, self.price_map,
                                       datetime.date(2013, 6, 6))
        self.assertEqual(inventory.from_string('2 MSFT'), market_value)
