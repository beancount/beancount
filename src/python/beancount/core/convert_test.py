"""
Unit tests for the Position class.
"""
__copyright__ = "Copyright (C) 2014-2015  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest
import copy
import random
from datetime import date

from beancount.core import convert
from beancount.core.position import Cost
from beancount.core.position import Position
from beancount.core.position import from_string
from beancount.core.position import from_amounts

from beancount.core.number import ZERO
from beancount.core.number import MISSING
from beancount.core.number import D
from beancount.core.amount import A
from beancount.core.amount import Amount
from beancount.core import display_context

from beancount.core import data
from beancount.ops import prices


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
        # Fallback on cost.
        self.assertEqual(A("51400.00 USD"),
                         convert.get_value(pos, self.PRICE_MAP_EMPTY))
        # Computed using the db.
        self.assertEqual(A("53000.00 USD"),
                         convert.get_value(pos, self.PRICE_MAP_HIT))


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
        # Fallback on price.
        self.assertEqual(A("52000.00 USD"),
                         convert.get_value(pos, self.PRICE_MAP_EMPTY))
        # Computed using the db.
        self.assertEqual(A("53000.00 USD"),
                         convert.get_value(pos, self.PRICE_MAP_HIT))
