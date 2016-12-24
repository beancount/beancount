"""Conversions from Positing or Posting to units, cost, weight, market value.
"""
__copyright__ = "Copyright (C) 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"

import copy
import datetime
import collections
import re

from beancount.core.number import Decimal
from beancount.core.number import D
from beancount.core.amount import Amount
from beancount.core.position import Cost
from beancount.core.position import Position

from beancount.ops import prices


def get_units(pos):
    """Return the units of a Position or Posting.

    Args:
      pos: An instance of Position or Posting, equivalently.
    Returns:
      An Amount.
    """
    return pos.units


def get_cost(pos):
    """Return the total cost of a Position or Posting.

    Args:
      pos: An instance of Position or Posting, equivalently.
    Returns:
      An Amount.
    """
    cost = pos.cost
    return (Amount(cost.number * pos.units.number, cost.currency)
            if (isinstance(cost, Cost) and isinstance(cost.number, Decimal))
            else pos.units)


def get_weight(pos):
    """Return the market value of a Position or Posting.

    Args:
      pos: An instance of Position or Posting, equivalently.
    Returns:
      An Amount.
    """
    units = pos.units
    cost = pos.cost

    # It the object has a cost, use that as the weight, to balance.
    if isinstance(cost, Cost) and isinstance(cost.number, Decimal):
        weight = Amount(cost.number * pos.units.number, cost.currency)
    else:
        # Otherwise use the postings.
        weight = units

        # Unless there is a price available; use that if present.
        if not isinstance(pos, Position):
            price = pos.price
            if price is not None:
                weight = Amount(price.number * units.number, price.currency)

    return weight


def get_value(pos, price_map, date=None):
    """Return the market value of a Position or Posting.

    Note that if the position is not held at cost, this does not convert
    anything, even if a price is available in the 'price_map'. We don't have a
    target currency.

    Args:
      pos: An instance of Position or Posting, equivalently.
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance to evaluate the value at, or None.
    Returns:
      An Amount.
    """
    units = pos.units
    cost = pos.cost

    # Try to infer what the cost currency is.
    value_currency = None
    price_fallback = None

    if isinstance(cost, Cost):
        value_currency = cost.currency
        price_fallback = cost.number
        assert value_currency is not None

    if not isinstance(pos, Position):
        price = pos.price
        if price is not None:
            value_currency = price.currency
            price_fallback = price.number
            assert value_currency is not None

    if value_currency is None:
        # We failed to infer a currency; return the units.
        value = units
    else:
        # We have a currency; try to hit the price database.
        base_quote = (units.currency, value_currency)
        _, price_number = prices.get_price(price_map, base_quote, date)
        if price_number is not None:
            value = Amount(units.number * price_number, value_currency)
        elif price_fallback is not None:
            value = Amount(units.number * price_fallback, value_currency)

    return value


# TODO: beancount.ops.prices may need to migrate to core because of the
# dependency. No problem; do this.
