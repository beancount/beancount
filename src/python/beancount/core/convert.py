"""Conversions from Position (or Posting) to units, cost, weight, market value.

  Units: Just the primary amount of the position.
  Cost: The cost basis of the position, if available.
  Weight: The cost basis or price of the position.
  Market Value: The units converted to a value via a price map.

This module converts equivalently Position and Posting instances.
(Note that we're specifically avoiding to create an import dependency on
beancount.core.data in order to keep this module isolatable.)

Functions named convert_* are used to convert to any currency.
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
from beancount.core import prices


def get_units(pos):
    """Return the units of a Position or Posting.

    Args:
      pos: An instance of Position or Posting, equivalently.
    Returns:
      An Amount.
    """
    assert isinstance(pos, Position) or type(pos).__name__ == 'Posting'
    return pos.units


def get_cost(pos):
    """Return the total cost of a Position or Posting.

    Args:
      pos: An instance of Position or Posting, equivalently.
    Returns:
      An Amount.
    """
    assert isinstance(pos, Position) or type(pos).__name__ == 'Posting'
    cost = pos.cost
    return (Amount(cost.number * pos.units.number, cost.currency)
            if (isinstance(cost, Cost) and isinstance(cost.number, Decimal))
            else pos.units)


def get_weight(pos):
    """Return the weight of a Position or Posting.

    This is the amount that will need to be balanced from a posting of a
    transaction.

    This is a *key* element of the semantics of transactions in this software. A
    balance amount is the amount used to check the balance of a transaction.
    Here are all relevant examples, with the amounts used to balance the
    postings:

      Assets:Account  5234.50 USD                             ->  5234.50 USD
      Assets:Account  3877.41 EUR @ 1.35 USD                  ->  5234.50 USD
      Assets:Account       10 HOOL {523.45 USD}               ->  5234.50 USD
      Assets:Account       10 HOOL {523.45 USD} @ 545.60 CAD  ->  5234.50 USD

    Args:
      pos: An instance of Position or Posting, equivalently.
    Returns:
      An Amount.
    """
    assert isinstance(pos, Position) or type(pos).__name__ == 'Posting'
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
                # Note: Here we could assert that price.currency == units.currency.
                weight = Amount(price.number * units.number, price.currency)

    return weight


def get_value(pos, price_map, date=None):
    """Return the market value of a Position or Posting.

    Note that if the position is not held at cost, this does not convert
    anything, even if a price is available in the 'price_map'. We don't have a
    target currency. If you're attempting to make such a conversion, see
    get_value_in_currency().

    Args:
      pos: An instance of Position or Posting, equivalently.
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance to evaluate the value at, or None.
    Returns:
      An Amount, either with a succesful value currency conversion, or if we
      could not convert the value, just the amount itself, with no modification.
      This is designed so that you could reduce an inventory with this and not
      lose any information silently in case of failure to convert (possibly due
      to an empty price map). Compare the returned currency to that of the input
      position if you need to check for success.
    """
    assert isinstance(pos, Position) or type(pos).__name__ == 'Posting'
    units = pos.units
    cost = pos.cost

    # Try to infer what the valuation currency should be.
    value_currency = None
    price_fallback = None

    if isinstance(cost, Cost):
        # Currency is provided by the cost.
        value_currency = cost.currency
        price_fallback = cost.number
        assert value_currency is not None

    if not isinstance(pos, Position):
        price = pos.price
        if price is not None:
            # Currency is provided by the price.
            value_currency = price.currency
            price_fallback = price.number
            assert value_currency is not None

    if value_currency is None:
        # We failed to infer a currency; return the units.
        value = units
    else:
        # Note: this is the same as convert_amount(); refactor.

        # We have a currency; hit the price database.
        base_quote = (units.currency, value_currency)
        _, price_number = prices.get_price(price_map, base_quote, date)
        if price_number is not None:
            value = Amount(units.number * price_number, value_currency)
        elif price_fallback is not None:
            value = Amount(units.number * price_fallback, value_currency)
        else:
            value = units

    return value


# FIXME: Remove the price fallback, it doesn't really make sense to do this, the
# price database ought to be the single source of rates for conversions when
# this is requested.


# def convert_position(pos, target_currency, price_map, date=None):
#     """Return the market value of a Position or Posting in a particular currency.
#
#     In addition, if the rate from units.currency to target_currency isn't
#     available, an attempt is made to convert from cost.currency to
#     target.currency, if available.
#
#     Args:
#       pos: An instance of Position or Posting, equivalently.
#       target_currency: The target currency to convert to.
#       price_map: A dict of prices, as built by prices.build_price_map().
#       date: A datetime.date instance to evaluate the value at, or None.
#     Returns:
#       An Amount, either with a succesful value currency conversion, or if we
#       could not convert the value, just the amount itself, with no modification.
#       (See get_value() above for details.)
#     """
#     units = pos.units
#
#     # First, attempt to convert directly. This should be the most
#     # straightforward conversion.
#     base_quote = (units.currency, target_currency)
#     _, price_number = prices.get_price(price_map, base_quote, date)
#     if price_number is not None:
#         return Amount(units.number * price_number, target_currency)
#     else:
#         # If a price is unavailable, attempt to convert to its cost or price
#         # currency.
#         value = get_value(pos, price_map, date)
#         if value.currency == units.currency:
#             # If this conversion failed, just return the units.
#             return units
#         else:
#             # We managed to convert to the cost/price currency; now this cannot
#             # be the target currency, otherwise the above attempt would have
#             # succeeded. Assert this.
#
#
#
#
# ### we shouldn't fall back just yet to the static price here.
#
#             if value.currency != target_currency, (value.currency, target_currency)
#
#             # So now we'll attempt to convert the value currency to the target
#             # currency.
#             base_quote = (value.currency, target_currency)
#             _, price_number = prices.get_price(price_map, base_quote, date)
#             if price_number is not None:
#                 return Amount(value.number * price_number, target_currency)
#
#     # Give up; don't return None, return units that can still be added to an
#     # inventory. This is our failure signal.
#     return units


# def convert_amount(amt, target_currency, price_map, date=None):
