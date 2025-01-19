"""Conversions from Position (or Posting) to units, cost, weight, market value.

  * Units: Just the primary amount of the position.
  * Cost: The cost basis of the position, if available.
  * Weight: The cost basis or price of the position.
  * Market Value: The units converted to a value via a price map.

To convert an inventory's contents, simply use these functions in conjunction
with ``Inventory.reduce()``, like

    cost_inv = inv.reduce(convert.get_cost)

This module equivalently converts Position and Posting instances. Note that
we're specifically avoiding to create an import dependency on
beancount.core.data in order to keep this module isolatable, but it works on
postings due to duck-typing.

Function named ``get_*()`` are used to compute values from postings to their price currency.
Functions named ``convert_*()`` are used to convert postings and amounts to any currency.
"""

__copyright__ = "Copyright (C) 2013-2017, 2019-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

from decimal import Decimal

from beancount.core import prices
from beancount.core.amount import Amount
from beancount.core.number import MISSING
from beancount.core.position import Cost
from beancount.core.position import Position


def get_units(pos):
    """Return the units of a Position or Posting.

    Args:
      pos: An instance of Position or Posting, equivalently.
    Returns:
      An Amount.
    """
    assert isinstance(pos, Position) or type(pos).__name__ == "Posting"
    return pos.units


def get_cost(pos):
    """Return the total cost of a Position or Posting.

    Args:
      pos: An instance of Position or Posting, equivalently.
    Returns:
      An Amount.
    """
    assert isinstance(pos, Position) or type(pos).__name__ == "Posting"
    cost = pos.cost
    return (
        Amount(cost.number * pos.units.number, cost.currency)
        if (isinstance(cost, Cost) and isinstance(cost.number, Decimal))
        else pos.units
    )


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
    assert isinstance(pos, Position) or type(pos).__name__ == "Posting"
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
                if price.number is MISSING or units.number is MISSING:
                    converted_number = MISSING
                else:
                    converted_number = price.number * units.number
                weight = Amount(converted_number, price.currency)

    return weight


def get_value(pos, price_map, date=None, output_date_prices=None):
    """Return the market value of a Position or Posting.

    Note that if the position is not held at cost, this does not convert
    anything, even if a price is available in the 'price_map'. We don't specify
    a target currency here. If you're attempting to make such a conversion, see
    ``convert_*()`` functions below. However, if the object is a posting and it
    has a price, we will use that price to infer the target currency and those
    will be converted.

    Args:
      pos: An instance of Position or Posting, equivalently.
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance to evaluate the value at, or None.
      output_date_prices: An optional output list of (date, price). If this list
        is provided, it will be appended to (mutated) to output the price
        pulled in making the conversions.
    Returns:
      An Amount, either with a successful value currency conversion, or if we
      could not convert the value, just the units, unmodified. This is designed
      so that you could reduce an inventory with this and not lose any
      information silently in case of failure to convert (possibly due to an
      empty price map). Compare the returned currency to that of the input
      position if you need to check for success.

    """
    assert isinstance(pos, Position) or type(pos).__name__ == "Posting"
    units = pos.units
    cost = pos.cost

    # Try to infer what the cost/price currency should be.
    value_currency = (
        (isinstance(cost, Cost) and cost.currency)
        or (hasattr(pos, "price") and pos.price and pos.price.currency)
        or None
    )

    if isinstance(value_currency, str):
        # We have a value currency; hit the price database.
        base_quote = (units.currency, value_currency)
        price_date, price_number = prices.get_price(price_map, base_quote, date)
        if output_date_prices is not None:
            output_date_prices.append((price_date, price_number))
        if price_number is not None:
            return Amount(units.number * price_number, value_currency)

    # We failed to infer a conversion rate; return the units.
    return units


# TODO(blais): This function needs to be modified to return the difference in
# value from the conversion, so that it can be inserted elsewhere (e.g. an
# unrealized gains account).
def convert_position(pos, target_currency, price_map, date=None):
    """Return the market value of a Position or Posting in a particular currency.

    In addition, if the rate from the position's currency to target_currency
    isn't available, an attempt is made to convert from its cost currency, if
    one is available.

    Args:
      pos: An instance of Position or Posting, equivalently.
      target_currency: The target currency to convert to.
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance to evaluate the value at, or None.
    Returns:
      An Amount, either with a successful value currency conversion, or if we
      could not convert the value, just the units, unmodified. (See get_value()
      above for details.)
    """
    cost = pos.cost
    value_currency = (
        (isinstance(cost, Cost) and cost.currency)
        or (hasattr(pos, "price") and pos.price and pos.price.currency)
        or None
    )
    return convert_amount(
        pos.units, target_currency, price_map, date=date, via=(value_currency,)
    )


def convert_amount(amt, target_currency, price_map, date=None, via=None):
    """Return the market value of an Amount in a particular currency.

    In addition, if a conversion rate isn't available, you can provide a list of
    currencies to attempt to synthesize a rate for via implied rates.

    Args:
      amt: An instance of Amount.
      target_currency: The target currency to convert to.
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance to evaluate the value at, or None.
      via: A list of currencies to attempt to synthesize an implied rate if the
        direct conversion fails.
    Returns:
      An Amount, either with a successful value currency conversion, or if we
      could not convert the value, the amount itself, unmodified.

    """
    # First, attempt to convert directly. This should be the most
    # straightforward conversion.
    base_quote = (amt.currency, target_currency)
    _, rate = prices.get_price(price_map, base_quote, date)
    if rate is not None:
        # On success, just make the conversion directly.
        return Amount(amt.number * rate, target_currency)
    elif via:
        assert isinstance(via, (tuple, list))

        # A price is unavailable, attempt to convert via cost/price currency
        # hop, if the value currency isn't the target currency.
        for implied_currency in via:
            if implied_currency == target_currency:
                continue
            base_quote1 = (amt.currency, implied_currency)
            _, rate1 = prices.get_price(price_map, base_quote1, date)
            if rate1 is not None:
                base_quote2 = (implied_currency, target_currency)
                _, rate2 = prices.get_price(price_map, base_quote2, date)
                if rate2 is not None:
                    return Amount(amt.number * rate1 * rate2, target_currency)

    # We failed to infer a conversion rate; return the amt.
    return amt
