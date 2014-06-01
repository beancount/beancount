"""Compute final holdings for a list of entries.
"""
import collections
import functools

from beancount.core.amount import ZERO
from beancount.core import realization
from beancount.core import account_types
from beancount.core import data
from beancount.core import flags
from beancount.core import position
from beancount.ops import prices
from beancount.utils import misc_utils


# A holding, a flattened position with an account, and optionally, price and
# book/market values.
#
# account: A string, the name of the account.
# number: A Decimal, the number of units for that position.
# currency: A string, the currency for that position.
# cost_number: A Decimal, the price of that currency.
# cost_currency: A string, the currency of the price of that currency.
# book_value: A Decimal, the book value of the holding.
# price_number: A Decimal, the price/rate of the currency/cost_currency.
# price_date: A datetime.date, the date of the price.
# market_value: A Decimal, the market value of the holding, with the
#   price of this holding.
#
# Note: we could reserve an 'extra' member to hold values from derived fields,
# such as fractional value of portfolio, instead of occasionally overloading the
# value of market_value or others.
Holding = collections.namedtuple('Holding',
                                 'account number currency cost_number cost_currency '
                                 'book_value market_value price_number price_date')


def get_final_holdings(entries, included_account_types=None, price_map=None, date=None):
    """Get a dictionary of the latest holdings by account.

    This basically just flattens the balance sheet's final positions, including
    that of equity accounts. If a 'price_map' is provided, insert price
    information in the flattened holdings at the latest date, or at the given
    date, if one is provided.

    Only the accounts in 'included_account_types' will be included, and this is
    always called for Assets and Liabilities only. If left unspecified, holdings
    from all account types will be included, including Equity, Income and
    Expenses.

    Args:
      entries: A list of directives.
      included_account_types: A sequence of strings, the account types to
        include in the output. A reasonable example would be
        ('Assets', 'Liabilities').
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance, the date at which to price the
        holdings. If left unspecified, we use the latest price information.
    Returns:
      A list of dicts, with the following fields:

    """
    # Remove the entries inserted by unrealized gains/losses. Those entries do
    # affect asset accounts, and we don't want them to appear in holdings.
    simple_entries = [entry
                      for entry in entries
                      if (not isinstance(entry, data.Transaction) or
                          entry.flag != flags.FLAG_UNREALIZED)]

    # Realize the accounts into a tree (because we want the positions by-account).
    real_accounts = realization.realize(simple_entries)

    # For each account, look at the list of positions and build a list.
    holdings = []
    for real_account in sorted(list(real_accounts), key=lambda x: x.fullname):

        if included_account_types:
            # Skip accounts of invalid types, we only want to reflect the requested
            # account types, typically assets and liabilities.
            account_type = account_types.get_account_type(real_account.fullname)
            if account_type not in included_account_types:
                continue

        for position in real_account.balance.get_positions():
            if position.lot.cost:
                # Get price information if we have a price_map.
                market_value = None
                if price_map is not None:
                    base_quote = (position.lot.currency, position.lot.cost.currency)
                    price_date, price_number = prices.get_price(price_map,
                                                                base_quote, date)
                    if price_number is not None:
                        market_value = position.number * price_number
                else:
                    price_date, price_number = None, None

                holding = Holding(real_account.fullname,
                                  position.number,
                                  position.lot.currency,
                                  position.lot.cost.number,
                                  position.lot.cost.currency,
                                  position.number * position.lot.cost.number,
                                  market_value,
                                  price_number,
                                  price_date)
            else:
                holding = Holding(real_account.fullname,
                                  position.number,
                                  position.lot.currency,
                                  None, None, None, None, None, None)
            holdings.append(holding)

    return holdings


def aggregate_by_base_quote(holdings):
    """Group and aggregate a list of holdings by (base, quote) pair.

    This groups the holdings and applies aggregation to each set of matching
    rows. The 'account' fields should all be None.

    Args:
      holdings: A list of Holding instances.
    Returns:
      An aggregated list of Holding instances.
    """
    grouped = collections.defaultdict(list)
    for holding in holdings:
        key = (holding.currency, holding.cost_currency)
        grouped[key].append(holding)
    return sorted(aggregate_holdings_list(key_holdings)
                  for key_holdings in grouped.values())


def aggregate_holdings_list(holdings):
    """Aggregate a list of holdings.

    Args:
      holdings: A list of Holding instances.
    Returns:
      A single Holding instance, or None, if there are no holdings in the input
      list.
    """
    if not holdings:
        return None

    # Note: Holding is a bit overspecified with book and market values. We
    # recompute them from cost and price numbers here anyhow.
    units, total_book_value, total_market_value = ZERO, ZERO, ZERO
    for holding in holdings:
        units += holding.number

        if holding.cost_number:
            total_book_value += holding.number * holding.cost_number

        if holding.price_number:
            total_market_value += holding.number * holding.price_number

    if not total_book_value:
        total_book_value = None
    average_cost = (total_book_value / units
                    if total_book_value
                    else None)

    if not total_market_value:
        total_market_value = None
    average_price = (total_market_value / units
                     if total_market_value
                     else None)

    first = holdings[0]
    return Holding(None, units, first.currency, average_cost, first.cost_currency,
                   total_book_value, total_market_value, average_price, None)


def convert_to_currency(price_map, target_currency, holdings_list):
    """Convert the given list of holdings's fields to a common currency.

    If the rate is not available to convert, leave the fields empty.

    Args:
      price_map: A price-map, as built by prices.build_price_map().
      target_currency: The target common currency to convert amounts to.
      holdings_list: A list of holdings.Holding instances.
    Returns:
      A modified list of holdings, with the 'extra' field set to the value in
      'currency', or None, if it was not possible to convert.
    """
    # A list of the fields we should convert.
    convert_fields = ('cost_number', 'book_value', 'market_value', 'price_number')

    price_converter = functools.partial(prices.convert_amount, price_map, target_currency)
    new_holdings = []
    for holding in holdings_list:
        if holding.cost_currency == target_currency:
            # The holding is already priced in the target currency; do nothing.
            new_holding = holding
        else:
            if holding.cost_currency is not None:
                # There is a valid cost currency; attempt to convert all the fields.
                base_quote = (holding.cost_currency, target_currency)
            else:
                # There is no cost currency; attempt to convert. Note that this may
                # be a degenerate case of the currency itself being the target
                # currency, in which case the price-map should yield a rate of 1.0
                # and everything else works out.
                assert holding.cost_currency is None
                if holding.currency is None:
                    raise ValueError("Invalid currency '{}'".format(holding.currency))
                base_quote = (holding.currency, target_currency)

                # Fill in with the units if the cost currency is not set.
                if holding.book_value is None:
                    holding = holding._replace(book_value=holding.number)
                if holding.market_value is None:
                    holding = holding._replace(market_value=holding.number)
            try:
                # Get the conversion rate and replace the required numerical
                # fields..
                _, rate = prices.get_latest_price(price_map, base_quote)
                new_holding = misc_utils.map_namedtuple_attributes(
                    convert_fields,
                    lambda number: number if number is None else number * rate,
                    holding)
                # Ensure we set the new cost currency after conversion.
                new_holding = new_holding._replace(cost_currency=target_currency)
            except KeyError:
                # Could not get the rate... clear every field and set the cost
                # currency to None. This enough marks the holding conversion as
                # a failure.
                new_holding = misc_utils.map_namedtuple_attributes(
                    convert_fields, lambda number: None, holding)
                new_holding = new_holding._replace(cost_currency=None)

        new_holdings.append(new_holding)

    return new_holdings


def reduce_relative(holdings):
    """Convert the market and book values of the given list of holdings to relative data.

    Args:
      holdings: A list of Holding instances.
    Returns:
      A list of holdings instances with the absolute value fields replaced by
      fractions of total portfolio. The new list of holdings is sorted by
      currency, and the relative fractions are also relative to that currency.
    """
    # Group holdings by value currency.
    by_currency = collections.defaultdict(list)
    ordering = {}
    for index, holding in enumerate(holdings):
        ordering.setdefault(holding.cost_currency, index)
        by_currency[holding.cost_currency].append(holding)

    fractional_holdings = []
    for currency in sorted(by_currency, key=ordering.get):
        currency_holdings = by_currency[currency]

        # Compute total market value for that currency.
        total_book_value = ZERO
        total_market_value = ZERO
        for holding in currency_holdings:
            if holding.book_value:
                total_book_value += holding.book_value
            if holding.market_value:
                total_market_value += holding.market_value

        # Sort the currency's holdings with decreasing values of market value.
        currency_holdings.sort(
            key=lambda holding: holding.market_value or ZERO,
            reverse=True)

        # Output new holdings with the relevant values replaced.
        for holding in currency_holdings:
            fractional_holdings.append(
                holding._replace(book_value=(holding.book_value / total_book_value
                                             if holding.book_value is not None
                                             else None),
                                 market_value=(holding.market_value / total_market_value
                                               if holding.market_value is not None
                                               else None)))
    return fractional_holdings
