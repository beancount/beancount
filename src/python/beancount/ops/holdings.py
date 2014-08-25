"""Compute final holdings for a list of entries.
"""
import collections
import functools

from beancount.core.amount import ZERO
from beancount.core import account
from beancount.core import amount
from beancount.core import position
from beancount.core import realization
from beancount.core import account_types
from beancount.core import data
from beancount.core import flags
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
    root_account = realization.realize(simple_entries)

    # For each account, look at the list of positions and build a list.
    holdings = []
    for real_account in sorted(list(realization.iter_children(root_account)),
                               key=lambda ra: ra.account):

        if included_account_types:
            # Skip accounts of invalid types, we only want to reflect the requested
            # account types, typically assets and liabilities.
            account_type = account_types.get_account_type(real_account.account)
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

                holding = Holding(real_account.account,
                                  position.number,
                                  position.lot.currency,
                                  position.lot.cost.number,
                                  position.lot.cost.currency,
                                  position.number * position.lot.cost.number,
                                  market_value,
                                  price_number,
                                  price_date)
            else:
                holding = Holding(real_account.account,
                                  position.number,
                                  position.lot.currency,
                                  None,
                                  position.lot.currency,
                                  position.number,
                                  position.number,
                                  None,
                                  None)
            holdings.append(holding)

    return holdings


def aggregate_holdings_by(holdings, keyfun):
    """Aggregate holdings by some key.

    Note that the cost-currency must always be included in the group-key (sums
    over multiple currency units do not make sense), so it is appended to the
    sort-key automatically.

    Args:
      keyfun: A callable, which returns the key to aggregate by. This key need
        not include the cost-currency.
    Returns:
      A list of aggregated holdings.
    """
    grouped = collections.defaultdict(list)
    for holding in holdings:
        key = (keyfun(holding), holding.cost_currency)
        grouped[key].append(holding)
    return sorted(aggregate_holdings_list(key_holdings)
                  for key_holdings in grouped.values())


def aggregate_holdings_list(holdings):
    """Aggregate a list of holdings.

    If there are varying 'account', 'currency' or 'cost_currency' attributes,
    their values are replaced by '*'. Otherwise they are preserved. Note that
    all the cost-currency values must be equal in order for aggregations to
    succeed (without this constraint a sum of units in different currencies has
    no meaning).

    Args:
      holdings: A list of Holding instances.
    Returns:
      A single Holding instance, or None, if there are no holdings in the input
      list.
    Raises:
      ValueError: If multiple cost currencies encountered.
    """
    if not holdings:
        return None

    # Note: Holding is a bit overspecified with book and market values. We
    # recompute them from cost and price numbers here anyhow.
    units, total_book_value, total_market_value = ZERO, ZERO, ZERO
    accounts = set()
    currencies = set()
    cost_currencies = set()
    price_dates = set()
    for holding in holdings:
        units += holding.number
        accounts.add(holding.account)
        price_dates.add(holding.price_date)
        currencies.add(holding.currency)
        cost_currencies.add(holding.cost_currency)

        if holding.book_value:
            total_book_value += holding.book_value
        elif holding.cost_number:
            total_book_value += holding.number * holding.cost_number

        if holding.market_value:
            total_market_value += holding.market_value
        elif holding.price_number:
            total_market_value += holding.number * holding.price_number

    if not total_book_value:
        total_book_value = None
    average_cost = (total_book_value / units
                    if total_book_value and units
                    else None)

    if not total_market_value:
        total_market_value = None
    average_price = (total_market_value / units
                     if total_market_value and units
                     else None)

    if len(cost_currencies) != 1:
        raise ValueError("Cost currencies are not homogeneous for aggregation: {}".format(
            ','.join(map(str, cost_currencies))))

    units = units if len(currencies) == 1 else ZERO
    currency = currencies.pop() if len(currencies) == 1 else '*'
    cost_currency = cost_currencies.pop()
    account_ = (accounts.pop()
                if len(accounts) == 1
                else account.commonprefix(accounts))
    price_date = price_dates.pop() if len(price_dates) == 1 else None
    return Holding(account_, units, currency, average_cost, cost_currency,
                   total_book_value, total_market_value, average_price, price_date)


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
            if holding.cost_currency is None:
                # There is no cost currency; make the holding priced in its own
                # units. The price-map should yield a rate of 1.0 and everything
                # else works out.
                if holding.currency is None:
                    raise ValueError("Invalid currency '{}'".format(holding.currency))
                holding = holding._replace(cost_currency=holding.currency)

                # Fill in with book and market value as well.
                if holding.book_value is None:
                    holding = holding._replace(book_value=holding.number)
                if holding.market_value is None:
                    holding = holding._replace(market_value=holding.number)

            assert holding.cost_currency, "Missing cost currency: {}".format(holding)
            base_quote = (holding.cost_currency, target_currency)

            # Get the conversion rate and replace the required numerical
            # fields..
            _, rate = prices.get_latest_price(price_map, base_quote)
            if rate is not None:
                new_holding = misc_utils.map_namedtuple_attributes(
                    convert_fields,
                    lambda number: number if number is None else number * rate,
                    holding)
                # Ensure we set the new cost currency after conversion.
                new_holding = new_holding._replace(cost_currency=target_currency)
            else:
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


def scale_holding(holding, scale_factor):
    """Scale the values of a holding.

    Args:
      holding: An instance of Holding.
      scale_factor: A float or Decimal number.
    Returns:
      A scaled copy of the holding.
    """
    return Holding(
        holding.account,
        holding.number * scale_factor if holding.number else None,
        holding.currency,
        holding.cost_number,
        holding.cost_currency,
        holding.book_value * scale_factor if holding.book_value else None,
        holding.market_value * scale_factor if holding.market_value else None,
        holding.price_number,
        holding.price_date)


def holding_to_position(holding):
    """Convert the holding to a position.

    Args:
      holding: An instance of Holding.
    Returns:
      An instance of Position.
    """
    cost = (amount.Amount(holding.cost_number, holding.cost_currency)
            if holding.cost_number
            else None)
    return position.Position(position.Lot(holding.currency, cost, None),
                             holding.number)
