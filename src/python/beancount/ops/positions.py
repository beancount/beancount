"""Compute final holdings for a list of entries.
"""
from collections import defaultdict

from beancount.core import realization
from beancount.ops import prices

try:
    import pandas
    import numpy
except ImportError:
    pandas = None
    numpy = None


def get_final_holdings(entries):
    """Get a dictionary of the latest holdings by account.

    This basically just flattens the balance sheet's final positions.

    Args:
      entries: A list of directives.
    Returns:
      A list of dicts, with the following fields:
        account: A string, the name of the account.
        number: A Decimal, the number of units for that position.
        currency: A string, the currency for that position.
        cost_number: A Decimal, the price of that currency.
        cost_currency: A string, the currency of the price of that currency.
        book_value: A Decimal, the book value of the holding.
    """
    # Realize the accounts into a tree (because we want the positions by-qaccount).
    real_accounts = realization.realize(entries)

    # For each account, look at the list of positions and build a list.
    holdings = []
    for real_account in real_accounts:
        for position in real_account.balance.get_positions():
            if position.lot.cost:
                holding = {'account': real_account.fullname,
                           'number': position.number,
                           'currency': position.lot.currency,
                           'cost_number': position.lot.cost.number,
                           'cost_currency': position.lot.cost.currency,
                           'book_value': position.number * position.lot.cost.number}
                cost = position.get_cost()
                assert cost.number == holding['number'] * holding['cost_number']
            else:
                holding = {'account': real_account.fullname,
                           'number': position.number,
                           'currency': position.lot.currency,
                           'cost_number': None,
                           'cost_currency': None,
                           'book_value': None}
            positions.append(holding)

    return holdings


def add_prices_to_holdings(holdings, price_map, date=None):
    """Given a list of holding dicts, enrich the dicts with prices.

    This is mainly a convenience as many functions will require this enrichment.

    Args:
      holdings: A list of dicts, as returned by get_final_holdings().
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance, the date at which to price the
        holdings.
    Returns:
      A list of enriched holding dicts, with added attributes:
        price_number: A Decimal, the price/rate of the currency/cost_currency.
        price_date: A datetime.date, the date of the price.
        market_value: A Decimal, the market value of the holding, with the
          price of this holding.
    """
    new_holdings = []
    for holding in holdings:
        holding = holding.copy()
        new_holdings.append(holding)

        # Get the price of this currency/cost pair.
        base_quote = holding['currency'], holding['cost_currency']
        price_date, price_number = prices.get_price(
            price_map, (currency, cost_currency), date)

        holding['price_date'] = price_date
        holding['price_number'] = price_number
        holding['market_value'] = holding['number'] * price_number

    return new_holdings


## FIXME: Use this, add a holding class.
Holding = collections.namedtuple('Holding',
                                 'account number currency cost_number cost_currency '
                                 'book_value market_value price_number price_date')








def get_priced_positions(entries, price_map):
    """Get a list of positions, grouped by (account, currency, cost_currency),
    with the latest prices fetched and dated.

    Args:
      entries: A list of directives.
      price_map: A dict of prices, as built by prices.build_price_map().
    Returns:
      A dict of (account, currency, cost_currency) -> Position dict.
    """

    # Get the full list of positions.
    positions_ = get_final_holdings(entries)

    # Group by account and currencies, and filter those which have an associated
    # cost.
    grouped_positions = defaultdict(list)
    for position in positions_:
        if position['cost_number'] is not None:
            key = (position['account'],
                   position['currency'],
                   position['cost_currency'])
        else:
            key = (position['account'],
                   position['currency'],
                   None)
        grouped_positions[key].append(position)

    # For each group, add the price to the dataframe.
    for (account, currency, cost_currency), position_list in grouped_positions.items():
        if not cost_currency:
            continue

        # Get the latest price.
        price_date, price_number = prices.get_latest_price(price_map, (currency, cost_currency))

        for position in position_list:
            position['price_number'] = price_number
            position['price_date'] = price_date

    # Flatten the grouped positions.
    flat_positions = [position
                      for position_list in grouped_positions.values()
                      for position in position_list]

    return grouped_positions, flat_positions


def get_positions_as_dataframe(entries, price_map):
    """Return a dataframe with a detailed list of positions."""

    if pandas is None:
        return None

    _, flat_positions = get_priced_positions(entries, price_map)


    # TODO(blais): Convert this to avoid the Pandas dependency.

    dataframe = pandas.DataFrame.from_records(
        flat_positions, columns=['account', 'number', 'currency', 'cost_number', 'price_number', 'cost_currency', 'price_date'])

    dataframe['number'] = dataframe['number'].astype(numpy.float)
    dataframe['cost_number'] = dataframe['cost_number'].astype(numpy.float)
    dataframe['price_number'] = dataframe['price_number'].astype(numpy.float)

    dataframe['book_value'] = dataframe['number'] * dataframe['cost_number']
    dataframe['market_value'] = dataframe['number'] * dataframe['price_number']
    dataframe['pnl'] = dataframe['market_value'] - dataframe['book_value']

    return dataframe
