"""Compute positions for a list of entries.
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


def get_latest_positions(entries):
    """Get a dictionary of the latest positions by account.

    """

    # Realize the accounts into a tree (because we want the positions by-qaccount).
    real_accounts = realization.realize(entries)

    # For each account, look at the list of positions and build a list.
    positions = []
    for real_account in real_accounts:
        for position in real_account.balance.get_positions():
            if position.lot.cost or position.lot.lot_date:
                posdict = {'account': real_account.fullname,
                           'number': position.number,
                           'currency': position.lot.currency,
                           'cost_number': position.lot.cost.number,
                           'cost_currency': position.lot.cost.currency}
                cost = position.get_cost()
                assert cost.number == posdict['number'] * posdict['cost_number']
            else:
                posdict = {'account': real_account.fullname,
                           'number': position.number,
                           'currency': position.lot.currency,
                           'cost_number': None,
                           'cost_currency': None}
            positions.append(posdict)

    return positions


def get_priced_positions(entries, price_map):
    """Get a list of positions, groups by (account, currency, cost_currency),
    with the latest prices fetched and dated.

    Args:
      entries: A list of directives.
      price_map: A price map dict, as built by build_price_map.
    Returns:
      A dict of (account, currency, cost_currency) -> Position dict.
    """

    # Get the full list of positions.
    positions_ = get_latest_positions(entries)

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
