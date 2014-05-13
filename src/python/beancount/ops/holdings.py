"""Compute final holdings for a list of entries.
"""
import copy
import collections

from beancount.core import realization
from beancount.ops import prices


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
Holding = collections.namedtuple('Holding',
                                 'account number currency cost_number cost_currency '
                                 'book_value market_value price_number price_date')


def get_final_holdings(entries, price_map=None, date=None):
    """Get a dictionary of the latest holdings by account.

    This basically just flattens the balance sheet's final positions. If a
    'price_map' is provided, insert price information in the flattened holdings
    at the latest date, or at the given date, if one is provided.

    Args:
      entries: A list of directives.
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance, the date at which to price the
        holdings. If left unspecified, we use the latest price information.
    Returns:
      A list of dicts, with the following fields:

    """
    # Realize the accounts into a tree (because we want the positions by-account).
    real_accounts = realization.realize(entries)

    # For each account, look at the list of positions and build a list.
    holdings = []
    for real_account in sorted(list(real_accounts), key=lambda x: x.fullname):
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
