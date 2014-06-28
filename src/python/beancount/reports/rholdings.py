"""Generate reports no holdings.
"""
import collections

from beancount.core import amount
from beancount.parser import options
from beancount.ops import prices
from beancount.ops import holdings
from beancount.reports import table


# A field spec that renders all fields.
FIELD_SPEC = [
    ('account', ),
    ('number', "Units", '{:,.2f}'.format),
    ('currency', ),
    ('cost_currency', ),
    ('cost_number', 'Average Cost', '{:,.2f}'.format),
    ('price_number', 'Price', '{:,.2f}'.format),
    ('book_value', 'Book Value', '{:,.2f}'.format),
    ('market_value', 'Market Value', '{:,.2f}'.format),
]


def get_assets_holdings(entries, options_map, currency=None):
    """Return holdings for all assets and liabilities.

    Args:
      entries: A list of directives.
      options_map: A dict of parsed options.
      currency: If specified, a string, the target currency to convert all
        holding values to.
    Returns:
      A list of Holding instances and a price-map.
    """
    # Compute a price map, to perform conversions.
    price_map = prices.build_price_map(entries)

    # Get the list of holdings.
    account_types = options.get_account_types(options_map)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)

    # Convert holdings to a unified currency.
    if currency:
        holdings_list = holdings.convert_to_currency(price_map, currency, holdings_list)

    return holdings_list, price_map


def report_holdings(currency, entries, options_map):
    """Generate a detailed list of all holdings by account.

    Args:
      currency: A string, a currency to convert to. If left to None, no
        conversion is carried out.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    holdings_list, _ = get_assets_holdings(entries, options_map, currency)
    return table.create_table(holdings_list, FIELD_SPEC)


def report_holdings_bycommodity(currency, entries, options_map):
    """Generate a detailed list of all holdings by (base, quote) pair.

    Args:
      currency: A string, a currency to convert to. If left to None, no
        conversion is carried out.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    holdings_list, _ = get_assets_holdings(entries, options_map, currency)
    holdings_list = holdings.aggregate_holdings_by(holdings_list,
                                                   lambda holding: holding.currency)
    return table.create_table(holdings_list, FIELD_SPEC)


def report_holdings_relative(currency, entries, options_map):
    """Generate a list of all holdings aggregated by instrument.

    Args:
      currency: A string, a currency to convert to. If left to None, no
        conversion is carried out.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    holdings_list, _ = get_assets_holdings(entries, options_map, currency)
    holdings_list = holdings.aggregate_holdings_by(holdings_list,
                                                   lambda holding: holding.currency)

    # Reduce the holdings to relative (fractional) values.
    holdings_list = holdings.reduce_relative(holdings_list)

    # Skipping the book value here because by combining it with market value %
    # and price one could theoretically determined the total value of the
    # portfolio.
    field_spec = [
        ('currency', ),
        ('cost_currency', ),
        ('cost_number', 'Average Cost', '{:,.2f}'.format),
        ('price_number', 'Price', '{:,.2f}'.format),
        ('market_value', 'Fraction', '{:,.3%}'.format),
    ]
    return table.create_table(holdings_list, FIELD_SPEC)


def report_holdings_byaccount(currency, entries, options_map):
    """Generate a detailed list of all holdings by account.

    Args:
      currency: A string, a currency to convert to. Must be non-null.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    holdings_list, _ = get_assets_holdings(entries, options_map, currency)
    holdings_list = holdings.aggregate_holdings_by(holdings_list,
                                                   lambda holding: holding.account)
    return table.create_table(holdings_list, FIELD_SPEC)


def report_holdings_bycurrency(currency, entries, options_map):
    """Generate a table of currency exposure.

    Args:
      currency: A string, a currency to convert to. Must be non-null.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance, where each row is a currency and a total amount.
    """
    holdings_list, _ = get_assets_holdings(entries, options_map, currency)
    holdings_list = holdings.aggregate_holdings_by(holdings_list,
                                                   lambda holding: holding.cost_currency)
    return table.create_table(holdings_list, FIELD_SPEC)


def report_networth(entries, options_map):
    """Generate a table of total net worth for each operating currency.

    Args:
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance, where each row is a currency and a total amount.
    """
    holdings_list, price_map = get_assets_holdings(entries, options_map)

    net_worths = []
    for currency in options_map['operating_currency']:

        # Convert holdings to a unified currency.
        currency_holdings_list = holdings.convert_to_currency(price_map, currency, holdings_list)
        if not currency_holdings_list:
            continue

        holdings_list = holdings.aggregate_holdings_by(currency_holdings_list,
                                                       lambda holding: holding.cost_currency)
        assert len(holdings_list) == 1, holdings_list
        net_worths.append((currency, holdings_list[0].market_value))

    field_spec = [
        (0, 'Currency'),
        (1, 'Net Worth', '{:,.2f}'.format),
    ]
    return table.create_table(net_worths, field_spec)
