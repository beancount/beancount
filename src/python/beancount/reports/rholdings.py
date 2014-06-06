"""Generate reports no holdings.
"""
import collections

from beancount.core import amount
from beancount.parser import options
from beancount.ops import prices
from beancount.ops import holdings
from beancount.reports import table


def report_holdings(entries, options_map):
    """Generate a detailed list of all holdings by account.

    Args:
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    price_map = prices.build_price_map(entries)
    account_types = options.get_account_types(options_map)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)
    field_spec = [
        ('account', ),
        ('number', "Units", '{:,.2f}'.format),
        ('currency', ),
        ('cost_currency', ),
        ('cost_number', 'Average Cost', '{:,.2f}'.format),
        ('price_number', 'Price', '{:,.2f}'.format),
        ('book_value', 'Book Value', '{:,.2f}'.format),
        ('market_value', 'Market Value', '{:,.2f}'.format),
    ]
    return table.create_table(holdings_list, field_spec)


def report_holdings_aggregated(currency, entries, options_map):
    """Generate a detailed list of all holdings by account.

    Args:
      currency: A string, a currency to convert to. If left to None, no
        conversion is carried out.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    price_map = prices.build_price_map(entries)
    account_types = options.get_account_types(options_map)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)

    # Aggregate the holdings.
    holdings_list = holdings.aggregate_by_base_quote(holdings_list)

    # Convert holdings to a unified currency.
    if currency:
        holdings_list = holdings.convert_to_currency(price_map, currency, holdings_list)

    field_spec = [
        ('number', "Units", '{:,.2f}'.format),
        ('currency', ),
        ('cost_currency', ),
        ('cost_number', 'Average Cost', '{:,.2f}'.format),
        ('price_number', 'Price', '{:,.2f}'.format),
        ('book_value', 'Book Value', '{:,.2f}'.format),
        ('market_value', 'Market Value', '{:,.2f}'.format),
    ]
    return table.create_table(holdings_list, field_spec)


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
    price_map = prices.build_price_map(entries)
    account_types = options.get_account_types(options_map)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)

    # Aggregate the holdings.
    holdings_list = holdings.aggregate_by_base_quote(holdings_list)

    # Convert holdings to a unified currency.
    if currency:
        holdings_list = holdings.convert_to_currency(price_map, currency, holdings_list)

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
    return table.create_table(holdings_list, field_spec)


def report_currency_exposure(entries, options_map):
    """Generate a table of currency exposure.

    Args:
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance, where each row is a currency and a total amount.
    """
    price_map = prices.build_price_map(entries)
    account_types = options.get_account_types(options_map)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)

    # Aggregate the holdings.
    holdings_list = holdings.aggregate_by_base_quote(holdings_list)

    # Aggregate by cost_currency.
    currency_totals = collections.defaultdict(amount.Decimal)
    for holding in holdings_list:
        if holding.market_value:
            currency_totals[holding.cost_currency] += holding.market_value
        else:
            currency_totals[holding.currency] += holding.number

    field_spec = [
        (0, 'Currency'),
        (1, 'Exposure', '{:,.2f}'.format),
    ]
    return table.create_table(sorted(currency_totals.items(),
                                     key=lambda x: x[1],
                                     reverse=True), field_spec)


def report_networth(entries, options_map):
    """Generate a table of net-worth, for each operating currency.

    Args:
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance, where each row is a currency and a total amount.
    """
    net_worths = []
    for currency in options_map['operating_currency']:
        price_map = prices.build_price_map(entries)
        account_types = options.get_account_types(options_map)
        holdings_list = holdings.get_final_holdings(entries,
                                                    (account_types.assets,
                                                     account_types.liabilities),
                                                    price_map)

        # Aggregate the holdings.
        holdings_list = holdings.aggregate_by_base_quote(holdings_list)

        # Convert holdings to a unified currency.
        holdings_list = holdings.convert_to_currency(price_map, currency, holdings_list)
        if not holdings_list:
            continue

        total = sum(holding.market_value
                    for holding in holdings_list
                    if holding.market_value is not None)
        net_worths.append((currency, total))

    field_spec = [
        (0, 'Currency'),
        (1, 'Net Worth', '{:,.2f}'.format),
    ]
    return table.create_table(net_worths, field_spec)
