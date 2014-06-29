"""Generate reports no holdings.
"""
import csv

from beancount.core import amount
from beancount.core import account
from beancount.parser import options
from beancount.ops import prices
from beancount.ops import holdings
from beancount.reports import table


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

# A field spec for relative reports. Skipping the book value here because by
# combining it with market value % and price one could theoretically determined
# the total value of the portfolio.
RELATIVE_FIELD_SPEC = FIELD_SPEC[:-2] + [
    ('market_value', 'Frac Market Value', '{:,.4f}'.format),
]


def report_holdings(currency, relative, entries, options_map,
                    aggregation_key=None,
                    sort_key=None):
    """Generate a detailed list of all holdings.

    Args:
      currency: A string, a currency to convert to. If left to None, no
        conversion is carried out.
      relative: A boolean, true if we should reduce this to a relative value.
      entries: A list of directives.
      options_map: A dict of parsed options.
      aggregation_key: A callable use to generate aggregations.
      sort_key: A function to use to sort the holdings, if specified.
    Returns:
      A Table instance.
    """
    holdings_list, _ = get_assets_holdings(entries, options_map, currency)
    if aggregation_key:
        holdings_list = holdings.aggregate_holdings_by(holdings_list, aggregation_key)

    if relative:
        holdings_list = holdings.reduce_relative(holdings_list)
        field_spec = RELATIVE_FIELD_SPEC
    else:
        field_spec = FIELD_SPEC

    if sort_key:
        holdings_list.sort(key=sort_key, reverse=True)

    return table.create_table(holdings_list, field_spec)


def report_holdings_bycommodity(currency, relative, entries, options_map):
    """Generate a detailed list of all holdings by (base, quote) pair.

    Args:
      currency: A string, a currency to convert to. If left to None, no
        conversion is carried out.
      relative: A boolean, true if we should reduce this to a relative value.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    return report_holdings(currency, relative, entries, options_map,
                           lambda holding: holding.currency)


def report_holdings_byaccount(currency, relative, entries, options_map):
    """Generate a detailed list of all holdings by account.

    Args:
      currency: A string, a currency to convert to. Must be non-null.
      relative: A boolean, true if we should reduce this to a relative value.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    return report_holdings(currency, relative, entries, options_map,
                           aggregation_key=lambda holding: holding.account)


def report_holdings_byaccount_shallow(currency, relative, entries, options_map):
    """Generate a detailed list of all holdings by account, with a max-depth.

    Args:
      currency: A string, a currency to convert to. Must be non-null.
      relative: A boolean, true if we should reduce this to a relative value.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance.
    """
    def account_maxdepth(n, account_):
        return account.sep.join(account_.split(account.sep)[:n])
    return report_holdings(
        currency, relative, entries, options_map,
        aggregation_key=lambda holding: account_maxdepth(3, holding.account),
        sort_key=lambda holding: holding.market_value or amount.ZERO)


def report_holdings_bycurrency(currency, relative, entries, options_map):
    """Generate a table of currency exposure.

    Args:
      currency: A string, a currency to convert to. Must be non-null.
      relative: A boolean, true if we should reduce this to a relative value.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A Table instance, where each row is a currency and a total amount.
    """
    return report_holdings(currency, relative, entries, options_map,
                           lambda holding: holding.cost_currency)


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
        currency_holdings_list = holdings.convert_to_currency(price_map,
                                                              currency,
                                                              holdings_list)
        if not currency_holdings_list:
            continue

        holdings_list = holdings.aggregate_holdings_by(
            currency_holdings_list, lambda holding: holding.cost_currency)

        holdings_list = [holding
                         for holding in holdings_list
                         if holding.currency and holding.cost_currency]

        assert len(holdings_list) == 1, holdings_list
        net_worths.append((currency, holdings_list[0].market_value))

    field_spec = [
        (0, 'Currency'),
        (1, 'Net Worth', '{:,.2f}'.format),
    ]
    return table.create_table(net_worths, field_spec)


def load_from_csv(fileobj):
    """Load a list of holdings from a CSV output file.

    Args:
      fileobj: A file object.
    Yields:
      Instances of Holding, as read from the file.
    """
    column_spec = [
        ('Account', 'account', None),
        ('Units', 'number', amount.to_decimal),
        ('Currency', 'currency', None),
        ('Cost Currency', 'cost_currency', None),
        ('Average Cost', 'cost_number', amount.to_decimal),
        ('Price', 'price_number', amount.to_decimal),
        ('Book Value', 'book_value', amount.to_decimal),
        ('Market Value', 'market_value', amount.to_decimal),
        ('Price Date', 'price_date', amount.to_decimal),
        ]
    column_dict = {name: (attr, converter)
                   for name, attr, converter in column_spec}
    klass = holdings.Holding

    # Create a set of default values for the namedtuple.
    defaults_dict = {attr: None for attr in klass._fields}

    # Start reading the file.
    reader = csv.reader(fileobj)

    # Check that the header is readable.
    header = next(reader)
    attr_converters = []
    for header_name in header:
        try:
            attr_converter = column_dict[header_name]
            attr_converters.append(attr_converter)
        except KeyError:
            raise IOError("Invalid file contents for holdings.")

    for line in reader:
        value_dict = defaults_dict.copy()
        for (attr, converter), value in zip(attr_converters, line):
            if converter:
                value = converter(value)
            value_dict[attr] = value
        yield holdings.Holding(**value_dict)
