"""Generate reports no holdings.
"""
import csv
import io

from beancount.core.amount import D
from beancount.core import amount
from beancount.core import account
from beancount.core import data
from beancount.core import position
from beancount.core import flags
from beancount.parser import options
from beancount.parser import printer
from beancount.parser import options
from beancount.ops import prices
from beancount.ops import holdings
from beancount.ops import summarize
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
RELATIVE_FIELD_SPEC = [
    field_desc
    for field_desc in FIELD_SPEC
    if field_desc[0] not in ('account', 'number', 'book_value', 'market_value')
] + [
    ('market_value', 'Frac Folio', '{:,.2%}'.format),
]


def report_holdings_print(entries, options_map):
    """Generate a printed list of entries that represent the holdings.

    This list includes the latest prices entries as well. This can be used to
    load a full snapshot of holdings without including the entire history. This
    is a way of summarizing a balance sheet in a way that filters away history.

    Args:
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A string, the entries to print out.

    """
    # The entries will be create at the latest date, against an equity account.
    latest_date = entries[-1].date
    _, equity_account, _ = options.get_previous_accounts(options_map)

    # Get all the assets.
    holdings_list, _ = get_assets_holdings(entries, options_map)

    # Create synthetic entries for them.
    holdings_entries = []

    for index, holding in enumerate(holdings_list):
        fileloc = data.FileLocation('report_holdings_print', index)
        entry = data.Transaction(fileloc, latest_date, flags.FLAG_SUMMARIZE,
                                 None, "", None, None, [])


        # Convert the holding to a position.
        # (FIXME: Move this to a function.)
        cost = (amount.Amount(holding.cost_number, holding.cost_currency)
                if holding.cost_number
                else None)
        position_ = position.Position(position.Lot(holding.currency, cost, None), holding.number)

        entry.postings.append(data.Posting(entry, holding.account, position_, None, None))
        entry.postings.append(data.Posting(entry, equity_account, -position_.get_cost_position(), None, None))

        holdings_entries.append(entry)


    # Get opening directives for all the accounts.
    used_accounts = {holding.account for holding in holdings_list}
    open_entries = summarize.get_open_entries(entries, latest_date)
    used_open_entries = [entry
                         for entry in open_entries
                         if entry.account in used_accounts]

    # FIXME: Why doesn't this appear anywhere... did we forget to add it in?
    # If so, why doesn't the validation routine warn about it? WTF?
    fileloc = data.FileLocation('report_holdings_print', -1)
    used_open_entries.insert(0,
                             data.Open(fileloc, latest_date, equity_account, None))


    # Get the latest price entries.
    price_entries = prices.get_last_price_entries(entries, None)



    # FIXME: You have to output the options too.
    oss = io.StringIO()
    printer.print_entries(used_open_entries, oss)
    printer.print_entries(holdings_entries, oss)
    printer.print_entries(price_entries, oss)
    return oss.getvalue()


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
        return account.join.join(*(account.split(account_)[:n]))
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
        ('Units', 'number', D),
        ('Currency', 'currency', None),
        ('Cost Currency', 'cost_currency', None),
        ('Average Cost', 'cost_number', D),
        ('Price', 'price_number', D),
        ('Book Value', 'book_value', D),
        ('Market Value', 'market_value', D),
        ('Price Date', 'price_date', D),
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
