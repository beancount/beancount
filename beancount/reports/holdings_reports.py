"""Generate reports no holdings.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import csv

from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core import account
from beancount.core import data
from beancount.core import flags
from beancount.parser import printer
from beancount.parser import options
from beancount.core import prices
from beancount.core import convert
from beancount.ops import holdings
from beancount.ops import summarize
from beancount.utils import table
from beancount.reports import base


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


def get_holdings_entries(entries, options_map):
    """Summarizes the entries to list of entries representing the final holdings..

    This list includes the latest prices entries as well. This can be used to
    load a full snapshot of holdings without including the entire history. This
    is a way of summarizing a balance sheet in a way that filters away history.

    Args:
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A string, the entries to print out.
    """

    # The entries will be created at the latest date, against an equity account.
    latest_date = entries[-1].date
    _, equity_account, _ = options.get_previous_accounts(options_map)

    # Get all the assets.
    holdings_list, _ = holdings.get_assets_holdings(entries, options_map)

    # Create synthetic entries for them.
    holdings_entries = []

    for index, holding in enumerate(holdings_list):
        meta = data.new_metadata('report_holdings_print', index)
        entry = data.Transaction(meta, latest_date, flags.FLAG_SUMMARIZE,
                                 None, "", None, None, [])

        # Convert the holding to a position.
        pos = holdings.holding_to_position(holding)
        entry.postings.append(
            data.Posting(holding.account, pos.units, pos.cost, None, None, None))

        cost = -convert.get_cost(pos)
        entry.postings.append(
            data.Posting(equity_account, cost, None, None, None, None))

        holdings_entries.append(entry)

    # Get opening directives for all the accounts.
    used_accounts = {holding.account for holding in holdings_list}
    open_entries = summarize.get_open_entries(entries, latest_date)
    used_open_entries = [open_entry
                         for open_entry in open_entries
                         if open_entry.account in used_accounts]

    # Add an entry for the equity account we're using.
    meta = data.new_metadata('report_holdings_print', -1)
    used_open_entries.insert(0, data.Open(meta, latest_date, equity_account,
                                          None, None))

    # Get the latest price entries.
    price_entries = prices.get_last_price_entries(entries, None)

    return used_open_entries + holdings_entries + price_entries


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
    holdings_list, _ = holdings.get_assets_holdings(entries, options_map, currency)
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


def load_from_csv(fileobj):
    """Load a list of holdings from a CSV file.

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
        ('Price Date', 'price_date', None),
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
        except KeyError as exc:
            raise IOError("Invalid file contents for holdings") from exc

    for line in reader:
        value_dict = defaults_dict.copy()
        for (attr, converter), value in zip(attr_converters, line):
            if converter:
                value = converter(value)
            value_dict[attr] = value
        yield holdings.Holding(**value_dict)


class HoldingsReport(base.TableReport):
    """The full list of holdings for Asset and Liabilities accounts."""

    names = ['holdings']

    aggregations = {
        'commodity': dict(aggregation_key=lambda holding: holding.currency),

        'account': dict(aggregation_key=lambda holding: holding.account),

        'root-account': dict(
            aggregation_key=lambda holding: account.root(3, holding.account),
            sort_key=lambda holding: holding.market_value or ZERO),

        'currency': dict(aggregation_key=lambda holding: holding.cost_currency),
        }

    def __init__(self, *rest, **kwds):
        super().__init__(*rest, **kwds)
        if self.args.relative and not self.args.currency:
            self.parser.error("--relative needs to have --currency set")

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-c', '--currency',
                            action='store', default=None,
                            help="Which currency to convert all the holdings to")

        parser.add_argument('-r', '--relative',
                            action='store_true',
                            help="True if we should render as relative values only")

        parser.add_argument('-g', '--groupby', '--by',
                            action='store', default=None,
                            choices=cls.aggregations.keys(),
                            help="How to group the holdings (default is: don't group)")

    def generate_table(self, entries, errors, options_map):
        keywords = self.aggregations[self.args.groupby] if self.args.groupby else {}
        return report_holdings(self.args.currency, self.args.relative,
                               entries, options_map,
                               **keywords)

    def render_beancount(self, entries, errors, options_map, file):
        # Don't allow any aggregations if we output as beancount format.
        for attribute in 'currency', 'relative', 'groupby':
            if getattr(self.args, attribute):
                self.parser.error(
                    "'beancount' format does not support --{} option".format(attribute))

        # Get the summarized entries and print them out.
        holdings_entries = get_holdings_entries(entries, options_map)
        dcontext = options_map['dcontext']
        printer.print_entries(holdings_entries, dcontext, file=file)


class CashReport(base.TableReport):
    """The list of cash holdings (defined as currency = cost-currency)."""

    names = ['cash']

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-c', '--currency',
                            action='store', default=None,
                            help="Which currency to convert all the holdings to")

        parser.add_argument('-i', '--ignored',
                            action='store_true',
                            help="Report on ignored holdings instead of included ones")

        parser.add_argument('-o', '--operating-only',
                            action='store_true',
                            help="Only report on operating currencies")

    def generate_table(self, entries, errors, options_map):
        holdings_list, price_map = holdings.get_assets_holdings(entries, options_map)
        holdings_list_orig = holdings_list

        # Keep only the holdings where currency is the same as the cost-currency.
        holdings_list = [holding
                         for holding in holdings_list
                         if (holding.currency == holding.cost_currency or
                             holding.cost_currency is None)]

        # Keep only those holdings held in one of the operating currencies.
        if self.args.operating_only:
            operating_currencies = set(options_map['operating_currency'])
            holdings_list = [holding
                             for holding in holdings_list
                             if holding.currency in operating_currencies]

        # Compute the list of ignored holdings and optionally report on them.
        if self.args.ignored:
            ignored_holdings = set(holdings_list_orig) - set(holdings_list)
            holdings_list = ignored_holdings

        # Convert holdings to a unified currency.
        if self.args.currency:
            holdings_list = holdings.convert_to_currency(price_map, self.args.currency,
                                                         holdings_list)

        return table.create_table(holdings_list, FIELD_SPEC)


def calculate_net_worths(entries, options_map):
    holdings_list, price_map = holdings.get_assets_holdings(entries, options_map)
    net_worths = []
    for currency in options_map['operating_currency']:

        # Convert holdings to a unified currency.
        #
        # Note: It's entirely possible that the price map does not have all
        # the necessary rate conversions here. The resulting holdings will
        # simply have no cost when that is the case. We must handle this
        # gracefully below.
        currency_holdings_list = holdings.convert_to_currency(price_map,
                                                              currency,
                                                              holdings_list)
        if not currency_holdings_list:
            continue

        aggregated_holdings_list = holdings.aggregate_holdings_by(
            currency_holdings_list, lambda holding: holding.cost_currency)

        aggregated_holdings_list = [holding
                         for holding in aggregated_holdings_list
                         if holding.currency and holding.cost_currency]

        # If after conversion there are no valid holdings, skip the currency
        # altogether.
        if not aggregated_holdings_list:
            continue

        net_worths.append((currency, aggregated_holdings_list[0].market_value))
    return net_worths


class NetWorthReport(base.TableReport):
    """Generate a table of total net worth for each operating currency."""

    names = ['networth', 'equity']

    def generate_table(self, entries, errors, options_map):
        field_spec = [
            (0, 'Currency'),
            (1, 'Net Worth', '{:,.2f}'.format),
        ]
        return table.create_table(calculate_net_worths(entries, options_map),
                                  field_spec)


__reports__ = [
    HoldingsReport,
    CashReport,
    NetWorthReport,
]
