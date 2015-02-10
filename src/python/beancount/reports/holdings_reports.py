"""Generate reports no holdings.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import csv
import collections
import datetime
import io
import re
import textwrap
import logging
import sys
import itertools

from decimal import Decimal

from beancount.core.amount import D
from beancount.core.amount import ZERO
from beancount.core import amount
from beancount.core import account
from beancount.core import data
from beancount.core import flags
from beancount.core import getters
from beancount.parser import options
from beancount.parser import printer
from beancount.ops import prices
from beancount.ops import holdings
from beancount.ops import summarize
from beancount.reports import table
from beancount.reports import report


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

    # The entries will be create at the latest date, against an equity account.
    latest_date = entries[-1].date
    _, equity_account, _ = options.get_previous_accounts(options_map)

    # Get all the assets.
    holdings_list, _ = get_assets_holdings(entries, options_map)

    # Create synthetic entries for them.
    holdings_entries = []

    for index, holding in enumerate(holdings_list):
        meta = data.new_metadata('report_holdings_print', index)
        entry = data.Transaction(meta, latest_date, flags.FLAG_SUMMARIZE,
                                 None, "", None, None, [])

        # Convert the holding to a position.
        position_ = holdings.holding_to_position(holding)

        entry.postings.append(
            data.Posting(entry, holding.account, position_, None, None, None))
        entry.postings.append(
            data.Posting(entry, equity_account, -position_.cost(), None, None, None))

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
        except KeyError:
            raise IOError("Invalid file contents for holdings")

    for line in reader:
        value_dict = defaults_dict.copy()
        for (attr, converter), value in zip(attr_converters, line):
            if converter:
                value = converter(value)
            value_dict[attr] = value
        yield holdings.Holding(**value_dict)


class HoldingsReport(report.TableReport):
    """The full list of holdings for Asset and Liabilities accounts."""

    names = ['holdings']

    aggregations = {
        'commodity': dict(aggregation_key=lambda holding: holding.currency),

        'account': dict(aggregation_key=lambda holding: holding.account),

        'root-account': dict(
            aggregation_key=lambda holding: account.root(3, holding.account),
            sort_key=lambda holding: holding.market_value or amount.ZERO),

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
        dcontext = options_map['display_context']
        printer.print_entries(holdings_entries, dcontext, file=file)


def is_mutual_fund(ticker):
    """Return true if the GFinanc ticker is for a mutual fund.

    Args:
      ticker: A string, the symbol for GFinance.
    Returns:
      A boolean, true for mutual funds.
    """
    return bool(re.match('MUTF.*:', ticker))


# An entry to be exported.
#
# Attributes:
#   ticker: A string, the ticker to use for that position.
#   cost_currency: A string, the quote commodity.
#   number: A Decimal, the number of units for that position.
#   cost_number: A Decimal, the price of that currency.
#   mutual_fund: A boolean, true if this positions is a mutual fund.
#   memo: A string to be attached to the export.
#   holdings: The list of holdings that this position was create for.
ExportEntry = collections.namedtuple(
    'ExportEntry', 'symbol cost_currency number cost_number mutual_fund memo holdings')


def classify_holdings_for_export(holdings_list, commodities_map):
    """Figure out what to do for example with each holding.

    Args:
      holdings_list: A list of Holding instances to be exported.
      commodities_map: A dict of commodity to Commodity instances.
    Returns:
      A pair of:
        action_holdings: A list of (symbol, holding) for each holding. 'Symbol'
          is the ticker to use for export, and may be "CASH" or "IGNORE" for
          holdings to be converted or ignored.
    """
    # Get the map of commodities to tickers and export meta tags.
    tickers = getters.get_values_meta(commodities_map, 'ticker')
    exports = getters.get_values_meta(commodities_map, 'export')

    # Classify the holdings based on their commodities' ticker metadata field.
    action_holdings = []
    for holding in holdings_list:
        export = exports.get(holding.currency, None)
        ticker = tickers.get(holding.currency, None)
        if isinstance(export, str) and export:
            if export.upper() == "CASH":
                action_holdings.append(('CASH', holding))
            elif export.upper() == "IGNORE":
                action_holdings.append(('IGNORE', holding))
            elif export.upper() == "MONEY":
                # Hmm this is an interesting case... an actual holding is in
                # units of our money-market standing. We could disallow this,
                # but we can also just export it. Let's export it with the
                # ticker value or commodity if present.
                action_holdings.append((ticker if ticker else holding.currency, holding))
            else:
                action_holdings.append((export, holding))
        elif ticker:
            action_holdings.append((ticker, holding))
        else:
            action_holdings.append((holding.currency, holding))

    return action_holdings


def get_money_instruments(commodities_map):
    """Get the money-market stand-ins for cash positions.

    Args:
      commodities_map: A map of currency to their corresponding Commodity directives.
    Returns:
      A dict of quote currency to the ticker symbol that stands for it,
      e.g. {'USD': 'VMMXX'}.
    """
    return {entry.meta.get('quote', None): entry.meta.get('ticker', currency)
            for currency, entry in commodities_map.items()
            if entry.meta.get('export', None) == 'MONEY'}


def export_holdings(entries, options_map, promiscuous):
    """Compute a list of holdings to export.

    Holdings that are converted to cash equivalents will receive a currency of
    "CASH:<currency>" where <currency> is the converted cash currency.

    Args:
      entries: A list of directives.
      options_map: A dict of options as provided by the parser.
      promiscuous: A boolean, true if we should output a promiscuious memo.
    Returns:
      A pair of
        exported: A list of ExportEntry tuples, one for each exported position.
        converted: A list of ExportEntry tuples, one for each converted position.
          These will contain multiple holdings.
        holdings_ignored: A list of Holding instances that were ignored, either
          because they were explicitly marked to be ignored, or because we could
          not convert them to a money vehicle matching the holding's cost-currency.
    """
    # Get the desired list of holdings.
    holdings_list, price_map = get_assets_holdings(entries, options_map)
    commodities_map = getters.get_commodity_map(entries)
    dcontext = options_map['display_context']

    # Classify all the holdings for export.
    action_holdings = classify_holdings_for_export(holdings_list, commodities_map)

    # The lists of exported and converted export entries, and the list of
    # ignored holdings.
    exported = []
    converted = []
    holdings_ignored = []

    # Export the holdings with tickers individually.
    for symbol, holding in action_holdings:
        if symbol in ("CASH", "IGNORE"):
            continue
        exported.append(
            ExportEntry(symbol,
                        holding.cost_currency,
                        holding.number,
                        holding.cost_number,
                        is_mutual_fund(symbol),
                        holding.account if promiscuous else '',
                        [holding]))

    # Convert all the cash entries to their book and market value by currency.
    cash_holdings_map = collections.defaultdict(list)
    for symbol, holding in action_holdings:
        if symbol != "CASH":
            continue

        if holding.cost_currency:
            # Accumulate market and book values.
            cash_holdings_map[holding.cost_currency].append(holding)
        else:
            # We cannot price this... no cost currency.
            holdings_ignored.append(holding)

    # Get the money instruments.
    money_instruments = get_money_instruments(commodities_map)

    # Convert all the cash values to money instruments, if possible. If not
    # possible, we'll just have to ignore those values.

    # Go through all the holdings to convert, and for each of those which aren't
    # in terms of one of the money instruments, which we can directly add to the
    # exported portfolio, attempt to convert them into currencies to one of
    # those in the money instruments.
    money_values_book = collections.defaultdict(D)
    money_values_market = collections.defaultdict(D)
    money_values_holdings = collections.defaultdict(list)
    for cost_currency, holdings_list in cash_holdings_map.items():
        book_value = sum(holding.book_value for holding in holdings_list)
        market_value = sum(holding.market_value for holding in holdings_list)
        ##print("X {:16} {:12.2f} {:12.2f}".format(cost_currency, book_value, market_value))

        if cost_currency in money_instruments:
            # The holding is already in terms of one of the money instruments.
            money_values_book[cost_currency] += book_value
            money_values_market[cost_currency] += market_value
            money_values_holdings[cost_currency].extend(holdings_list)
        else:
            # The holding is not in terms of one of the money instruments.
            # Find the first available price to convert it into one
            for money_currency in money_instruments.keys():
                base_quote = (cost_currency, money_currency)
                _, rate = prices.get_latest_price(price_map, base_quote)
                if rate is not None:
                    money_values_book[money_currency] += book_value * rate
                    money_values_market[money_currency] += market_value * rate
                    money_values_holdings[money_currency].extend(holdings_list)
                    break
            else:
                # We could not convert into any of the money commodities. Ignore
                # those holdings.
                holdings_ignore.extend(holdings_list)

    for money_currency in money_values_book.keys():
        book_value = money_values_book[money_currency]
        market_value = money_values_market[money_currency]
        holdings_list = money_values_holdings[money_currency]

        symbol = money_instruments[money_currency]

        assert isinstance(book_value, Decimal)
        assert isinstance(market_value, Decimal)
        converted.append(
            ExportEntry(symbol,
                        holding.cost_currency,
                        dcontext.quantize(market_value, money_currency),
                        dcontext.quantize(book_value / market_value, money_currency),
                        is_mutual_fund(symbol),
                        '',
                        holdings_list))

    # Add all ignored holdings to a final list.
    for symbol, holding in action_holdings:
        if symbol == "IGNORE":
            holdings_ignored.append(holding)

    return exported, converted, holdings_ignored


class ExportPortfolioReport(report.TableReport):
    """Holdings lists that can be exported to external portfolio management software."""

    names = ['export_holdings', 'export_portfolio', 'pfexport', 'exportpf']
    default_format = 'ofx'

    PREFIX = textwrap.dedent("""\
        OFXHEADER:100
        DATA:OFXSGML
        VERSION:102
        SECURITY:NONE
        ENCODING:USASCII
        CHARSET:1252
        COMPRESSION:NONE
        OLDFILEUID:NONE
        NEWFILEUID:NONE

    """)

    TEMPLATE = textwrap.dedent("""
        <OFX>
          <SIGNONMSGSRSV1>
            <SONRS>
              <STATUS>
                <CODE>0
                <SEVERITY>INFO
              </STATUS>
              <DTSERVER>{dtserver}
              <LANGUAGE>ENG
            </SONRS>
          </SIGNONMSGSRSV1>
          <INVSTMTMSGSRSV1>
            <INVSTMTTRNRS>
              <TRNUID>1001
              <STATUS>
                <CODE>0
                <SEVERITY>INFO
              </STATUS>
              <INVSTMTRS>
                <DTASOF>{dtasof}
                <CURDEF>USD
                <INVACCTFROM>
                  <BROKERID>{broker}
                  <ACCTID>{account}
                </INVACCTFROM>
                <INVTRANLIST>
                  <DTSTART>{dtstart}
                  <DTEND>{dtend}
                  {invtranlist}
                </INVTRANLIST>
              </INVSTMTRS>
            </INVSTMTTRNRS>
          </INVSTMTMSGSRSV1>
          <SECLISTMSGSRSV1>
            <SECLIST>
             {seclist}
            </SECLIST>
          </SECLISTMSGSRSV1>
        </OFX>
    """)

    TRANSACTION = textwrap.dedent("""
                  <{txntype}>
                    <INVBUY>
                      <INVTRAN>
                        <FITID>{fitid}
                        <DTTRADE>{dttrade}
                        <MEMO>{memo}
                      </INVTRAN>
                      <SECID>
                        <UNIQUEID>{uniqueid}
                        <UNIQUEIDTYPE>TICKER
                      </SECID>
                      <UNITS>{units}
                      <UNITPRICE>{unitprice}
                      <COMMISSION>{fee}
                      <TOTAL>{total}
                      <SUBACCTSEC>CASH
                      <SUBACCTFUND>CASH
                    </INVBUY>
                    <BUYTYPE>{buytype}
                  </{txntype}>
    """)

    # Note: This does not import well in GFinance.
    # CASH = textwrap.dedent("""
    #       <INVBANKTRAN>
    #         <STMTTRN>
    #           <TRNTYPE>OTHER
    #           <DTPOSTED>{dtposted}
    #           <TRNAMT>{trnamt}
    #           <FITID>{fitid}
    #         </STMTTRN>
    #         <SUBACCTFUND>CASH
    #       </INVBANKTRAN>
    # """)

    SECURITY = textwrap.dedent("""
              <{infotype}>
                <SECINFO>
                  <SECID>
                    <UNIQUEID>{uniqueid}
                    <UNIQUEIDTYPE>TICKER
                  </SECID>
                  <SECNAME>{secname}
                  <TICKER>{ticker}
                </SECINFO>
              </{infotype}>
    """)

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-d', '--debug', action='store_true',
                            help="Output position export debugging information on stderr.")

        parser.add_argument('-p', '--promiscuous', action='store_true',
                            help=("Include title and account names in memos. "
                                  "Use this if you trust wherever you upload."))

    # The cash equivalent currency. Note: Importing a cash deposit in GFinance
    # portfolio import feature fails, so use a cash equivalent (Vanguard Prime
    # Money Market Fund, which pretty much has a fixed price of 1.0 USD).
    CASH_EQUIVALENT_CURRENCY = 'VMMXX'
    CASH_EQUIVALENT_MFUND = True

    def render_ofx(self, entries, unused_errors, options_map, file):
        holdings_list, price_map = get_assets_holdings(entries, options_map)
        dcontext = options_map['display_context']

        commodities_map = getters.get_commodity_map(entries)
        undefined = object()
        tickers = getters.get_values_meta(commodities_map, 'ticker', default=undefined)

        # Create a list of purchases.
        #
        # Note: we'll enter the positions two days ago. When we have lot-dates
        # on all lots, put these transactions at the correct dates.
        morning = datetime.datetime.now().replace(hour=9, minute=0, second=0, microsecond=0)
        trade_date = morning - datetime.timedelta(days=2)

        invtranlist_io = io.StringIO()
        commodities = set()
        skipped_holdings = []
        ignored_commodities = set()
        index = 0
        for index, holding in enumerate(holdings_list):
            ticker = tickers.get(holding.currency, None)

            if ticker is undefined:
                ignored_commodities.add(holding.currency)

            if (holding.currency == holding.cost_currency or
                holding.cost_number is None or
                not ticker or ticker is undefined):
                skipped_holdings.append(holding)
                continue

            # Note: We assume GFinance ticker symbology here to infer MF vs.
            # STOCK, but frankly even if we fail to characterize it right this
            # distinction is not important, it's just an artifact of OFX, I
            # verified that the GFinance portfolio feature doesn't appear to
            # care whether it was loaded as a MF or STOCK. Nevertheless, we
            # "try" to get it right by inferring this from the symbol. We could
            # eventually recognize a "class" metadata field from the
            # commodities, but I feel that this simpler. Less is more.
            txntype = ('BUYMF'
                       if is_mutual_fund(ticker)
                       else 'BUYSTOCK')
            fitid = index + 1
            dttrade = render_ofx_date(trade_date)
            memo = holding.account if self.args.promiscuous else ''
            uniqueid = ticker
            units = holding.number
            unitprice = holding.cost_number
            fee = ZERO
            total = -(units * unitprice + fee)
            buytype = 'BUY'

            invtranlist_io.write(self.TRANSACTION.format(**locals()))
            commodities.add((holding.currency, ticker, is_mutual_fund(ticker)))

        # Print a table of ignored holdings (for debugging).
        ## import sys
        ## table.render_table(table.create_table(skipped_holdings, FIELD_SPEC),
        ##                    sys.stderr, 'text')

        # Convert the skipped holdings to a bank deposit to cash to approximate their value.
        if options_map['operating_currency']:
            # Convert all skipped holdings to the first operating currency.
            cash_currency = options_map['operating_currency'][0]
            converted_holdings = holdings.convert_to_currency(price_map,
                                                              cash_currency,
                                                              skipped_holdings)

            # Estimate the total market value in cash.
            included_holdings = []
            for holding in converted_holdings:
                if holding.cost_currency == cash_currency:
                    included_holdings.append(holding)
            book_value = sum(holding.book_value for holding in included_holdings)
            market_value = sum(holding.market_value for holding in included_holdings)

            # Insert a cash deposit equivalent for that amount.
            txntype = ('BUYMF' if self.CASH_EQUIVALENT_MFUND else 'BUYSTOCK')
            fitid = index + 1
            dttrade = render_ofx_date(trade_date)
            memo = ''
            uniqueid = self.CASH_EQUIVALENT_CURRENCY
            units = dcontext.quantize(market_value, cash_currency)
            unitprice = dcontext.quantize(book_value / market_value, cash_currency)
            fee = ZERO
            total = -(units * unitprice + fee)
            buytype = 'BUY'

            invtranlist_io.write(self.TRANSACTION.format(**locals()))
            commodities.add((self.CASH_EQUIVALENT_CURRENCY,
                             self.CASH_EQUIVALENT_CURRENCY,
                             False))

        invtranlist = invtranlist_io.getvalue()

        # Create a list of securities.
        seclist_io = io.StringIO()
        for currency, ticker, mutual_fund in sorted(commodities):
            uniqueid = currency
            secname = currency
            infotype = 'MFINFO' if mutual_fund else 'STOCKINFO'
            ticker = ticker
            seclist_io.write(self.SECURITY.format(**locals()))
        seclist = seclist_io.getvalue()

        # Create the top-level template.
        broker = 'Beancount'
        account = options_map['title'] if self.args.promiscuous else ''
        dtserver = dtasof = dtstart = dtend = render_ofx_date(morning)
        contents = self.TEMPLATE.format(**locals())

        # Clean up final contents and output it.
        stripped_contents = '\n'.join(line.lstrip()
                                      for line in contents.splitlines()
                                      if line.strip())
        file.write(self.PREFIX + stripped_contents)

        if self.args.debug:
            log = sys.stderr.write
            for commodity in ignored_commodities:
                log("Ignoring commodity '{}'".format(commodity))


    def render_ofx(self, entries, unused_errors, options_map, file):
        exported, converted, holdings_ignored = export_holdings(entries, options_map, False)

        if self.args.debug:
            self._render_debug_exports(exported, 'Exported Holdings')
            self._render_debug_exports(converted, 'Cash Holdings')
            self._render_debug_holdings(holdings_ignored, 'Ignored Holdings')

    EXPORT_FORMAT = ("{0.symbol:16} {0.cost_currency:16} "
                     "{0.number:10.2f} {cost_number:10.2f} "
                     "{0.mutual_fund} {0.memo}\n")

    def _render_debug_exports(self, export_entries, title=None):
        if title:
            sys.stderr.write('-------- {}:\n'.format(title))
        for export_entry in export_entries:
            sys.stderr.write(self.EXPORT_FORMAT.format(
                export_entry,
                cost_number=export_entry.cost_number or 0))
            self._render_debug_holdings(export_entry.holdings)
        sys.stderr.write('\n')

    HOLDING_FORMAT = ("  Holding: {h.account:48} "
                      "{h.number:10.2f} {h.currency:12} "
                      "{cost_number:10.2f} {cost_currency:12}\n")

    @staticmethod
    def hargs(holding):
        return dict(h=holding,
                    cost_number=holding.cost_number or ZERO,
                    cost_currency=holding.cost_currency)

    def _render_debug_holdings(self, holdings, title=None):
        if title:
            sys.stderr.write('-------- {}:\n'.format(title))
        for holding in holdings:
            sys.stderr.write(self.HOLDING_FORMAT.format(**self.hargs(holding)))
        sys.stderr.write('\n')


def render_ofx_date(dtime):
    """Render a datetime to the OFX format.

    Args:
      dtime: A datetime.datetime instance.
    Returns:
      A string, rendered to milliseconds.
    """
    return '{}.{:03d}'.format(dtime.strftime('%Y%m%d%H%M%S'),
                              int(dtime.microsecond / 1000))


class CashReport(report.TableReport):
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
        holdings_list, price_map = get_assets_holdings(entries, options_map)
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


class NetWorthReport(report.TableReport):
    """Generate a table of total net worth for each operating currency."""

    names = ['networth', 'equity']

    def generate_table(self, entries, errors, options_map):
        holdings_list, price_map = get_assets_holdings(entries, options_map)

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

            holdings_list = holdings.aggregate_holdings_by(
                currency_holdings_list, lambda holding: holding.cost_currency)

            holdings_list = [holding
                             for holding in holdings_list
                             if holding.currency and holding.cost_currency]

            # If after conversion there are no valid holdings, skip the currency
            # altogether.
            if not holdings_list:
                continue

            net_worths.append((currency, holdings_list[0].market_value))

        field_spec = [
            (0, 'Currency'),
            (1, 'Net Worth', '{:,.2f}'.format),
        ]
        return table.create_table(net_worths, field_spec)


__reports__ = [
    HoldingsReport,
    CashReport,
    NetWorthReport,
    ExportPortfolioReport,
    ]
