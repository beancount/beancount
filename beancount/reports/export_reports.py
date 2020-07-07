"""Reports to Export to third-party portfolio sites.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import csv
import datetime
import itertools
import io
import logging
import re
import textwrap
import sys

from decimal import Decimal

from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core import getters
from beancount.core import amount
from beancount.core import prices
from beancount.ops import holdings
from beancount.reports import base


# The name of the metadata field used by this report.
FIELD = 'export'


# An entry to be exported.
#
# Attributes:
#   symbol: A string, the ticker to use for that position.
#   cost_currency: A string, the quote commodity.
#   number: A Decimal, the number of units for that position.
#   cost_number: A Decimal, the price of that currency.
#   mutual_fund: A boolean, true if this positions is a mutual fund.
#   memo: A string to be attached to the export.
#   holdings: The list of holdings that this position was create for.
ExportEntry = collections.namedtuple(
    'ExportEntry', 'symbol cost_currency number cost_number mutual_fund memo holdings')


def is_mutual_fund(ticker):
    """Return true if the GFinance ticker is for a mutual fund.

    Args:
      ticker: A string, the symbol for GFinance.
    Returns:
      A boolean, true for mutual funds.
    """
    return bool(re.match('MUTF.*:', ticker))


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
    exports = getters.get_values_meta(commodities_map, FIELD)

    # Classify the holdings based on their commodities' ticker metadata field.
    action_holdings = []
    for holding in holdings_list:
        # Get export field and remove (MONEY:...) specifications.
        export = re.sub(r'\(.*\)', '', exports.get(holding.currency, None) or '').strip()
        if export:
            if export.upper() == "CASH":
                action_holdings.append(('CASH', holding))
            elif export.upper() == "IGNORE":
                action_holdings.append(('IGNORE', holding))
            else:
                action_holdings.append((export, holding))
        else:
            logging.warning(("Exporting holding using default commodity name '{}'; this "
                             "can potentially break the OFX import. Consider providing "
                             "'export' metadata for your commodities.").format(
                                 holding.currency))
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
    instruments = {}
    for currency, entry in commodities_map.items():
        export = entry.meta.get(FIELD, '')
        paren_match = re.search(r'\((.*)\)', export)
        if paren_match:
            match = re.match('MONEY:({})'.format(amount.CURRENCY_RE), paren_match.group(1))
            if match:
                instruments[match.group(1)] = (
                    re.sub(r'\(.*\)', '', export).strip() or currency)
            else:
                logging.error("Invalid money specification: %s", export)

    return instruments


def export_holdings(entries, options_map, promiscuous, aggregate_by_commodity=False):
    """Compute a list of holdings to export.

    Holdings that are converted to cash equivalents will receive a currency of
    "CASH:<currency>" where <currency> is the converted cash currency.

    Args:
      entries: A list of directives.
      options_map: A dict of options as provided by the parser.
      promiscuous: A boolean, true if we should output a promiscuous memo.
      aggregate_by_commodity: A boolean, true if we should group the holdings by account.
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
    holdings_list, price_map = holdings.get_assets_holdings(entries, options_map)
    commodities = getters.get_commodity_directives(entries)
    dcontext = options_map['dcontext']

    # Aggregate the holdings, if requested. Google Finance is notoriously
    # finnicky and if you have many holdings this might help.
    if aggregate_by_commodity:
        holdings_list = holdings.aggregate_holdings_by(holdings_list,
                                                       lambda holding: holding.currency)

    # Classify all the holdings for export.
    action_holdings = classify_holdings_for_export(holdings_list, commodities)

    # The lists of exported and converted export entries, and the list of
    # ignored holdings.
    exported = []
    converted = []
    holdings_ignored = []

    # Export the holdings with tickers individually.
    for symbol, holding in action_holdings:
        if symbol in ("CASH", "IGNORE"):
            continue

        if holding.cost_number is None:
            assert holding.cost_currency in (None, holding.currency)
            cost_number = holding.number
            cost_currency = holding.currency
        else:
            cost_number = holding.cost_number
            cost_currency = holding.cost_currency

        exported.append(
            ExportEntry(symbol,
                        cost_currency,
                        holding.number,
                        cost_number,
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
    money_instruments = get_money_instruments(commodities)

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

        if cost_currency in money_instruments:
            # The holding is already in terms of one of the money instruments.
            money_values_book[cost_currency] += book_value
            money_values_market[cost_currency] += market_value
            money_values_holdings[cost_currency].extend(holdings_list)
        else:
            # The holding is not in terms of one of the money instruments.
            # Find the first available price to convert it into one
            for money_currency in money_instruments:
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
                holdings_ignored.extend(holdings_list)

    for money_currency in money_values_book.keys():
        book_value = money_values_book[money_currency]
        market_value = money_values_market[money_currency]
        holdings_list = money_values_holdings[money_currency]

        symbol = money_instruments[money_currency]

        assert isinstance(book_value, Decimal)
        assert isinstance(market_value, Decimal)
        converted.append(
            ExportEntry(symbol,
                        money_currency,
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


def render_ofx_date(dtime):
    """Render a datetime to the OFX format.

    Args:
      dtime: A datetime.datetime instance.
    Returns:
      A string, rendered to milliseconds.
    """
    return '{}.{:03d}'.format(dtime.strftime('%Y%m%d%H%M%S'),
                              int(dtime.microsecond / 1000))


def get_symbol(sources, prefer='google'):
    """Filter a source specification to some corresponding ticker.

    Args:
      source: A comma-separated list of sources as a string, such as
        "google/NASDAQ:AAPL,yahoo/AAPL".
    Returns:
      The symbol string.
    Raises:
      ValueError: If the sources does not contain a ticker for the
        google source.
    """

    # If the ticker is a list of <source>/<symbol>, extract the symbol
    # from it.
    symbol_items = []
    for source in map(str.strip, sources.split(',')):
        match = re.match('([a-zA-Z][a-zA-Z0-9._]+)/(.*)', source)
        if match:
            source, symbol = match.groups()
        else:
            source, symbol = None, source
        symbol_items.append((source, symbol))
    if not symbol_items:
        raise ValueError(
            'Invalid source "{}" does not contain a ticker'.format(sources))
    symbol_map = dict(symbol_items)
    # If not found, return the first symbol in the list of items.
    return symbol_map.get(prefer, symbol_items[0][1])


class ExportPortfolioReport(base.TableReport):
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

        parser.add_argument('-a', '--aggregate-by-commodity', action='store_true',
                            help=("Group the holdings by account. This may help if your "
                                  "portfolio fails to import and you have many holdings."))

    EXPORT_FORMAT = ("{atype}  "
                     "{0.number:10.2f} {0.symbol:16}  "
                     "{cost_number:10.2f} {0.cost_currency:16}  "
                     "{0.memo}\n")

    HOLDING_FORMAT = ("  Holding: {h.account:48} "
                      "{h.number:10.2f} {h.currency:12} "
                      "{cost_number:10.2f} {cost_currency:12}\n")

    def _render_debug_exports(self, export_entries, title=None):
        if title:
            sys.stderr.write('{}:\n'.format(title))
            sys.stderr.write('----------------------------------------\n')
        for export_entry in export_entries:
            sys.stderr.write(self.EXPORT_FORMAT.format(
                export_entry,
                atype='MUTFUND' if export_entry.mutual_fund else 'STOCK  ',
                cost_number=export_entry.cost_number or 0))
            self._render_debug_holdings(export_entry.holdings)
        sys.stderr.write('\n')

    def _render_debug_holdings(self, holdings, title=None):
        if title:
            sys.stderr.write('{}:\n'.format(title))
            sys.stderr.write('----------------------------------------\n')
        for holding in holdings:
            sys.stderr.write(self.HOLDING_FORMAT.format(
                h=holding,
                cost_number=holding.cost_number or ZERO,
                cost_currency=holding.cost_currency))
        sys.stderr.write('\n')

    def render_csv(self, entries, unused_errors, options_map, file):
        exported, converted, holdings_ignored = export_holdings(
            entries, options_map, False, self.args.aggregate_by_commodity)
        writer = csv.writer(file)
        writer.writerow(ExportEntry._fields[:-1])
        for index, export in enumerate(itertools.chain(exported, converted)):
            writer.writerow(export[:-1])

    def render_ofx(self, entries, unused_errors, options_map, file):
        exported, converted, holdings_ignored = export_holdings(
            entries, options_map, False, self.args.aggregate_by_commodity)

        # Print debug information if requested, before exporting.
        if self.args.debug:
            self._render_debug_exports(exported, 'Exported Holdings')
            self._render_debug_exports(converted, 'Cash Holdings')
            self._render_debug_holdings(holdings_ignored, 'Ignored Holdings')

        # Compute trade date. Note that we'll enter the positions two days ago.
        # When we have lot-dates on all lots, put these transactions at the
        # correct dates.
        morning = datetime.datetime.now().replace(hour=9, minute=0, second=0, microsecond=0)
        trade_date = morning - datetime.timedelta(days=2)

        # A list of all accumulated commodities.
        commodities = set()

        # pylint: disable=possibly-unused-variable
        dcontext = options_map['dcontext']

        invtranlist_io = io.StringIO()
        skipped_holdings = []
        ignored_commodities = set()
        for index, export in enumerate(itertools.chain(exported, converted)):
            # Note: We assume GFinance ticker symbology here to infer MF vs.
            # STOCK, but frankly even if we fail to characterize it right this
            # distinction is not important, it's just an artifact of OFX, I
            # verified that the GFinance portfolio feature doesn't appear to
            # care whether it was loaded as a MF or STOCK. Nevertheless, we
            # "try" to get it right by inferring this from the symbol. We could
            # eventually recognize a "class" metadata field from the
            # commodities, but I feel that this simpler. Less is more.
            txntype = ('BUYMF' if export.mutual_fund else 'BUYSTOCK')
            fitid = index + 1
            dttrade = render_ofx_date(trade_date)
            memo = export.memo
            uniqueid = get_symbol(export.symbol)
            units = export.number
            unitprice = export.cost_number
            fee = ZERO
            total = -(units * unitprice + fee)
            buytype = 'BUY'

            invtranlist_io.write(self.TRANSACTION.format(**locals()))
            commodities.add((get_symbol(export.symbol), export.mutual_fund))

        invtranlist = invtranlist_io.getvalue()

        # Create a list of securities.
        seclist_io = io.StringIO()
        for symbol, mutual_fund in sorted(commodities):
            uniqueid = symbol
            secname = symbol
            infotype = 'MFINFO' if mutual_fund else 'STOCKINFO'
            ticker = symbol
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


__reports__ = [
    ExportPortfolioReport,
]
