"""Miscellaneous report classes.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import re

from beancount.reports import base
from beancount.utils import table
from beancount.reports import gviz
from beancount.parser import printer
from beancount.core import data
from beancount.core import amount
from beancount.core import getters
from beancount.core import prices
from beancount.ops import lifetimes


class CommoditiesReport(base.TableReport):
    """Print out a list of commodities."""

    names = ['commodities']
    default_format = 'text'

    def generate_table(self, entries, errors, options_map):
        price_map = prices.build_price_map(entries)
        return table.create_table([(base_quote,)
                                   for base_quote in sorted(price_map.forward_pairs)],
                                  [(0, "Base/Quote", self.formatter.render_commodity)])


class CommodityLifetimes(base.TableReport):
    """Print out a list of lifetimes of each commodity."""

    names = ['lifetimes']
    default_format = 'text'

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-c', '--compress-days', type=int,
                            action='store', default=None,
                            help="The number of unused days to allow for continuous usage.")

    def render_text(self, entries, errors, options_map, file):
        lifetimes_map = lifetimes.get_commodity_lifetimes(entries)
        if self.args.compress_days:
            lifetimes_map = lifetimes.compress_lifetimes_days(lifetimes_map,
                                                              self.args.compress_days)

        name_map = {pair: '{}/{}'.format(pair[0], pair[1]) if pair[1] else pair[0]
                    for pair in lifetimes_map.keys()}
        ccywidth = max(map(len, name_map.values()))
        for currency, lifetime in sorted(lifetimes_map.items(),
                                         key=lambda x: (x[1][0][0], x[0])):
            file.write('{:{width}}: {}\n'.format(
                name_map[currency],
                '  /  '.join('{} - {}'.format(begin, end or '')
                             for begin, end in lifetime),
                width=ccywidth))


class CommodityPricesReport(base.TableReport):
    """Print all the prices for a particular commodity."""

    names = ['prices']
    default_format = 'text'

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-c', '--commodity', '--currency',
                            action='store', default=None,
                            help="The commodity pair to display.")

    def get_date_rates(self, entries):
        if not self.args.commodity:
            self.parser.error("Commodity pair must be specified (in BASE/QUOTE format)")
        self.args.commodity = ''.join(self.args.commodity.split())
        if not re.match('{ccy}/{ccy}$'.format(ccy=amount.CURRENCY_RE),
                        self.args.commodity):
            self.parser.error(('Invalid commodity pair "{}"; '
                               'must be in BASE/QUOTE format').format(self.args.commodity))
        price_map = prices.build_price_map(entries)
        try:
            date_rates = prices.get_all_prices(price_map, self.args.commodity)
        except KeyError:
            self.parser.error(
                "Commodity not present in database: {}".format(self.args.commodity))
        return date_rates

    def generate_table(self, entries, errors, options_map):
        date_rates = self.get_date_rates(entries)
        return table.create_table(date_rates,
                                  [(0, "Date", datetime.date.isoformat),
                                   (1, "Price", '{:.5f}'.format)])

    def render_htmldiv(self, entries, errors, options_map, file):
        date_rates = self.get_date_rates(entries)
        dates, rates = zip(*date_rates)
        scripts = gviz.gviz_timeline(dates,
                                     {'rates': rates, 'rates2': rates},
                                     css_id='chart')

        file.write('<div id="prices">\n')
        super().render_htmldiv(entries, errors, options_map, file)
        file.write('<scripts>\n')
        file.write(scripts)
        file.write('</scripts>\n')
        file.write('<div id="chart" style="height: 512px"></div>\n')
        file.write('</div>\n') # prices



class PricesReport(base.Report):
    """Print out the unnormalized price entries that we input.
    Unnormalized means that we may render both (base,quote) and (quote,base).
    This can be used to rebuild a prices database without having to share the
    entire ledger file.

    Note: this type of report should be removed once we have filtering on
    directive type, this is simply the 'print' report with type:price. Maybe
    rename the 'pricedb' report to just 'prices' for simplicity's sake.
    """

    names = ['all_prices']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        price_entries = [entry
                         for entry in entries
                         if isinstance(entry, data.Price)]
        dcontext = options_map['dcontext']
        printer.print_entries(price_entries, dcontext, file=file)


class PriceDBReport(base.Report):
    """Print out the normalized price entries from the price db.
    Normalized means that we print prices in the most common (base, quote) order.
    This can be used to rebuild a prices database without having to share the
    entire ledger file.

    Only the forward prices are printed; which (base, quote) pair is selected is
    selected based on the most common occurrence between (base, quote) and
    (quote, base). This is done in the price map.
    """

    names = ['pricedb', 'pricesdb', 'prices_db']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        dcontext = options_map['dcontext']
        price_map = prices.build_price_map(entries)
        meta = data.new_metadata('<report_prices_db>', 0)
        for base_quote in price_map.forward_pairs:
            price_list = price_map[base_quote]
            base, quote = base_quote
            for date, price in price_list:
                entry = data.Price(meta, date, base, amount.Amount(price, quote))
                file.write(printer.format_entry(entry, dcontext))
            file.write('\n')


# Note: This should use the same routines as in beancount.ops.find_prices.
class TickerReport(base.TableReport):
    """Print a parseable mapping of (base, quote, ticker, name) for all commodities."""

    names = ['tickers', 'symbols']

    def generate_table(self, entries, errors, options_map):
        commodities = getters.get_commodity_directives(entries)
        ticker_info = getters.get_values_meta(commodities, 'name', 'ticker', 'quote')

        price_rows = [
            (currency, cost_currency, ticker, name)
            for currency, (name, ticker, cost_currency) in sorted(ticker_info.items())
            if ticker]

        return table.create_table(price_rows,
                                  [(0, "Currency"),
                                   (1, "Cost-Currency"),
                                   (2, "Symbol"),
                                   (3, "Name")])


__reports__ = [
    CommoditiesReport,
    CommodityLifetimes,
    CommodityPricesReport,
    PricesReport,
    PriceDBReport,
    TickerReport,
    ]
