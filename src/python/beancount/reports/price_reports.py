"""Miscellaneous report classes.
"""
import datetime
import re

from beancount.reports import report
from beancount.reports import table
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import realization
from beancount.core import amount
from beancount.core import getters
from beancount.core import account_types
from beancount.ops import prices


class PricesReport(report.Report):
    """Print out the unnormalized price entries that we input.
    Unnormalized means that we may render both (base,quote) and (quote,base).
    This can be used to rebuild a prices database without having to share the
    entire ledger file.

    Note: this type of report should be removed once we have filtering on
    directive type, this is simply the 'print' report with type:price. Maybe
    rename the 'pricedb' report to just 'prices' for simplicity's sake.
    """

    names = ['prices']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        price_entries = [entry
                         for entry in entries
                         if isinstance(entry, data.Price)]
        printer.print_entries(price_entries, file=file)


class PriceDBReport(report.Report):
    """Print out the normalized price entries from the price db.
    Normalized means that we print prices in the most common (base, quote) order.
    This can be used to rebuild a prices database without having to share the
    entire ledger file.

    Only the forward prices are printed; which (base, quote) pair is selected is
    selected based on the most common occurrence between (base, quote) and
    (quote, base). This is done in the price map.
    """

    names = ['pricedb', 'prices_db']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        price_map = prices.build_price_map(entries)
        source = data.Source('<report_prices_db>', 0)
        for base_quote in price_map.forward_pairs:
            price_list = price_map[base_quote]
            base, quote = base_quote
            for date, price in price_list:
                entry = data.Price(source, date, base, amount.Amount(price, quote))
                file.write(printer.format_entry(entry))
            file.write('\n')


__reports__ = [
    PricesReport,
    PriceDBReport,
    ]
