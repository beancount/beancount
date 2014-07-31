"""Select a report generator based on an expression.
"""
import functools
import re
import io

from beancount.utils.snoop import snooper
from beancount.reports import report
from beancount.reports import rholdings
from beancount.reports import table
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import realization
from beancount.core import amount
from beancount.core import getters
from beancount.core import account_types
from beancount.ops import prices


def get_report_generator(report_str):
    """Given a report name/spec, return a function to generate that report.

    Args:
      report_str: A string, the name of the report to produce. This name may
        include embedded parameters, such as in 'holdings_bycommodity:USD'.
    Returns:
      A callable, that can generate the report. It should accept a list of
      entries and an options map.
    """
    currency_re = '(?::([A-Z]+)(?::(%))?)?'

    if report_str in ('check', 'validate', None):
        return report_validate

    elif report_str == 'print':
        return report_print

    elif report_str == 'prices':
        return report_prices

    elif report_str == 'prices_db':
        return report_prices_db

    elif report_str == 'accounts':
        return report_accounts

    elif report_str == 'events':
        return report_events

    elif snooper(re.match('(?:trial|bal|balances|ledger)(?::(.*))?$', report_str)):
        return functools.partial(report_trial, snooper.value.group(1))

    elif snooper(re.match('holdings{}$'.format(currency_re),
                          report_str)):
        return functools.partial(rholdings.report_holdings,
                                 snooper.value.group(1),
                                 bool(snooper.value.group(2)))

    elif snooper(re.match('holdings_by(?:commodity|instrument){}$'.format(currency_re),
                          report_str)):
        return functools.partial(rholdings.report_holdings_bycommodity,
                                 snooper.value.group(1),
                                 bool(snooper.value.group(2)))

    elif snooper(re.match('holdings_byaccount{}$'.format(currency_re),
                          report_str)):
        return functools.partial(rholdings.report_holdings_byaccount,
                                 snooper.value.group(1),
                                 bool(snooper.value.group(2)))

    elif snooper(re.match('holdings_byaccount_shallow{}$'.format(currency_re),
                          report_str)):
        return functools.partial(rholdings.report_holdings_byaccount_shallow,
                                 snooper.value.group(1),
                                 bool(snooper.value.group(2)))

    elif snooper(re.match('holdings_by(?:currency|cost){}$'.format(currency_re),
                          report_str)):
        return functools.partial(rholdings.report_holdings_bycurrency,
                                 snooper.value.group(1),
                                 bool(snooper.value.group(2)))

    elif snooper(re.match('networth$', report_str)):
        return rholdings.report_networth

    # Note: This will grow in the future to accommodate journals, balsheet, and
    # all the other possible reports that the web interface currently serves.


def get_report_types():
    """Return a list of the available reports and the formats they support.

    Returns:
      A list of (report-name, report-args, report-class, formats, description).
    """
    return [
        ('trial', ['regexp'], None,
         ['text'],
         "Print out the trial balance of all accounts."),

        ('holdings', ['currency', 'relative'], None,
         ['text'],
         "The full list of holdings for Asset and Liabilities accounts."),

        ('holdings_bycommodity', ['currency', 'relative'], None,
         ['text', 'csv', 'html'],
         "A list of holdings aggregated by base/quote commodity."),

        ('holdings_byaccount', ['currency', 'relative'], None,
         ['text', 'csv', 'html'],
         "A list of holdings aggregated by account."),

        ('holdings_byaccount_shallow', ['currency', 'relative'], None,
         ['text', 'csv', 'html'],
         "A list of holdings aggregated by account, at no more than a depth of 3."),

        ('holdings_bycurrency', ['currency', 'relative'], None,
         ['text', 'csv', 'html'],
         "A list of holdings aggregated by cost currency."),

        ('networth', [], None,
         ['text', 'csv', 'html'],
         "A table of networth in each ofthe operating currencies."),

        ]


class ErrorReport(report.Report):
    """Report the errors."""

    names = ['check', 'validate', 'errors']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        printer.print_errors(errors, file=file)


class PrintReport(report.Report):
    """Print out the entries."""

    names = ['print']
    default_format = 'beancount'

    def render_beancount(self, entries, errors, options_map, file):
        printer.print_entries(entries, file=file)


class PricesReport(report.Report):
    """Print out the unnormalized price entries that we input.
    Unnormalized means that we may render both (base,quote) and (quote,base).
    This can be used to rebuild a prices database without having to share the
    entire ledger file."""

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


class BalancesReport(report.Report):
    """Print out the trial balance of accounts matching an expression."""

    names = ['balances', 'bal', 'trial', 'ledger']
    default_format = 'text'

    def add_args(self, parser):
        parser.add_argument('-e', '--expression', action='store', default=None,
                            help="Filter expression for which account balances to display.")

    def render_text(self, entries, errors, options_map, file):
        real_accounts = realization.realize(entries)
        if self.opts.expression:
            regexp = re.compile(self.opts.expression)
            real_accounts = realization.filter(
                real_accounts,
                lambda real_account: regexp.search(real_account.account))
        if real_accounts:
            realization.dump_balances(real_accounts, file=file)





REPORTS = [
    ErrorReport,
    PrintReport,
    PricesReport,
    PriceDBReport,
    BalancesReport,
    ]

def get_report(report_name):
    """Instantiate a report class.

    Args:
      report_name: A string, the name of the report to generate.
    Returns:
      An instance of the report generator.
    """
    for report in REPORTS:
        if report_name in report.names:
            return report()


















def report_accounts(entries, options_map):
    """Produce a list of all the accounts.

    Args:
      entries: A list of directives.
      unused_options_map: An options dict, as read by the parser.
    Returns:
      A string, the text to print.
    """
    if not entries:
        return

    open_close = getters.get_account_open_close(entries)

    # Render to stdout.
    maxlen = max(len(account) for account in open_close)
    sortkey_fun = account_types.get_account_sort_function(
        options.get_account_types(options_map))
    oss = io.StringIO()
    for account, (open, close) in sorted(open_close.items(),
                                         key=lambda entry: sortkey_fun(entry[0])):
        open_date = open.date if open else ''
        close_date = close.date if close else ''
        oss.write('{:{len}}  {}  {}\n'.format(account, open_date, close_date, len=maxlen))
    return oss.getvalue()


def report_events(entries, options_map):
    """Produce a table of the latest values of all event types.

    Args:
      entries: A list of directives.
      unused_options_map: An options dict, as read by the parser.
    Returns:
      A string, the text to print.
    """
    events = {}
    for entry in entries:
        if isinstance(entry, data.Event):
            events[entry.type] = entry.description

    return table.create_table([(type_, description)
                               for type_, description in sorted(events.items())],
                              [(0, "Type"), (1, "Description")])
