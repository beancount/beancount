"""Select a report generator based on an expression.
"""
import functools
import re
import io

from beancount.utils.snoop import snooper
from beancount.reports import rholdings
from beancount.parser import printer
from beancount.core import data


def get_report_generator(report_str, output_format):
    """Given a report name/spec, return a function to generate that report.

    Args:
      report_str: A string, the name of the report to produce. This name may
        include embedded parameters, such as in 'holdings_bycommodity:USD'.
      output_format: A string, the name of the desired output format.
    Returns:
      A callable, that can generate the report. It should accept a list of
      entries and an options map.
    """
    currency_re = '(?::([A-Z]+)(?::(%))?)?'

    if report_str == 'print':
        return report_print

    elif report_str == 'print_prices':
        return report_print_prices

    elif report_str == 'print_holdings':
        return rholdings.report_holdings_print

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
      A list of (report-name, report-args, report-class, formats, description)..
    """
    return [
        ('print', None, None,
         ['beancount'],
         "Print out the entries."),

        ('print_prices', None, None,
         ['beancount'],
         "Print out just the price entries. This can be used to rebuild a prices "
         "database without having to share the entire ledger file."),

        ('print_holdings', None, None,
         ['beancount'],
         "The full list of holdings and latest prices, in Beancount format."),

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


def report_print(entries, unused_options_map):
    """A report type that prints out the entries as parsed.

    Args:
      entries: A list of directives.
      unused_options_map: An options dict, as read by the parser.
    """
    oss = io.StringIO()
    printer.print_entries(entries, oss)
    return oss.getvalue()


def report_print_prices(entries, unused_options_map):
    """A report type that prints out just the price entries.

    Note: this is a temporary solution, until we have proper filtering, and when
    we do, we can just filter by 'type:price' and use 'print' output format,
    it'll be the same.

    Args:
      entries: A list of directives.
      unused_options_map: An options dict, as read by the parser.

    """
    price_entries = [entry
                     for entry in entries
                     if isinstance(entry, data.Price)]
    oss = io.StringIO()
    printer.print_entries(price_entries, oss)
    return oss.getvalue()
