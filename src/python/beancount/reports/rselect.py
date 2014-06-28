"""Select a report generator based on an expression.
"""
import functools
import re
import sys

from beancount.utils.snoop import snooper
from beancount.reports import rholdings
from beancount.parser import printer


def get_report_generator(report_str):
    """Given a report name/spec, return a function to generate that report.

    Args:
      report_str: A string, the name of the report to produce. This name may
        include embedded parameters, such as in 'holdings_bycommodity:USD'.
    Returns:
      A callable, that can generate the report. It should accept a list of
      entries and an options map.
    """
    if report_str == 'print':
        return report_print

    elif snooper(re.match('holdings(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings,
                                 snooper.value.group(1))

    elif snooper(re.match('holdings_by(?:commodity|instrument)(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_bycommodity,
                                 snooper.value.group(1))

    elif snooper(re.match('holdings_relative(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_relative,
                                 snooper.value.group(1))

    elif snooper(re.match('holdings_byaccount(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_byaccount,
                                 snooper.value.group(1))

    elif snooper(re.match('holdings_bycurrency(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_bycurrency,
                                 snooper.value.group(1))

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
        ('print', None, None, ['beancount'],
         "Print out the entries."),

        ('holdings', ['currency'], None, ['text'],
         "The full list of holdings for Asset and Liabilities accounts."),

        ('holdings_bycommodity', ['currency'], None, ['text', 'csv', 'html'],
         "A list of holdings aggregated by base/quote commodity."),

        ('holdings_relative', ['currency'], None, ['text', 'csv', 'html'],
         "A list of holdings aggregated by base/quote commodity, "
         "only rendering relative values. This is useful to share with others."),

        ('holdings_byaccount', ['currency'], None, ['text', 'csv', 'html'],
         "A list of holdings aggregated by base/quote commodity, "
         "only rendering relative values. This is useful to share with others "
         "without disclosing the absolute values of your portfolio."),

        ('holdings_bycurrency', [], None, ['text', 'csv', 'html'],
         "A list of holdings aggregated by cost currency."),

        ('networth', [], None, ['text', 'csv', 'html'],
         "A table of networth in each ofthe operating currencies."),

        ]

def report_print(entries, unused_options_map):
    """A simple report type that prints out the entries as parsed.

    Args:
      entries: A list of directives.
      unused_options_map: An options dict, as read by the parser.
    """
    printer.print_entries(entries, sys.stdout)
