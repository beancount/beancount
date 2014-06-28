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
        include embedded parameters, such as in 'holdings_aggregated:USD'.
    Returns:
      A callable, that can generate the report. It should accept a list of
      entries and an options map.
    """
    if report_str == 'print':
        return report_print

    elif snooper(re.match('holdings(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings,
                                 snooper.value.group(1))

    elif snooper(re.match('holdings_aggregated(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_aggregated,
                                 snooper.value.group(1))

    elif snooper(re.match('holdings_relative(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_relative,
                                 snooper.value.group(1))

    elif snooper(re.match('holdings_byaccount(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_byaccount,
                                 snooper.value.group(1))

    elif snooper(re.match('currency_exposure$', report_str)):
        return rholdings.report_currency_exposure

    elif snooper(re.match('networth$', report_str)):
        return rholdings.report_networth

    # Note: This will grow in the future to accommodate journals, balsheet, and
    # all the other possible reports that the web interface currently serves.


def report_print(entries, unused_options_map):
    """A simple report type that prints out the entries as parsed.

    Args:
      entries: A list of directives.
      unused_options_map: An options dict, as read by the parser.
    """
    printer.print_entries(entries, sys.stdout)
