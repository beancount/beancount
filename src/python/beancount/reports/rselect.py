"""Select a report generator based on an expression.
"""
import functools
import re

from beancount.utils.snoop import snooper
from beancount.reports import rholdings


def get_report_generator(report_str):
    """Given a report name/spec, return a function to generate that report.

    Args:
      report_str: A string, the name of the report to produce. This name may
        include embedded parameters, such as in 'holdings_aggregated:USD'.
    Returns:
      A callable, that can generate the report.
    """
    if report_str == 'holdings':
        return rholdings.report_holdings
    elif snooper(re.match('holdings_aggregated(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_aggregated,
                                 snooper.value.group(1))
    elif snooper(re.match('holdings_relative(?::([A-Z]+))?$', report_str)):
        return functools.partial(rholdings.report_holdings_relative,
                                 snooper.value.group(1))

    # Note: This will grow in the future to accommodate journals, balsheet, and
    # all the other possible reports that the web interface currently serves.
