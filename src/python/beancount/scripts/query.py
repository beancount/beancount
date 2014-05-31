"""Print out a list of current holdings, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import functools
import re
import sys

from beancount import load
from beancount.parser import options
from beancount.core import amount
from beancount.ops import prices
from beancount.ops import holdings
from beancount.reports import table
from beancount.utils import file_utils
from beancount.utils.snoop import snooper


def main():
    import argparse
    parser = argparse.ArgumentParser(__doc__)

    parser.add_argument('filename', help='Filename.')
    parser.add_argument('report', help='Name of the desired report.')

    parser.add_argument('-f', '--format', default=None,
                           choices=['txt', 'csv', 'html'],
                           help="Output format.")

    parser.add_argument('-o', '--output', action='store',
                           help="Output filename. If not specified, output goes to stdout.")

    opts = parser.parse_args()

    outfile = open(opts.output, 'w') if opts.output else sys.stdout
    opts.format = opts.format or file_utils.guess_file_format(opts.output)

    # Parse the input file.
    entries, errors, options_map = load(opts.filename, quiet=True)

    # Dispatch on which report to generate.
    report_function = get_report_generator(opts.report)
    if report_function is None:
        parser.error("Unknown report.")

    # Create holdings list.
    table_ = report_function(entries, options_map)

    # Create the table report.
    table.render_table(table_, outfile, opts.format)



## FIXME: Eventually move these to beancount.reports.holdings. The above should
## begin to include other types of reports, prices, accounts, balsheet, etc.

def get_report_generator(report_str):
    """Given a report name/spec, return a function to generate that report.

    Args:
      report_str: A string, the name of the report to produce. This name may
        include embedded parameters, such as in 'holdings_aggregated:USD'.
    Returns:
      A callable, that can generate the report.
    """
    if report_str == 'holdings':
        return report_holdings
    elif snooper(re.match('holdings_aggregated(?::([A-Z]+))?$', report_str)):
        return functools.partial(report_holdings_aggregated, snooper.value.group(1))
    elif snooper(re.match('holdings_relative(?::([A-Z]+))?$', report_str)):
        return functools.partial(report_holdings_relative, snooper.value.group(1))


def report_holdings(entries, options_map):
    """Generate a detailed list of all holdings by account.

    Args:
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A list of Holding instances.
    """
    price_map = prices.build_price_map(entries)
    account_types = options.get_account_types(options_map)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)
    field_spec = [
        ('account', ),
        ('number', "Units", '{:,.2f}'.format),
        ('currency', ),
        ('cost_currency', ),
        ('cost_number', 'Average Cost', '{:,.2f}'.format),
        ('price_number', 'Price', '{:,.2f}'.format),
        ('book_value', 'Book Value', '{:,.2f}'.format),
        ('market_value', 'Market Value', '{:,.2f}'.format),
    ]
    return table.create_table(holdings_list, field_spec)


def report_holdings_aggregated(currency, entries, options_map):
    """Generate a detailed list of all holdings by account.

    Args:
      currency: A string, a currency to convert to. If left to None, no
        conversion is carried out.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A list of Holding instances.
    """
    price_map = prices.build_price_map(entries)
    account_types = options.get_account_types(options_map)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)

    # Aggregate the holdings.
    holdings_list = holdings.aggregate_by_base_quote(holdings_list)

    # Convert holdings to a unified currency.
    if currency:
        holdings_list = convert_to_unified_currency(price_map, currency, holdings_list)

    field_spec = [
        ('number', "Units", '{:,.2f}'.format),
        ('currency', ),
        ('cost_currency', ),
        ('cost_number', 'Average Cost', '{:,.2f}'.format),
        ('price_number', 'Price', '{:,.2f}'.format),
        ('book_value', 'Book Value', '{:,.2f}'.format),
        ('market_value', 'Market Value', '{:,.2f}'.format),
    ]
    return table.create_table(holdings_list, field_spec)


def report_holdings_relative(currency, entries, options_map):
    """Generate a list of all holdings aggregated by instrument.

    Args:
      currency: A string, a currency to convert to. If left to None, no
        conversion is carried out.
      entries: A list of directives.
      options_map: A dict of parsed options.
    Returns:
      A list of Holding instances.
    """
    price_map = prices.build_price_map(entries)
    account_types = options.get_account_types(options_map)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)

    # Aggregate the holdings.
    holdings_list = holdings.aggregate_by_base_quote(holdings_list)

    # Convert holdings to a unified currency.
    if currency:
        holdings_list = convert_to_unified_currency(price_map, currency, holdings_list)

    # Reduce the holdings to relative (fractional) values.
    holdings_list = holdings.reduce_relative(holdings_list)  ## FIXME: add currency constraint here

    field_spec = [
        ('currency', ),
        ('cost_currency', ),
        ('cost_number', 'Average Cost', '{:,.2f}'.format),
        ('price_number', 'Price', '{:,.2f}'.format),
        ('market_value', '% of Portfolio', '{:,.1%}'.format),
    ]
    return table.create_table(holdings_list, field_spec)


if __name__ == '__main__':
    main()
