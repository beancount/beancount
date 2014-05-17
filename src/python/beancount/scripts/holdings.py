"""Print out a list of current holdings, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import sys

from beancount import load
from beancount.core import account_types
from beancount.parser import options
from beancount.ops import prices
from beancount.ops import holdings
from beancount.ops import summarize
from beancount.reports import table


def main():
    import argparse
    optparser = argparse.ArgumentParser(__doc__)
    optparser.add_argument('filename', help='Filename.')

    optparser.add_argument('-a', '--aggregated', '--by-currency',
                           action='store_true',
                           help="Aggregate by currency.")

    optparser.add_argument('-f', '--format', default='txt',
                           choices=['txt', 'csv', 'html'],
                           help="Output format.")

    opts = optparser.parse_args()

    # Parse the input file.
    entries, errors, options_map = load(opts.filename, quiet=True)

    # Close the books.
    entries = summarize.close(entries, options_map,
                              *options.get_current_accounts(options_map))

    # Get the aggregate sum of holdings.
    price_map = prices.build_price_map(entries)
    holdings_list = holdings.get_final_holdings(entries, price_map)

    # Remove the equity accounts, we only want to list Assets and Liabilities.
    holdings_list = filter(
        lambda holding: not account_types.is_equity_account(holding.account, options_map),
        holdings_list)

    if opts.aggregated:
        holdings_list = holdings.aggregate_by_base_quote(holdings_list)

    if not holdings_list:
        return

    # Create the table report.
    field_spec = [('number', None, '{:,.2f}'.format),
                  'currency',
                  'cost_currency',
                  ('cost_number', 'Average Cost', '{:,.2f}'.format),
                  ('price_number', 'Average Price', '{:,.2f}'.format),
                  ('book_value', None, '{:,.2f}'.format),
                  ('market_value', None, '{:,.2f}'.format)]
    if not opts.aggregated:
        field_spec.insert(0, 'account')
    table_ = table.create_table(holdings_list, field_spec)

    # Render the table.
    if opts.format == 'txt':
        text = table.table_to_text(table_, "  ", formats={'*': '>', 'account': '<'})
        sys.stdout.write(text)

    elif opts.format == 'csv':
        table.table_to_csv(table_, file=sys.stdout)

    elif opts.format == 'html':
        sys.stdout.write('<html>\n')
        sys.stdout.write('<body>\n')
        table.table_to_html(table_, file=sys.stdout)
        sys.stdout.write('</body>\n')
        sys.stdout.write('</html>\n')


if __name__ == '__main__':
    main()
