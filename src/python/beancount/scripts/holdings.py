"""Print out a list of current holdings, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import functools
import sys

from beancount import load
from beancount.parser import options
from beancount.core import amount
from beancount.ops import prices
from beancount.ops import holdings
from beancount.ops import summarize
from beancount.reports import table
from beancount.utils import file_utils


def main():
    import argparse
    optparser = argparse.ArgumentParser(__doc__)
    optparser.add_argument('filename', help='Filename.')

    optparser.add_argument('-a', '--aggregated', '--by-currency',
                           action='store_true',
                           help="Aggregate by currency.")

    # This is useful to share with other people the composition of your
    # portfolio without sharing the absolute amounts.
    optparser.add_argument('-r', '--relative', '--public',
                           action='store_true',
                           help="Only render relative amounts, not absolute.")

    optparser.add_argument('-f', '--format', default=None,
                           choices=['txt', 'csv', 'html'],
                           help="Output format.")

    optparser.add_argument('-o', '--output', action='store',
                           help="Outpuf filename. Default goes to stdout.")

    opts = optparser.parse_args()

    outfile = open(opts.output, 'w') if opts.output else sys.stdout
    opts.format = opts.format or file_utils.guess_file_format(opts.output)

    # Parse the input file.
    entries, errors, options_map = load(opts.filename, quiet=True)
    account_types = options.get_account_types(options_map)

    # Get the aggregate sum of holdings.
    price_map = prices.build_price_map(entries)
    holdings_list = holdings.get_final_holdings(entries,
                                                (account_types.assets,
                                                 account_types.liabilities),
                                                price_map)

    # Aggregate the holdings by (base, quote) pairs if requested.
    if opts.aggregated:
        holdings_list = holdings.aggregate_by_base_quote(holdings_list)

    if not holdings_list:
        return

    # Decide what fields we will print out.
    specs = {
        'account'       : ('account', ),
        'number'        : ('number', "Units", '{:,.2f}'.format),
        'currency'      : ('currency', ),
        'cost_currency' : ('cost_currency', ),
        'cost_number'   : ('cost_number', 'Average Cost', '{:,.2f}'.format),
        'price_number'  : ('price_number', 'Price', '{:,.2f}'.format),
        'book_value'    : ('book_value', 'Book Value', '{:,.2f}'.format),
        'market_value'  : ('market_value', 'Market Value', '{:,.2f}'.format),
        'market_value%' : ('market_value', '% of Portfolio', '{:,.1%}'.format),
    }

    if opts.relative:
        # Reduce the holdings to relative (fractional) values.
        holdings_list = holdings.reduce_relative(holdings_list)

        # Note: we do not include the book value in the output because an astute
        # person could combine it with the market value percentage and use that
        # to back out the total value of your portfolio.
        field_spec = [specs[field]
                      for field in ('currency cost_currency cost_number price_number '
                                    'market_value%').split()]
    else:
        field_spec = [specs[field]
                      for field in ('number currency cost_currency cost_number price_number '
                                    'book_value market_value').split()]

    if not opts.aggregated:
        field_spec.insert(0, 'account')


    # FIXME: Insert a new row for each operating currency, valuing each of the
    # commodities in them. Figure this out, not always the case we want this.
    for currency in options_map['operating_currency']:

        # Convert the amounts to a common currency.
        price_converter = functools.partial(convert_amount, price_map, currency)
        new_holdings = []
        for holding in holdings_list:
            converted_amount = price_converter(
                amount.Amount(holding.market_value or holding.number,
                              holding.cost_currency or holding.currency))


            ## FIXME: put the result in an 'extra' member instead and render that.
            ## FIXME: fix the header as well.

            new_holdings.append(holding._replace(market_value=converted_amount.number
                                                 if converted_amount
                                                 else None))
        # Create the table report.
        table_ = table.create_table(new_holdings, field_spec)
        render_table(table_, outfile, opts.format)
    else:
        # Create the table report.
        table_ = table.create_table(holdings_list, field_spec)
        render_table(table_, outfile, opts.format)



def render_table(table_, output, format):
    # Render the table.
    if format == 'txt':
        text = table.table_to_text(table_, "  ", formats={'*': '>', 'account': '<'})
        output.write(text)

    elif format == 'csv':
        table.table_to_csv(table_, file=output)

    elif format == 'html':
        output.write('<html>\n')
        output.write('<body>\n')
        table.table_to_html(table_, file=output)
        output.write('</body>\n')
        output.write('</html>\n')


def convert_amount(price_map, target_currency, amount_):
    """Convert commodities held at a cost that differ from the value currency.

    Args:
      price_map: A price map dict, as created by build_price_map.
      target_currency: A string, the currency to convert to.
      amount_: An Amount instance, the amount to convert from.
    Returns:
      An instance of Amount, or None, if we could not convert it to the target
      currency.
    """
    if amount_.currency != target_currency:
        base_quote = (amount_.currency, target_currency)
        try:
            _, rate = prices.get_latest_price(price_map, base_quote)
            converted_amount = amount.Amount(amount_.number * rate, target_currency)

        except KeyError:
            # If a rate is not found, simply remove the market value.
            converted_amount = None
    else:
        converted_amount = amount_
    return converted_amount


## FIXME: Write an autoamted test for this.


if __name__ == '__main__':
    main()
