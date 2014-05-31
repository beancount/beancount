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
from beancount.reports import table
from beancount.utils import file_utils


def main():
    import argparse
    optparser = argparse.ArgumentParser(__doc__)
    optparser.add_argument('filename', help='Filename.')

    optparser.add_argument('-a', '--aggregated', '--by-currency',
                           action='store_true',
                           help="Aggregate holdings by currency.")

    # This is useful to share with other people the composition of your
    # portfolio without sharing the absolute amounts.
    optparser.add_argument('-r', '--relative', '--public',
                           action='store_true',
                           help="Only render relative amounts, not absolute.")

    optparser.add_argument('-c', '--currency', action='store',
                           help="Target common currency to convert to.")

    optparser.add_argument('-f', '--format', default=None,
                           choices=['txt', 'csv', 'html'],
                           help="Output format.")

    optparser.add_argument('-o', '--output', action='store',
                           help="Output filename. If not specified, output goes to stdout.")

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
    if opts.currency:
        holdings_list = convert_to_unified_currency(price_map, opts.currency, holdings_list)

    # Create the table report.
    table_ = table.create_table(holdings_list, field_spec)
    table.render_table(table_, outfile, opts.format)



## FIXME: Write an automated test for this, with all the possible combinations of options.

## FIXME: Does render_table support offsets for rendering regular tuples? It really should.

## FIXME: If you value to a currency + relative, it should result in a single total % amount of 100%.


## FIXME: Accept the name of a report from here directly, conver this to
## bean-query right away, move the reporting code to beancount.reports.holdings.

#    holdings
#    holdings_aggregated
#    holdings_aggregated:USD
#    holdings_aggregated:CAD
#    holdings_relative


def convert_to_unified_currency(price_map, currency, holdings_list):
    """Convert the given list of holdings's market value  to a common currency.

    Args:
      price_map: A price-map, as built by prices.build_price_map().
      currency: The target common currency to convert amounts to.
      holdings_list: A list of holdings.Holding instances.
    Returns:
      A modified list of holdings, with the 'extra' field set to the value in
      'currency', or None, if it was not possible to convert.
    """
    # Convert the amounts to a common currency.
    price_converter = functools.partial(prices.convert_amount, price_map, currency)
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
    return new_holdings


if __name__ == '__main__':
    main()
