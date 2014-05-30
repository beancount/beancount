"""Print out a list of current holdings, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import sys

from beancount import load
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

    # This is useful to share with other people the composition of your
    # portfolio without sharing the absolute amounts.
    optparser.add_argument('-r', '--relative', '--public',
                           action='store_true',
                           help="Only render relative amounts, not absolute.")

    optparser.add_argument('-f', '--format', default='txt',
                           choices=['txt', 'csv', 'html'],
                           help="Output format.")

    opts = optparser.parse_args()

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
    # commodities in them.
    opts.value_currency = 'USD'

    # FIXME: Move this to holdings.
    if opts.value_currency:
        new_holdings = []
        for holding in holdings_list:
            # FIXME: the following logic should be implemented in b.opts.prices.
            try:
                # Convert commodities held at a cost that differ from the value
                # currency.
                if holding.cost_currency:
                    if holding.cost_currency != opts.value_currency:
                        base_quote = (holding.cost_currency, opts.value_currency)
                        _, price = prices.get_latest_price(price_map, base_quote)
                        holding = holding._replace(
                            market_value=holding.market_value * price)

                # Convert commodities not held at cost (usually currencies).
                elif holding.currency != opts.value_currency:
                    base_quote = (holding.currency, opts.value_currency)
                    _, price = prices.get_latest_price(price_map, base_quote)
                    holding = holding._replace(market_value=holding.number * price)

            except KeyError:
                # If a rate is not found, simply remove the market value.
                holding = holding._replace(market_value=None)

            new_holdings.append(holding)
        holdings_list = new_holdings

    #        def convert_to(price_map, target_currency, units, base_quote):
    #            """Convert the number of units in base_quote to currency, if possible.
    #
    #
    #            Args:
    #              FIXME: TODO
    #
    #                The 'quote' currency in 'base_quote' may be None, in which case the 'base'
    #                is attempted to be converted. This is the case for cash, for instance.
    #
    #            Returns:
    #              The converted amount in 'currency' units, or None if it was not possible
    #              to compute it.
    #            """
    #
    #            base_currency, quote_currency = base_quote
    #            try:
    #                # Convert commodities held at a cost whose that differ from the value
    #                # currency.
    #                if quote_currency:
    #                    if quote_currency != target_currency:
    #                        base_quote = (quote_currency, target_currency)
    #                        _, price = prices.get_latest_price(price_map, base_quote)
    #                        return units * price
    #
    #                # Convert commodities not held at cost (usually currencies).
    #                elif base_currency != target_currency:
    #                    base_quote = (base_currency, target_currency)
    #                    _, price = prices.get_latest_price(price_map, base_quote)
    #                    return units * price
    #
    #            except KeyError:
    #                # If a rate is not found, simply remove the market value.
    #                return None



    # Create the table report.
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
