"""Print out a list of current positions, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import re

from beancount import load
from beancount import utils
from beancount.core import data
from beancount.ops import prices


def main():
    import argparse
    optparser = argparse.ArgumentParser(__doc__)
    optparser.add_argument('filename', help='Filename.')
    opts = optparser.parse_args()

    # Parse the input file.
    entries, errors, options = load(opts.filename, quiet=True)

    dataframe = prices.get_positions_as_dataframe(entries)

    byinst = dataframe.groupby(['account', 'currency', 'cost_currency'])
    byinst_agg = byinst['number', 'market_value'].sum()
    byinst_agg['avg_cost'] = byinst['cost_number'].mean()
    byinst_agg['price_number'] = byinst['price_number'].mean()
    byinst_agg = byinst_agg.sort('market_value', ascending=False)

    # Compute the total value.
    total_value = byinst_agg['market_value'].sum()

    byinst_agg['percent'] = byinst_agg['market_value'] / total_value

    percent_only = byinst_agg[['percent']].sum(level=[1,2]).sort('percent', ascending=False)
    
    print(percent_only.to_string(formatters={'percent': '{:.1%}'.format}))



if __name__ == '__main__':
    main()
