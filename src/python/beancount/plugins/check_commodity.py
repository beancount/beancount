"""A plugin that verifies that all seen commodities have a Commodity directive.

This is useful if you're a bit pedantic and like to make sure that you're
declared attributes for each of the commodities you use. It's useful if you use
the portfolio export, for example.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import data
from beancount.core import getters

__plugins__ = ('validate_commodity_directives',)


CheckCommodityError = collections.namedtuple('CheckCommodityError', 'source message entry')


def validate_commodity_directives(entries, options_map):
    """Find all commodities used and ensure they have a corresponding Commodity directive.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    commodities_used = options_map['commodities']
    errors = []

    meta = data.new_metadata('<check_commodity>', 0)

    commodity_map = getters.get_commodity_map(entries, create_missing=False)
    for currency in commodities_used:
        commodity_entry = commodity_map.get(currency, None)
        if commodity_entry is None:
            errors.append(
                CheckCommodityError(
                    meta,
                    "Missing Commodity directive for '{}'".format(currency),
                    None))

    return entries, errors
