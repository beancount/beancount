"""A plugin that verifies that all seen commodities have a Commodity directive.

This is useful if you're a bit pedantic and like to make sure that you're
declared attributes for each of the commodities you use. It's useful if you use
the portfolio export, for example.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import re

from beancount.core import data
from beancount.core.amount import CURRENCY_RE

__plugins__ = ('validate_commodity_directives',)


CheckCommodityError = collections.namedtuple('CheckCommodityError', 'source message entry')


def get_commodity_map_ex(entries, metadata=False):

    ignore = set(['filename', 'lineno', '__automatic__'])
    regexp = re.compile(CURRENCY_RE)

    def currencies_in_meta(entry):
        if entry.meta is not None:
            for key, value in entry.meta.items():
                if isinstance(value, str) and key not in ignore:
                    if regexp.fullmatch(value):
                        yield value

    commodities_map = {}

    for entry in entries:
        if isinstance(entry, data.Commodity):
            commodities_map[entry.currency] = entry

        elif isinstance(entry, data.Open):
            if entry.currencies:
                for currency in entry.currencies:
                    commodities_map.setdefault(currency, None)

        elif isinstance(entry, data.Transaction):
            for posting in entry.postings:

                # Main currency.
                units = posting.units
                commodities_map.setdefault(units.currency, None)

                # Currency in cost.
                cost = posting.cost
                if cost:
                    commodities_map.setdefault(cost.currency, None)

                # Currency in price.
                price = posting.price
                if price:
                    commodities_map.setdefault(price.currency, None)

                # Currency in posting metadata.
                if metadata:
                    for currency in currencies_in_meta(posting):
                        commodities_map.setdefault(currency, None)

        elif isinstance(entry, data.Balance):
            commodities_map.setdefault(entry.amount.currency, None)

        elif isinstance(entry, data.Price):
            commodities_map.setdefault(entry.currency, None)
            commodities_map.setdefault(entry.amount.currency, None)

        # Entry metadata.
        if metadata:
            for currency in currencies_in_meta(entry):
                commodities_map.setdefault(currency, None)

    return commodities_map


def validate_commodity_directives(entries, options_map):
    """Find all commodities used and ensure they have a corresponding Commodity directive.

    Args:
      entries: A list of directives.
      options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []

    meta = data.new_metadata('<check_commodity>', 0)

    # TODO(dnicolodi) Unfortunately detecting commodities in metadata
    # values may result in false positives: common used string are
    # matched by the regular expression. Revisit this when commodities
    # will be represented with their own type.
    commodity_map = get_commodity_map_ex(entries, metadata=False)

    for commodity, commodity_entry in commodity_map.items():
        if commodity_entry is None:
            errors.append(
                CheckCommodityError(
                    meta,
                    "Missing Commodity directive for '{}'".format(commodity),
                    None))

    return entries, errors
