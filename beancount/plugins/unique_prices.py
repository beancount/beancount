"""This module adds validation that there is a single price defined per
date and base/quote currencies. If multiple conflicting price values are
declared, an error is generated. Note that multiple price entries with the
same number do not generate an error.

This is meant to be turned on if you want to use a very strict mode for
entering prices, and may not be realistic usage. For example, if you have
(1) a transaction with an implicitly generated price during the day (from
its cost) and (2) a separate explicit price directive that declares a
different price for the day's closing price, this would generate an error.
I'm not certain this will be useful in the long run, so placing it in a
plugin.
"""
__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import data

__plugins__ = ('validate_unique_prices',)


UniquePricesError = collections.namedtuple('UniquePricesError', 'source message entry')


def validate_unique_prices(entries, unused_options_map):
    """Check that there is only a single price per day for a particular base/quote.

    Args:
      entries: A list of directives. We're interested only in the Transaction instances.
      unused_options_map: A parser options dict.
    Returns:
      The list of input entries, and a list of new UniquePricesError instances generated.
    """
    new_entries = []
    errors = []

    prices = collections.defaultdict(list)
    for entry in entries:
        if not isinstance(entry, data.Price):
            continue
        key = (entry.date, entry.currency, entry.amount.currency)
        prices[key].append(entry)

    errors = []
    for price_entries in prices.values():
        if len(price_entries) > 1:
            number_map = {price_entry.amount.number: price_entry
                          for price_entry in price_entries}
            if len(number_map) > 1:
                # Note: This should be a list of entries for better error
                # reporting. (Later.)
                error_entry = next(iter(number_map.values()))
                errors.append(
                    UniquePricesError(error_entry.meta,
                                      "Disagreeing price entries",
                                      price_entries))

    return entries, errors
