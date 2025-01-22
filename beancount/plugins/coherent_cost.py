"""This plugin validates that currencies held at cost aren't ever converted at
price and vice-versa. This is usually the case, and using it will prevent users
from making the mistake of selling a lot without specifying it via its cost
basis.
"""

__copyright__ = "Copyright (C) 2016-2017, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import data

__plugins__ = ("validate_coherent_cost",)


CoherentCostError = collections.namedtuple("CoherentCostError", "source message entry")


def validate_coherent_cost(entries, unused_options_map):
    """Check that all currencies are either used at cost or not at all, but never both.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []

    with_cost = {}
    without_cost = {}
    for entry in data.filter_txns(entries):
        for posting in entry.postings:
            target_set = without_cost if posting.cost is None else with_cost
            currency = posting.units.currency
            target_set.setdefault(currency, entry)

    for currency in set(with_cost) & set(without_cost):
        errors.append(
            CoherentCostError(
                without_cost[currency].meta,
                "Currency '{}' is used both with and without cost".format(currency),
                with_cost[currency],
            )
        )
        # Note: We really ought to include both of the first transactions here.

    return entries, errors
