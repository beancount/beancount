"""A plugin that issues errors when more than one commodity is used in an account.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import data

__plugins__ = ('validate_one_commodity',)


OneCommodityError = collections.namedtuple('OneCommodityError', 'source message entry')


def validate_one_commodity(entries, unused_options_map):
    """Check that each account has units in only a single commodity.

    This is an extra constraint that you may want to apply optionally, despite
    Beancount's ability to support inventories and aggregations with more than
    one commodity. I believe this also matches GnuCash's model, where each
    account has a single commodity attached to it.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    # Mappings of account name to lists of currencies for each units and cost.
    units_map = collections.defaultdict(set)
    cost_map = collections.defaultdict(set)

    # Mappings to use just for getting a relevant source.
    units_source_map = {}
    cost_source_map = {}

    # Accumulate all the commodities used.
    for entry in entries:
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                units = posting.units
                units_map[posting.account].add(units.currency)
                if len(units_map[posting.account]) > 1:
                    units_source_map[posting.account] = entry

                cost = posting.cost
                if cost:
                    cost_map[posting.account].add(cost.currency)
                    if len(cost_map[posting.account]) > 1:
                        units_source_map[posting.account] = entry

        elif isinstance(entry, data.Balance):
            units_map[entry.account].add(entry.amount.currency)
            if len(units_map[entry.account]) > 1:
                units_source_map[entry.account] = entry

    # Check units.
    errors = []
    for account, currencies in units_map.items():
        if len(currencies) > 1:
            errors.append(OneCommodityError(
                units_source_map[account].meta,
                "More than one currency in account '{}': {}".format(
                    account, ','.join(currencies)),
                None))

    # Check costs.
    for account, currencies in cost_map.items():
        if len(currencies) > 1:
            errors.append(OneCommodityError(
                cost_source_map[account].meta,
                "More than one cost currency in account '{}': {}".format(
                    account, ','.join(currencies)),
                None))

    return entries, errors
