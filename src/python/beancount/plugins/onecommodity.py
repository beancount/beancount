"""A plugin that issues errors when more than one commodity is used in an account.
"""
import collections

from beancount.core import data
from beancount.core import getters

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
                lot = posting.position.lot
                units_map[posting.account].add(lot.currency)
                if len(units_map[posting.account]) > 1:
                    units_source_map[posting.account] = entry

                if lot.cost:
                    cost_map[posting.account].add(lot.cost.currency)
                    if len(cost_map[posting.account]) > 1:
                        units_source_map[posting.account] = entry

    # Check units.
    errors = []
    for account, currencies in units_map.items():
        if len(currencies) > 1:
            errors.append(OneCommodityError(
                units_source_map[account].source,
                "More than one currency in account '{}': {}".format(
                    account, ','.join(currencies)),
                None))

    # Check costs.
    for account, currencies in cost_map.items():
        if len(currencies) > 1:
            errors.append(OneCommodityError(
                cost_source_map[account].source,
                "More than one cost currency in account '{}': {}".format(
                    account, ','.join(currencies)),
                None))

    return entries, errors
