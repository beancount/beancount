"""A plugin that issues errors when more than one commodity is used in an account.

For investments or trading accounts, it can make it easier to filter the action
around a single stock by using the name of the stock as the leaf of the account
name.

Notes:

- The plugin will automatically skip accounts that have explicitly declared
  commodities in their Open directive.

- You can also set the metadata "onecommodity: FALSE" on an account's Open
  directive to skip the checks for that account.

- If provided, the configuration should be a regular expression restricting the
  set of accounts to check.

"""

__copyright__ = "Copyright (C) 2014-2020  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import re

from beancount.core import data

__plugins__ = ('validate_one_commodity',)


OneCommodityError = collections.namedtuple('OneCommodityError', 'source message entry')


def validate_one_commodity(entries, unused_options_map, config=None):
    """Check that each account has units in only a single commodity.

    This is an extra constraint that you may want to apply optionally, despite
    Beancount's ability to support inventories and aggregations with more than
    one commodity. I believe this also matches GnuCash's model, where each
    account has a single commodity attached to it.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
      config: The plugin configuration string, a regular expression to match
        against the subset of accounts to check.
    Returns:
      A list of new errors, if any were found.
    """
    accounts_re = re.compile(config) if config else None

    # Mappings of account name to lists of currencies for each units and cost.
    units_map = collections.defaultdict(set)
    cost_map = collections.defaultdict(set)

    # Mappings to use just for getting a relevant source.
    units_source_map = {}
    cost_source_map = {}

    # Gather the set of accounts to skip from the Open directives.
    skip_accounts = set()
    for entry in entries:
        if not isinstance(entry, data.Open):
            continue
        if (not entry.meta.get("onecommodity", True) or
            (accounts_re and not accounts_re.match(entry.account)) or
            (entry.currencies and len(entry.currencies) > 1)):
            skip_accounts.add(entry.account)

    # Accumulate all the commodities used.
    for entry in entries:
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                if posting.account in skip_accounts:
                    continue

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
            if entry.account in skip_accounts:
                continue

            units_map[entry.account].add(entry.amount.currency)
            if len(units_map[entry.account]) > 1:
                units_source_map[entry.account] = entry

        elif isinstance(entry, data.Open):
            if entry.currencies and len(entry.currencies) > 1:
                skip_accounts.add(entry.account)

    # Check units.
    errors = []
    for account, currencies in units_map.items():
        if account in skip_accounts:
            continue
        if len(currencies) > 1:
            errors.append(OneCommodityError(
                units_source_map[account].meta,
                "More than one currency in account '{}': {}".format(
                    account, ','.join(currencies)),
                None))

    # Check costs.
    for account, currencies in cost_map.items():
        if account in skip_accounts:
            continue
        if len(currencies) > 1:
            errors.append(OneCommodityError(
                cost_source_map[account].meta,
                "More than one cost currency in account '{}': {}".format(
                    account, ','.join(currencies)),
                None))

    return entries, errors
