"""A plugin that issues errors when amounts are posted to non-leaf accounts,
that is, accounts with child accounts.

This is an extra constraint that you may want to apply optionally. If you
install this plugin, it will issue errors for all accounts that have
postings to non-leaf accounts. Some users may want to disallow this and
enforce that only leaf accounts may have postings on them.
"""

__copyright__ = "Copyright (C) 2014-2017, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import data
from beancount.core import getters
from beancount.core import realization

__plugins__ = ("validate_leaf_only",)


LeafOnlyError = collections.namedtuple("LeafOnlyError", "source message entry")


def validate_leaf_only(entries, unused_options_map):
    """Check for non-leaf accounts that have postings on them.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    real_root = realization.realize(entries, compute_balance=False)

    default_meta = data.new_metadata("<leafonly>", 0)
    open_close_map = None  # Lazily computed.
    errors = []
    for real_account in realization.iter_children(real_root):
        if len(real_account) == 0 or not real_account.txn_postings:
            continue
        allowed_types = (data.Open, data.Balance)
        postings = real_account.txn_postings
        if all(isinstance(post, allowed_types) for post in postings):
            continue
        if open_close_map is None:
            open_close_map = getters.get_account_open_close(entries)
        try:
            open_entry = open_close_map[real_account.account][0]
        except KeyError:
            open_entry = None
        errors.append(
            LeafOnlyError(
                open_entry.meta if open_entry else default_meta,
                "Non-leaf account '{}' has postings on it".format(real_account.account),
                open_entry,
            )
        )

    return entries, errors
