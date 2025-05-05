"""This module automatically inserts Open directives for accounts not opened (at
the date of the first entry) and automatically removes open directives for
unused accounts. This can be used as a convenience for doing demos, or when
setting up your initial transactions, as an intermediate step.
"""

__copyright__ = "Copyright (C) 2014-2017, 2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core import data
from beancount.core import getters

__plugins__ = ("auto_insert_open",)


def auto_insert_open(entries, unused_options_map):
    """Insert Open directives for accounts not opened.

    Open directives are inserted at the date of the first entry. Open directives
    for unused accounts are removed.

    Args:
      entries: A list of directives.
      unused_options_map: A parser options dict.
    Returns:
      A list of entries, possibly with more Open entries than before, and a
      list of errors.
    """
    opened_accounts = {entry.account for entry in entries if isinstance(entry, data.Open)}

    new_entries = []
    accounts_first, _ = getters.get_accounts_use_map(entries)
    for index, (account, date_first_used) in enumerate(sorted(accounts_first.items())):
        if account not in opened_accounts:
            meta = data.new_metadata("<auto_accounts>", index)
            new_entries.append(data.Open(meta, date_first_used, account, None, None))

    if new_entries:
        new_entries.extend(entries)
        new_entries.sort(key=data.entry_sortkey)
    else:
        new_entries = entries

    return new_entries, []
