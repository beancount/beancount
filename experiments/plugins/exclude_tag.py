"""Exclude #virtual tags.

This is used to demonstrate excluding a set of transactions from a particular
tag. In this example module, the tag name is fixed, but if we integrated this we
could provide a way to choose which tags to exclude. This is simply just another
mechanism for selecting a subset of transactions.

See discussion here for details:
https://groups.google.com/d/msg/ledger-cli/N8Slh2t45K0/aAz0i3Be4LYJ
"""

__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"
__plugins__ = ('exclude_tag',)

from beancount.core import data


EXCLUDED_TAG = 'virtual'

def exclude_tag(entries, options_map):
    """Select all transactions that do not have a #virtual tag.

    Args:
      entries: A list of entry instances.
      options_map: A dict of options parsed from the file.
    Returns:
      A tuple of entries and errors.
    """
    filtered_entries = [entry
                        for entry in entries
                        if (not isinstance(entry, data.Transaction) or
                            not entry.tags or
                            EXCLUDED_TAG not in entry.tags)]
    return (filtered_entries, [])
