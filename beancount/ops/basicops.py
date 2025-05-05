"""Basic filtering and aggregation operations on lists of entries.

This module contains some common basic operations on entries that are complex
enough not to belong in core/data.py.
"""

__copyright__ = "Copyright (C) 2014, 2016-2017, 2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

from collections import defaultdict

from beancount.core import data


def filter_tag(tag, entries):
    """Yield all the entries which have the given tag.

    Args:
      tag: A string, the tag we are interested in.
    Yields:
      Every entry in 'entries' that tags to 'tag.
    """
    for entry in entries:
        if isinstance(entry, data.Transaction) and entry.tags and tag in entry.tags:
            yield entry


def filter_link(link, entries):
    """Yield all the entries which have the given link.

    Args:
      link: A string, the link we are interested in.
    Yields:
      Every entry in 'entries' that links to 'link.
    """
    for entry in entries:
        if isinstance(entry, data.Transaction) and entry.links and link in entry.links:
            yield entry


def group_entries_by_link(entries):
    """Group the list of entries by link.

    Args:
      entries: A list of directives/transactions to process.
    Returns:
      A dict of link-name to list of entries.
    """
    link_groups = defaultdict(list)
    for entry in entries:
        if not (isinstance(entry, data.Transaction) and entry.links):
            continue
        for link in entry.links:
            link_groups[link].append(entry)
    return link_groups


def get_common_accounts(entries):
    """Compute the intersection of the accounts on the given entries.

    Args:
      entries: A list of Transaction entries to process.
    Returns:
      A set of strings, the names of the common accounts from these
      entries.
    """
    assert all(isinstance(entry, data.Transaction) for entry in entries)

    # If there is a single entry, the common accounts to it is all its accounts.
    # Note that this also works with no entries (yields an empty set).
    if len(entries) < 2:
        if entries:
            intersection = {posting.account for posting in entries[0].postings}
        else:
            intersection = set()
    else:
        entries_iter = iter(entries)
        intersection = set(posting.account for posting in next(entries_iter).postings)
        for entry in entries_iter:
            accounts = set(posting.account for posting in entry.postings)
            intersection &= accounts
            if not intersection:
                break
    return intersection
