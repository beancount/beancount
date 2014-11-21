"""This plugin validates that there are no duplicate transactions.
"""
__author__ = "Martin Blais <blais@furius.ca>"

from beancount.core import compare

__plugins__ = ('validate_no_duplicates',)


def validate_no_duplicates(entries, unused_options_map):
    """Check that the entries are unique, by computing hashes.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    unused_hashes, errors = compare.hash_entries(entries)
    return entries, errors
