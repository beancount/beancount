"""This plugin validates that there are no duplicate transactions."""

__copyright__ = "Copyright (C) 2014, 2016-2017, 2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core import compare

__plugins__ = ("validate_no_duplicates",)


def validate_no_duplicates(entries, unused_options_map):
    """Check that the entries are unique, by computing hashes.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    unused_hashes, errors = compare.hash_entries(entries, exclude_meta=True)
    return entries, errors
