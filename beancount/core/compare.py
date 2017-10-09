"""Comparison helpers for data objects.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import hashlib

from beancount.core.data import Price
from beancount.core import data


CompareError = collections.namedtuple('CompareError', 'source message entry')

# A list of field names that are being ignored for persistence.
IGNORED_FIELD_NAMES = {'meta', 'diff_amount'}


def stable_hash_namedtuple(objtuple, ignore=frozenset()):
    """Hash the given namedtuple and its child fields.

    The hash_obj is updated. This iterates over all the members of objtuple,
    skipping the attributes from 'ignore', and if the elements are
    lists or sets, sorts them for stability.

    Args:
      objtuple: A tuple object or other.
      ignore: A set of strings, attribute names to be skipped in
        computing a stable hash. For instance, circular references to objects
        or irrelevant data.
    """
    # Note: this routine is slow and would stand to be implemented in C.
    hashobj = hashlib.md5()
    for attr_name, attr_value in zip(objtuple._fields, objtuple):
        if attr_name in ignore:
            continue
        if isinstance(attr_value, (list, set, frozenset)):
            subhashes = set()
            for element in attr_value:
                if isinstance(element, tuple):
                    subhashes.add(stable_hash_namedtuple(element, ignore))
                else:
                    md5 = hashlib.md5()
                    md5.update(str(element).encode())
                    subhashes.add(md5.hexdigest())
            for subhash in sorted(subhashes):
                hashobj.update(subhash.encode())
        else:
            hashobj.update(str(attr_value).encode())
    return hashobj.hexdigest()


def hash_entry(entry):
    """Compute the stable hash of a single entry.

    Args:
      entry: A directive instance.
    Returns:
      A stable hexadecimal hash of this entry.
    """
    return stable_hash_namedtuple(entry, IGNORED_FIELD_NAMES)


def hash_entries(entries):
    """Compute unique hashes of each of the entries and return a map of them.

    This is used for comparisons between sets of entries.

    Args:
      entries: A list of directives.
    Returns:
      A dict of hash-value to entry (for all entries) and a list of errors.
      Errors are created when duplicate entries are found.
    """
    entry_hash_dict = {}
    errors = []
    num_legal_duplicates = 0
    for entry in entries:
        hash_ = hash_entry(entry)

        if hash_ in entry_hash_dict:
            if isinstance(entry, Price):
                # Note: Allow duplicate Price entries, they should be common
                # because of the nature of stock markets (if they're closed, the
                # data source is likely to return an entry for the previously
                # available date, which may already have been fetched).
                num_legal_duplicates += 1
            else:
                other_entry = entry_hash_dict[hash_]
                errors.append(
                    CompareError(entry.meta,
                                 "Duplicate entry: {} == {}".format(entry, other_entry),
                                 entry))
        entry_hash_dict[hash_] = entry

    if not errors:
        assert len(entry_hash_dict) + num_legal_duplicates == len(entries), (
            len(entry_hash_dict), len(entries), num_legal_duplicates)
    return entry_hash_dict, errors


def compare_entries(entries1, entries2):
    """Compare two lists of entries. This is used for testing.

    The entries are compared with disregard for their file location.

    Args:
      entries1: A list of directives of any type.
      entries2: Another list of directives of any type.
    Returns:
      A tuple of (success, not_found1, not_found2), where the fields are:
        success: A booelan, true if all the values are equal.
        missing1: A list of directives from 'entries1' not found in
          'entries2'.
        missing2: A list of directives from 'entries2' not found in
          'entries1'.
    Raises:
      ValueError: If a duplicate entry is found.
    """
    hashes1, errors1 = hash_entries(entries1)
    hashes2, errors2 = hash_entries(entries2)
    keys1 = set(hashes1.keys())
    keys2 = set(hashes2.keys())

    if errors1 or errors2:
        error = (errors1 + errors2)[0]
        raise ValueError(str(error))

    same = keys1 == keys2
    missing1 = data.sorted([hashes1[key] for key in keys1 - keys2])
    missing2 = data.sorted([hashes2[key] for key in keys2 - keys1])
    return (same, missing1, missing2)


def includes_entries(subset_entries, entries):
    """Check if a list of entries is included in another list.

    Args:
      subset_entries: The set of entries to look for in 'entries'.
      entries: The larger list of entries that could include 'subset_entries'.
    Returns:
      A boolean and a list of missing entries.
    Raises:
      ValueError: If a duplicate entry is found.
    """
    subset_hashes, subset_errors = hash_entries(subset_entries)
    subset_keys = set(subset_hashes.keys())
    hashes, errors = hash_entries(entries)
    keys = set(hashes.keys())

    if subset_errors or errors:
        error = (subset_errors + errors)[0]
        raise ValueError(str(error))

    includes = subset_keys.issubset(keys)
    missing = data.sorted([subset_hashes[key] for key in subset_keys - keys])
    return (includes, missing)


def excludes_entries(subset_entries, entries):
    """Check that a list of entries does not appear in another list.

    Args:
      subset_entries: The set of entries to look for in 'entries'.
      entries: The larger list of entries that should not include 'subset_entries'.
    Returns:
      A boolean and a list of entries that are not supposed to appear.
    Raises:
      ValueError: If a duplicate entry is found.
    """
    subset_hashes, subset_errors = hash_entries(subset_entries)
    subset_keys = set(subset_hashes.keys())
    hashes, errors = hash_entries(entries)
    keys = set(hashes.keys())

    if subset_errors or errors:
        error = (subset_errors + errors)[0]
        raise ValueError(str(error))

    intersection = keys.intersection(subset_keys)
    excludes = not bool(intersection)
    extra = data.sorted([subset_hashes[key] for key in intersection])
    return (excludes, extra)
