"""Comparison helpers for data objects.
"""
import hashlib
import datetime
from collections import namedtuple

from .data import Transaction, entry_sortkey
from .amount import Amount, Decimal, to_decimal
from .position import Position, create_position
from .account import has_component


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
    hashobj = hashlib.md5()
    for attr_name, attr_value in zip(objtuple._fields, objtuple):
        if attr_name in ignore:
            continue
        if isinstance(attr_value, (list, set)):
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
    return stable_hash_namedtuple(entry, {'fileloc', 'entry', 'diff_amount'})


def hash_entries(entries):
    """Compute unique hashes of each of the entries and return a map of them.

    This is used for comparisons between sets of entries.

    Args:
      entries: A list of directives.
    Returns:
      A dict of hash-value to entry, for all entries.
    """
    entry_hash_dict = {}
    for entry in entries:
        entry_type = type(entry)

        hash_ = hash_entry(entry)
        if hash_ in entry_hash_dict:
            other_entry = entry_hash_dict[hash_]
            raise ValueError("Duplicate entry: {} == {}".format(entry,
                                                                other_entry))
        entry_hash_dict[hash_] = entry

    assert len(entry_hash_dict) == len(entries)
    return entry_hash_dict


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
    """
    hashes1 = hash_entries(entries1)
    hashes2 = hash_entries(entries2)
    keys1 = set(hashes1.keys())
    keys2 = set(hashes2.keys())

    same = keys1 == keys2
    missing1 = sorted([hashes1[key] for key in keys1 - keys2],
                      key=entry_sortkey)
    missing2 = sorted([hashes2[key] for key in keys2 - keys1],
                      key=entry_sortkey)
    return (same, missing1, missing2)


def includes_entries(subset_entries, entries):
    """Check if a list of entries is included in another list.

    Args:
      subset_entries: The set of entries to look for in 'entries'.
      entries: The larger list of entries that could include 'subset_entries'.
    Returns:
      A boolean and a list of missing entries.
    """
    subset_hashes = hash_entries(subset_entries)
    subset_keys = set(subset_hashes.keys())
    hashes = hash_entries(entries)
    keys = set(hashes.keys())

    includes = subset_keys.issubset(keys)
    missing = sorted([subset_hashes[key] for key in subset_keys - keys],
                     key=entry_sortkey)
    return (includes, missing)
