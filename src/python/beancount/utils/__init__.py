"""
Generic utility packages and functions.
"""
import re
import datetime
import logging
from time import time
import contextlib
from collections import defaultdict
import os
import logging
from os import path




def groupby(keyfun, elements):
    """Group the elements as a dict of lists, where the key is computed using the
    function 'keyfun'."""
    grouped = defaultdict(list)
    for element in elements:
        grouped[keyfun(element)].append(element)
    return grouped


def filter_type(elist, types):
    """Filter the given list to yield only instances of the given types."""
    for element in elist:
        if not isinstance(element, types):
            continue
        yield element


def longest(seq):
    """Return the longest of the given subsequences."""
    longest, length = None, -1
    for x in seq:
        lenx = len(x)
        if lenx > length:
            longest, length = x, lenx
    return longest


def get_tuple_values(ntuple, predicate):
    """Return all members referred to by this namedtuple instance that satisfy the
    given predicate. This function also works recursively on its members, and so
    it can be used for Transaction instances.
    """
    for attribute in ntuple:
        if predicate(attribute):
            yield attribute
        elif isinstance(attribute, list):
            for sub_attribute in attribute:
                for account in get_tuple_values(sub_attribute, predicate):
                    yield account


def index_key(sequence, value, key, cmp):
    """Find the index of the first element in 'sequence' which is equal to 'value'.
    If 'key' is specified, the value compared to the value returned by this
    function. If the value is not found, return None."""
    for index, element in enumerate(sequence):
        if cmp(key(element), value):
            return index
    return None




def walk_files_or_dirs(fords, ignore_dirs=['.hg', '.svn', '.git']):
    """Enumerate the files under the given directories."""
    for ford in fords:
        if path.isdir(ford):
            for root, dirs, filenames in os.walk(ford):
                dirs[:] = [dirname for dirname in dirs if dirname not in ignore_dirs]
                for filename in filenames:
                    yield path.join(root, filename)
        elif path.isfile(ford) or path.islink(ford):
            yield ford
        elif not path.exists(ford):
            logging.error("File or directory '{}' does not exist.".format(ford))



ONEDAY = datetime.timedelta(days=1)

def iter_dates(start_date, end_date):
    "Yield all the dates between 'start_date' and 'end_date'."
    date = start_date
    while date < end_date:
        yield date
        date += ONEDAY


class DateIntervalTicker:
    """An object that will tick when the dates cross specific intervals."""

    def __init__(self, compute_value):
        self.last_value = None

        # Compute the new tick value from the date; default implementation
        # returns the date itself, thus ticking every time the date changes.
        if compute_value is None:
            compute_value = lambda new_date: new_date
        self.compute_value = compute_value

    def __call__(self, new_date):
        """Return True if the interval has been crossed; False otherwise."""
        new_value = self.compute_value(new_date)
        if new_value != self.last_value:
            self.last_value = new_value
            return True
        else:
            return False


def compute_ids(strings):
    """Given a sequence of strings, reduce them to corresponding ids without any
    funny characters and insure that the list of ids is unique. Yields pairs
    of (id, string) for the result."""

    string_set = set(strings)

    # Try multiple methods until we get one that has no collisions.
    for regexp, replacement in [('[^A-Za-z0-9-.]', '_'),
                                ('[^A-Za-z0-9]', ''),]:

        # Map ids to strings.
        idmap = defaultdict(list)
        for string in string_set:
            id = re.sub(regexp, replacement, string)
            idmap[id].append(string)

        # Check for collisions.
        if all(len(stringlist) == 1 for stringlist in idmap.values()):
            break
    else:
        raise RuntimeError("Could not find a unique mapping for {}".format(string_set))

    return sorted((id, stringlist[0]) for id, stringlist in idmap.items())
