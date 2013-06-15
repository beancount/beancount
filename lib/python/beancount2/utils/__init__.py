"""
Generic utility packages and functions.
"""
import datetime
import logging
from time import time
import contextlib
from collections import defaultdict
import os
from os import path


@contextlib.contextmanager
def print_time(operation_name, quiet=False):
    if quiet:
        yield; return
    t1 = time()
    yield
    t2 = time()
    print(">>>>> Operation: '{}'  Time: {:.0f}ms".format(operation_name,
                                                         (t2 - t1)*1000))


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


def get_tuple_typed_values(ntuple, clstype):
    """Return all the accounts referred to by this namedtuple instance.
    This function also works recursively on its members, and so it
    can be used for Transaction instances."""
    for attribute in ntuple:
        if isinstance(attribute, clstype):
            yield attribute
        elif isinstance(attribute, list):
            for sub_attribute in attribute:
                for account in get_tuple_typed_values(sub_attribute, clstype):
                    yield account


def index_key(sequence, value, key=None):
    """Find the index of the first element in 'sequence' whic is equal to 'value'.
    If 'key' is specified, the value compared to the value returned by this
    function. If the value is not found, return None."""
    for index, element in enumerate(sequence):
        # FIXME: Use a version with 'is' comparison for performance? Test it, measure the difference.
        if key(element) == value:
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

    def check(self, new_date):
        """Return True if the interval has been crossed; False otherwise."""
        new_value = self.compute_value(new_date)
        if new_value != self.last_value:
            self.last_value = new_value
            return True
        else:
            return False
