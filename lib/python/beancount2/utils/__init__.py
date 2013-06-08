"""
Generic utility packages and functions.
"""
import datetime
from time import time
import contextlib
from collections import defaultdict, namedtuple
import csv
import re
import os
from os import path
import itertools


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


def csv_dict_reader(fileobj):
    "Read a CSV file yielding normalized dictionary fields."
    reader = csv.DictReader(fileobj)
    reader.fieldnames = [re.sub('[^a-z]', '_', x.lower()).strip(' _') for x in reader.fieldnames]
    return reader


def csv_tuple_reader(fileobj):
    "Read a CSV file yielding namedtuple instances. The CSV file must have a header line."
    reader = csv.reader(fileobj)
    ireader = iter(reader)
    header = next(ireader)
    fieldnames = [re.sub('[^a-z]', '_', x.lower()).strip(' _') for x in header]
    Tuple = namedtuple('Tuple', fieldnames)
    for row in ireader:
        try:
            yield Tuple(*row)
        except TypeError:
            # If there's an error, it's usually from a line that has a 'END OF
            # LINE' marker at the end, or some comment line.
            assert len(row) == 1


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
            raise IOError("File or directory '{}' does not exist.".format(ford))


ONEDAY = datetime.timedelta(days=1)

def iter_dates(start_date, end_date):
    "Yield all the dates between 'start_date' and 'end_date'."
    date = start_date
    while date < end_date:
        yield date
        date += ONEDAY
