"""
Generic utility packages and functions.
"""
import datetime
import logging
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

## FIXME: Move these into csv_utils.py.

def csv_dict_reader(fileobj):
    "Read a CSV file yielding normalized dictionary fields."
    reader = csv.DictReader(fileobj)
    reader.fieldnames = [re.sub('[^a-z]', '_', x.lower()).strip(' _') for x in reader.fieldnames]
    return reader


def csv_tuple_reader(fileobj, **kw):
    """Read a CSV file yielding namedtuple instances. The CSV file must have a header line."""
    reader = csv.reader(fileobj, **kw)
    ireader = iter(reader)
    Tuple = csv_parse_header(next(ireader))
    for row in ireader:
        try:
            yield Tuple(*row)
        except TypeError:
            # If there's an error, it's usually from a line that has a 'END OF
            # LINE' marker at the end, or some comment line.
            assert len(row) == 1


def csv_split_sections(rows):
    """Given a list of rows, split the list where rows have only a single entry.
    This is useful for CSV files with multiple sections, where the separator is
    a title. We use this to separate the multiple tables within the CSV files."""

    sections = {}

    section_name = None
    section_rows = []
    for row in rows:
        if len(row) == 1:
            if section_name:
                sections[section_name] = section_rows
            section_name = row[0]
            section_rows = []
        elif row:
            section_rows.append(row)

    return sections


def csv_parse_header(header_row):
    """Create a new class for the following rows from the header line.
    This both normalizes hte header line and assign"""

    fieldnames = []
    for index, column in enumerate(header_row):
        field = column.lower()
        field = re.sub(r'\bp/l\b', 'pnl', field)
        field = re.sub('[^a-z0-9]', '_', field)
        field = field.strip(' _')
        if not field:
            field = 'col{}'.format(index)
        assert field not in fieldnames, field
        fieldnames.append(field)

    return namedtuple('Row', fieldnames)
