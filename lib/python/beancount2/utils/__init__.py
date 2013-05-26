"""
Generic utility packages and functions.
"""
from time import time
import contextlib
from collections import defaultdict


@contextlib.contextmanager
def print_time(operation_name):
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
