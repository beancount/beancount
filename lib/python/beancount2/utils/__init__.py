"""
Generic utility packages and functions.
"""
from time import time
import contextlib


def filter_type(elist, types):
    """Filter the given list to yield only instances of the given types."""
    for element in elist:
        if not isinstance(element, types):
            continue
        yield element


@contextlib.contextmanager
def print_time(operation_name):
    t1 = time()
    yield
    t2 = time()
    print(">>>>> Operation: '{}'  Time: {:.0f}ms".format(operation_name,
                                                         (t2 - t1)*1000))
