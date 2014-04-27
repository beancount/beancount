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


@contextlib.contextmanager
def print_time(operation_name, quiet=False):
    """A context manager that times the block and logs it to info level.

    Args:
      operation_name: A string, a label for the name of the operation.
      quiet: A boolean, true if this should be a no-op.
    Yields:
      The start time of the operation.
    """
    if quiet:
        yield; return
    t1 = time()
    yield t1
    t2 = time()
    logging.info(">>>>> Operation: '{}'  Time: {:.0f}ms".format(operation_name,
                                                                (t2 - t1)*1000))


def groupby(keyfun, elements):
    """Group the elements as a dict of lists, where the key is computed using the
    function 'keyfun'.

    Args:
      keyfun: A callable, used to obtain the group key from each element.
      elements: An iterable of the elements to group.
    Returns:
      A dict of key to list of sequences.
    """
    grouped = defaultdict(list)
    for element in elements:
        grouped[keyfun(element)].append(element)
    return grouped


def filter_type(elist, types):
    """Filter the given list to yield only instances of the given types.

    Args:
      elist: A sequence of elements.
      types: A sequence of types to include in the output list.
    Yields:
      Each element, if it is an instance of 'types'.
    """
    for element in elist:
        if not isinstance(element, types):
            continue
        yield element


def longest(seq):
    """Return the longest of the given subsequences.

    Args:
      seq: An iterable sequence of lists.
    Returns:
      The longest list from the sequence.
    """
    longest, length = None, -1
    for x in seq:
        lenx = len(x)
        if lenx > length:
            longest, length = x, lenx
    return longest


def get_tuple_values(ntuple, predicate, memo=None):
    """Return all members referred to by this namedtuple instance that satisfy the
    given predicate. This function also works recursively on its members which
    are lists or typles, and so it can be used for Transaction instances.

    Args:
      ntuple: A tuple or namedtuple.
      predicate: A predicate function that returns true if an attribute is to be
        output.
    Yields:
      Attributes of the tuple and its sub-elements if the predicate is true.
    """
    if memo is None:
        memo = set()
    if id(ntuple) in memo:
        return
    memo.add(id(ntuple))

    if predicate(ntuple):
        yield
    for attribute in ntuple:
        if predicate(attribute):
            yield attribute
        if isinstance(attribute, (list, tuple)):
            yield from get_tuple_values(attribute, predicate, memo)
