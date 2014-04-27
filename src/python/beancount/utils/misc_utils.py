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
    if quiet:
        yield; return
    t1 = time()
    yield
    t2 = time()
    logging.info(">>>>> Operation: '{}'  Time: {:.0f}ms".format(operation_name,
                                                                (t2 - t1)*1000))


def groupby(keyfun, elements):
    """Group the elements as a dict of lists, where the key is computed using the
    function 'keyfun'."""
    grouped = defaultdict(list)
    for element in elements:
        grouped[keyfun(element)].append(element)
    return grouped
