#!/usr/bin/env python3
"""Test out performance from caching the output of the loader as a pickle.
This should be possible because it's all simple tuples.
"""
__author__ = 'blais@furius.ca (Martin Blais)'

import argparse
import datetime
import collections
import time
import pickle

from beancount import loader
from beancount.core import data
from beancount.core.number import D


Thing = collections.namedtuple('Thing', 'foo bar baz boo')


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename')
    parser.add_argument('-t', '--test', action='store_true')
    args = parser.parse_args()
    cache_filename = args.filename + '_pickled'

    t1 = time.time()

    entries, errors, options_map = loader.load_file(args.filename)
    result = (entries, errors, options_map)

    t2 = time.time()

    with open(cache_filename, 'wb') as file:
        pickle.dump(result, file)

    t3 = time.time()

    with open(cache_filename, 'rb') as file:
        result = pickle.load(file)

    t4 = time.time()

    print(t2-t1)
    print(t3-t2)
    print(t4-t3)

    entries, errors, options_map = result
    txn = next(entry
               for entry in entries
               if isinstance(entry, data.Transaction))
    print(txn)


if __name__ == '__main__':
    main()
