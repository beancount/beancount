#!/usr/bin/env python3
"""Test out performance from caching the output of the loader as a pickle.
This should be possible because it's all simple tuples.
"""
__author__ = 'blais@furius.ca (Martin Blais)'

import argparse
import time
import pickle

from beancount import loader

def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename')
    args = parser.parse_args()

    t1 = time.time()
    result = loader.load_file(args.filename)
    t2 = time.time()
    cache_filename = args.filename + '_pickled'
    with open(cache_filename, 'w') as file:
        pickle.dump(result, file)
    t3 = time.time()
    with open(cache_filename) as file:
        pickle.load(result, file)
    t4 = time.time()

    print(t2-t1)
    print(t3-t2)
    print(t4-t3)

if __name__ == '__main__':
    main()
