"""Driver code for the price script.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import csv
import collections
import datetime
import io
import functools
import threading
from os import path
import shelve
import tempfile
import re
import os
import sys
from urllib import parse
from urllib import request
import urllib.parse
import hashlib
import argparse
import logging
from concurrent import futures

from dateutil.parser import parse as parse_datetime

import beancount.prices
from beancount import loader
from beancount.core import data
from beancount.core import amount
from beancount.ops import holdings
from beancount.parser import printer
from beancount.prices.sources import yahoo     # FIXME: remove this, should be dynamic
from beancount.prices.sources import google    # FIXME: remove this, should be dynamic
from beancount.utils import net_utils
from beancount.utils import memo


def setup_cache(cache_filename, clear_cache):
    """Setup the results cache.

    Args:
      cache_filename: A string or None, the filename for the cache.
      clear_cache: A boolean, if true, delete the cache before beginning.
    """
    if clear_cache and cache_filename and path.exists(cache_filename):
        logging.info("Clearing cache %s", cache_filename)
        os.remove(cache_filename)

    if cache_filename:
        logging.info('Using cache at "{}"'.format(cache_filename))
        #request.urlopen = memo.memoize_recent_fileobj(request.urlopen, cache_filename)
        net_utils.retrying_urlopen = memo.memoize_recent_fileobj(net_utils.retrying_urlopen,
                                                                 cache_filename)


def main():
    parser = argparse.ArgumentParser(description=beancount.prices.__doc__)

    # Input sources or filenames.
    parser.add_argument('inputs', nargs='+',
                        help='A list of filenames or price sources to fetch.')

    # Regular options.
    parser.add_argument('-v', '--verbose', action='store_true',
                        help="Print out progress log.")

    parse_date = lambda s: parse_datetime(s).date()
    parser.add_argument('--date', action='store', type=parse_date,
                        help="Specify the date for which to fetch the prices.")

    parser.add_argument('-i', '--always-invert', action='store_true',
                        help=("Never just swap currencies for inversion, always invert the "
                              "actual rate"))

    parser.add_argument('-a', '--all', '--all-commodities', '--all-instruments',
                        action='store_true',
                        help=("Select all commodities from files, not just the ones active "
                              "on the date"))

    parser.add_argument('-c', '--clobber', action='store_true',
                        help=("Do not skip prices which are already present in input "
                              "files; fetch them anyway."))

    parser.add_argument('-n', '--dry-run', '--jobs', '--print-only', action='store_true',
                        help=("Don't actually fetch the prices, just print the list of the "
                              "ones to be fetched."))

    # Caching options.
    cache_group = parser.add_argument_group('cache')
    cache_filename = path.join(tempfile.gettempdir(),
                               "{}.cache".format(path.basename(sys.argv[0])))
    cache_group.add_argument('--cache', dest='cache_filename',
                             action='store', default=cache_filename,
                             help="Enable the cache and with the given cache name.")
    cache_group.add_argument('--no-cache', dest='cache_filename',
                             action='store_const', const=None,
                             help="Disable the price cache.")

    cache_group.add_argument('--clear-cache', action='store_true',
                             help="Clear the cache prior to startup")

    args = parser.parse_args(argv)

    # Setup for processing.
    logging.basicConfig(level=logging.INFO if args.verbose else logging.WARN,
                        format='%(levelname)-8s: %(message)s')
    setup_cache(args.cache_filename, args.clear_cache)
