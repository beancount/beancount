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
from beancount.utils import net_utils
from beancount.utils import memo
from beancount.prices import find_prices


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


def process_args():
    """Process the arguments.

    Returns:
      A pair of 'args' the receiver of arguments and a list of Job objects.
    """
    parser = argparse.ArgumentParser(description=beancount.prices.__doc__)

    # Input sources or filenames.
    parser.add_argument('sources', nargs='+',
                        help=('A list of filenames (or source "module/symbol", if -e is '
                              'specified) from which to create a list of jobs.'))

    parser.add_argument('-e', '--expressions', '--expression', action='store_true',
                        help='Interpret the arguments as "module/symbol" source strings.')

    # Regular options.
    parser.add_argument('-v', '--verbose', action='store_true',
                        help="Print out progress log.")

    parse_date = lambda s: parse_datetime(s).date()
    parser.add_argument('--date', action='store', type=parse_date,
                        help="Specify the date for which to fetch the prices.")

    parser.add_argument('-i', '--always-invert', action='store_true',
                        help=("Never just swap currencies for inversion, always invert the "
                              "actual rate"))

    parser.add_argument('-c', '--inactive',
                        action='store_true',
                        help=("Select all commodities from input files, not just the ones "
                              "active on the date"))

    parser.add_argument('-u', '--undeclared', action='store_true',
                        help="Include commodities viewed in the file even without a "
                        "corresponding Commodity directive. The currency name itself is "
                        "used as the lookup symbol in the default sources.")

    parser.add_argument('-c', '--clobber', action='store_true',
                        help=("Do not skip prices which are already present in input "
                              "files; fetch them anyway."))

    parser.add_argument('-a', '--all', action='store_true',
                        help="A shorthand for --inactive, --undeclared, --clobber.")

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

    args = parser.parse_args()

    if args.all:
        args.inactive = args.undeclared = args.clobber = True

    # Setup for processing.
    setup_cache(args.cache_filename, args.clear_cache)

    # Get the list of jobs from the arguments.
    jobs = []
    if args.expressions:
        # Interpret the arguments as price sources.
        for sourcelist in args.sources:
            try:
                for source in sourcelist.split(','):
                    jobs.append(find_prices.parse_source_string(source))
            except ValueError:
                if path.exists(sourcelist):
                    msg = 'Invalid source "{}"; did you provide a filename?'
                else:
                    msg = 'Invalid source "{}"'
                parser.error(msg.format(sourcelist))
    else:
        # Interpret the arguments as Beancount input filenames.
        for filename in args.sources:
            if not path.exists(filename) or not path.isfile(filename):
                parser.error('File does not exist: "{}"'.format(filename))
            else:
                jobs.extend(find_prices.jobs_from_file(filename, args.date))

    return args, jobs


def main():
    args, jobs = process_args()
    logging.basicConfig(level=logging.INFO if args.verbose else logging.WARN,
                        format='%(levelname)-8s: %(message)s')

    # If we're just being asked to list the jobs, do this here.
    if args.dry_run:
        for job in jobs:
            print(job)

    # TODO: Define the jobs as cascading.

    # TODO: Validate modules here for all jobs.
