"""Driver code for the price script.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import functools
from os import path
import shelve
import tempfile
import hashlib
import os
import sys
import logging
from concurrent import futures

from dateutil import tz

from beancount.core.number import ONE
import beancount.prices
from beancount import loader
from beancount.core import data
from beancount.core import amount
from beancount.parser import printer
from beancount.prices import find_prices
from beancount.utils import date_utils
from beancount.utils import version


# Stand-in currency name for unknown currencies.
UNKNOWN_CURRENCY = '?'


# A cache for the prices.
_CACHE = None

# Expiration for latest prices in the cache.
DEFAULT_EXPIRATION = datetime.timedelta(seconds=30*60)  # 30 mins.


def now():
    "Indirection in order to be able to mock it out in the tests."
    return datetime.datetime.now(datetime.timezone.utc)


def fetch_cached_price(source, symbol, date):
    """Call Source to fetch a price, but look and/or update the cache first.

    This function entirely deals with caching and correct expiration. It keeps
    old prices if they were fetched in the past, and it quickly expires
    intra-day prices if they are fetched on the same day.

    Args:
      source: A Python module object.
      symbol: A string, the ticker to fetch.
      date: A datetime.date instance, None if we're to fetch the latest date.
    Returns:
      A SourcePrice instance.
    """
    # Compute a suitable timestamp from the date, if specified.
    if date is not None:
        # We query as for 4pm for the given date of the current timezone, if
        # specified.
        query_time = datetime.time(16, 0, 0)
        time_local = datetime.datetime.combine(date, query_time, tzinfo=tz.tzlocal())
        time = time_local.astimezone(tz.tzutc())
    else:
        time = None

    if _CACHE is None:
        # The cache is disabled; just call and return.
        result = (source.get_latest_price(symbol)
                  if time is None else
                  source.get_historical_price(symbol, time))

    else:
        # The cache is enabled and we have to compute the current/latest price.
        # Try to fetch from the cache but miss if the price is too old.
        md5 = hashlib.md5()
        md5.update(str((type(source).__module__, symbol, date)).encode('utf-8'))
        key = md5.hexdigest()
        timestamp_now = int(now().timestamp())
        try:
            timestamp_created, result_naive = _CACHE[key]

            # Convert naive timezone to UTC, which is what the cache is always
            # assumed to store. (The reason for this is that timezones from
            # aware datetime objects cannot be serialized properly due to bug.)
            if result_naive.time is not None:
                result = result_naive._replace(
                    time=result_naive.time.replace(tzinfo=tz.tzutc()))
            else:
                result = result_naive

            if (timestamp_now - timestamp_created) > _CACHE.expiration.total_seconds():
                raise KeyError
        except KeyError:
            logging.info("Fetching: %s (time: %s)", symbol, time)
            result = (source.get_latest_price(symbol)
                      if time is None else
                      source.get_historical_price(symbol, time))

            # Make sure the timezone is UTC and make naive before serialization.
            if result and result.time is not None:
                time_utc = result.time.astimezone(tz.tzutc())
                time_naive = time_utc.replace(tzinfo=None)
                result_naive = result._replace(time=time_naive)
            else:
                result_naive = result

            _CACHE[key] = (timestamp_now, result_naive)
    return result


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
        logging.info('Using price cache at "%s" (with indefinite expiration)',
                     cache_filename)

        global _CACHE
        _CACHE = shelve.open(cache_filename, 'c')
        _CACHE.expiration = DEFAULT_EXPIRATION


def reset_cache():
    """Reset the cache to its uninitialized state."""
    global _CACHE
    if _CACHE is not None:
        _CACHE.close()
    _CACHE = None


def fetch_price(dprice, swap_inverted=False):
    """Fetch a price for the DatePrice job.

    Args:
      dprice: A DatedPrice instances.
      swap_inverted: A boolean, true if we should invert currencies instead of
        rate for an inverted price source.
    Returns:
      A Price entry corresponding to the output of the jobs processed.

    """
    for psource in dprice.sources:
        try:
            source = psource.module.Source()
        except AttributeError:
            continue
        srcprice = fetch_cached_price(source, psource.symbol, dprice.date)
        if srcprice is not None:
            break
    else:
        if dprice.sources:
            logging.error("Could not fetch for job: %s", dprice)
        return None

    base = dprice.base
    quote = dprice.quote or srcprice.quote_currency
    price = srcprice.price

    # Invert the rate if requested.
    if psource.invert:
        if swap_inverted:
            base, quote = quote, base
        else:
            price = ONE/price

    assert base is not None
    fileloc = data.new_metadata('<{}>'.format(type(psource.module).__name__), 0)

    # The datetime instance is required to be aware. We always convert to the
    # user's timezone before extracting the date. This means that if the market
    # returns a timestamp for a particular date, once we convert to the user's
    # timezone the returned date may be different by a day. The intent is that
    # whatever we print is assumed coherent with the user's timezone. See
    # discussion at
    # https://groups.google.com/d/msg/beancount/9j1E_HLEMBQ/fYRuCQK_BwAJ
    srctime = srcprice.time
    if srctime.tzinfo is None:
        raise ValueError("Time returned by the price source is not timezone aware.")
    date = srctime.astimezone(tz.tzlocal()).date()

    return data.Price(fileloc, date, base,
                      amount.Amount(price, quote or UNKNOWN_CURRENCY))


def filter_redundant_prices(price_entries, existing_entries, diffs=False):
    """Filter out new entries that are redundant from an existing set.

    If the price differs, we override it with the new entry only on demand. This
    is because this would create conflict with existing price entries when
    parsing, if the new entries are simply inserted into the input.

    Args:
      price_entries: A list of newly created, proposed to be added Price directives.
      existing_entries: A list of existing entries we are proposing to add to.
      diffs: A boolean, true if we should output differing price entries
        at the same date.
    Returns:
      A filtered list of remaining entries, and a list of ignored entries.
    """
    # Note: We have to be careful with the dates, because requesting the latest
    # price for a date may yield the price at a previous date. Clobber needs to
    # take this into account. See {1cfa25e37fc1}.
    existing_prices = {(entry.date, entry.currency): entry
                       for entry in existing_entries
                       if isinstance(entry, data.Price)}
    filtered_prices = []
    ignored_prices = []
    for entry in price_entries:
        key = (entry.date, entry.currency)
        if key in existing_prices:
            if diffs:
                existing_entry = existing_prices[key]
                if existing_entry.amount == entry.amount:
                    output = ignored_prices
            else:
                output = ignored_prices
        else:
            output = filtered_prices
        output.append(entry)
    return filtered_prices, ignored_prices


def process_args():
    """Process the arguments. This also initializes the logging module.

    Returns:
      A tuple of:
        args: The argparse receiver of command-line arguments.
        jobs: A list of DatedPrice job objects.
        entries: A list of all the parsed entries.
    """
    parser = version.ArgumentParser(description=beancount.prices.__doc__.splitlines()[0])

    # Input sources or filenames.
    parser.add_argument('sources', nargs='+', help=(
        'A list of filenames (or source "module/symbol", if -e is '
        'specified) from which to create a list of jobs.'))

    parser.add_argument('-e', '--expressions', '--expression', action='store_true', help=(
        'Interpret the arguments as "module/symbol" source strings.'))

    # Regular options.
    parser.add_argument('-v', '--verbose', action='count', help=(
        "Print out progress log. Specify twice for debugging info."))

    parser.add_argument('-d', '--date', action='store',
                        type=date_utils.parse_date_liberally, help=(
        "Specify the date for which to fetch the prices."))

    parser.add_argument('-i', '--inactive', action='store_true', help=(
        "Select all commodities from input files, not just the ones active on the date"))

    parser.add_argument('-u', '--undeclared', action='store', help=(
        "Include commodities viewed in the file even without a "
        "corresponding Commodity directive, from this default source. "
        "The currency name itself is used as the lookup symbol in this default source."))

    parser.add_argument('-c', '--clobber', action='store_true', help=(
        "Do not skip prices which are already present in input files; fetch them anyway."))

    parser.add_argument('-a', '--all', action='store_true', help=(
        "A shorthand for --inactive, --undeclared, --clobber."))

    parser.add_argument('-s', '--swap-inverted', action='store_true', help=(
        "For inverted sources, swap currencies instead of inverting the rate. "
        "For example, if fetching the rate for CAD from 'USD:google/^CURRENCY:USDCAD' "
        "results in 1.25, by default we would output \"price CAD  0.8000 USD\". "
        "Using this option we would instead output \" price USD   1.2500 CAD\"."))

    parser.add_argument('-n', '--dry-run', action='store_true', help=(
        "Don't actually fetch the prices, just print the list of the ones to be fetched."))

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

    verbose_levels = {None: logging.WARN,
                      0: logging.WARN,
                      1: logging.INFO,
                      2: logging.DEBUG}
    logging.basicConfig(level=verbose_levels[args.verbose],
                        format='%(levelname)-8s: %(message)s')

    if args.all:
        args.inactive = args.undeclared = args.clobber = True

    # Setup for processing.
    setup_cache(args.cache_filename, args.clear_cache)

    # Get the list of DatedPrice jobs to get from the arguments.
    logging.info("Processing at date: %s", args.date or datetime.date.today())
    jobs = []
    all_entries = []
    dcontext = None
    if args.expressions:
        # Interpret the arguments as price sources.
        for source_str in args.sources:
            psources = []
            try:
                psource_map = find_prices.parse_source_map(source_str)
            except ValueError:
                extra = "; did you provide a filename?" if path.exists(source_str) else ''
                msg = ('Invalid source "{{}}"{}. '.format(extra) +
                       'Supported format is "CCY:module/SYMBOL"'.format(extra))
                parser.error(msg.format(source_str))
            else:
                for currency, psources in psource_map.items():
                    jobs.append(find_prices.DatedPrice(
                        psources[0].symbol, currency, args.date, psources))
    else:
        # Interpret the arguments as Beancount input filenames.
        for filename in args.sources:
            if not path.exists(filename) or not path.isfile(filename):
                parser.error('File does not exist: "{}"; '
                             'did you mean to use -e?'.format(filename))
                continue
            logging.info('Loading "%s"', filename)
            entries, errors, options_map = loader.load_file(filename, log_errors=sys.stderr)
            if dcontext is None:
                dcontext = options_map['dcontext']
            jobs.extend(
                find_prices.get_price_jobs_at_date(
                    entries, args.date, args.inactive, args.undeclared))
            all_entries.extend(entries)

    return args, jobs, data.sorted(all_entries), dcontext


def main():
    args, jobs, entries, dcontext = process_args()

    # If we're just being asked to list the jobs, do this here.
    if args.dry_run:
        for dprice in jobs:
            print(find_prices.format_dated_price_str(dprice))
        return

    # Fetch all the required prices, processing all the jobs.
    executor = futures.ThreadPoolExecutor(max_workers=3)
    price_entries = filter(None, executor.map(
        functools.partial(fetch_price, swap_inverted=args.swap_inverted), jobs))

    # Sort them by currency, regardless of date (the dates should be close
    # anyhow, and we tend to put them in chunks in the input files anyhow).
    price_entries = sorted(price_entries, key=lambda e: e.currency)

    # Avoid clobber, remove redundant entries.
    if not args.clobber:
        price_entries, ignored_entries = filter_redundant_prices(price_entries, entries)
        for entry in ignored_entries:
            logging.info("Ignored to avoid clobber: %s %s", entry.date, entry.currency)

    # Print out the entries.
    printer.print_entries(price_entries, dcontext=dcontext)
