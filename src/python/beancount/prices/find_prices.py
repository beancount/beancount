"""A library of codes create price fetching jobs from strings and files.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import itertools
import re
import sys

from beancount.core import data
from beancount.ops import summarize


# A dated price source description.
#
# Attributes:
#   base: A commodity string, the base for the given symbol from the input file.
#     This may be null if we don't have a mapping for it.
#   quote: A commodity string, the quote currency that defines the units of the price.
#     This is also intended to be a commodity from the input file, and similarly,
#     may be null.
#   date: A datetime.date object for the date to be fetched, or None
#     with the meaning of fetching the latest price.
#   sources: A list of PriceSource instances.
DatedPrice = collections.namedtuple('DatedPrice', 'base quote date sources')


# A price source.
#
#   module: A Python module, the module to be called to create a price source.
#   symbol: A ticker symbol in the universe of the source.
#   invert: A boolean, true if we need to invert the currency.
PriceSource = collections.namedtuple('PriceSource', 'module symbol invert')


def format_dated_price_str(dprice):
    """Convert a dated price to a one-line printable string.

    Args:
      dprice: A DatedPrice instance.
    Returns:
      The string for a DatedPrice instance.
    """
    psstrs = ['{}({}{})'.format(psource.module.__name__,
                                '1/' if psource.invert else '',
                                psource.symbol)
              for psource in dprice.sources]
    base_quote = '{} / {}'.format(dprice.base, dprice.quote)
    return '{:>24} @ {:10} [ {} ]'.format(base_quote,
                                        dprice.date or 'latest',
                                        ','.join(psstrs))


def parse_source_string(source):
    """Parse a single source string.

    Source specifications follow the syntax: @<module>/[^]<ticker>
    The <module> is resolved against the Python path, but first looked up
    under the package where the default price extractors lie.

    Args:
      source: A single source string specification.
    Returns:
      A PriceSource tuple.
    Raises:
      ValueError: If the source is invalid.
    """
    match = re.match(r'([a-zA-Z]+[a-zA-Z0-9\.]+)/(\^?)([a-zA-Z0-9:_\-\.]+)', source)
    if not match:
        raise ValueError('Invalid source name: "{}"'.format(source))
    short_module_name, invert, symbol = match.groups()
    full_module_name = import_source(short_module_name)
    return PriceSource(full_module_name, symbol, bool(invert))


# The Python package where the default sources are found.
DEFAULT_SOURCE_PACKAGE = 'beancount.prices.sources'


def import_source(module_name):
    """Import the source module defined by the given name.

    The default location is handled here.

    Args:
      short_module_name: A string, the name of a Python module, which may
        be within the default package or a full name.
    Returns:
      A corresponding Python module object.
    Raises:
      ImportError: If the module cannot be imported.
    """
    default_name = '{}.{}'.format(DEFAULT_SOURCE_PACKAGE, module_name)
    try:
        __import__(default_name)
        return sys.modules[default_name]
    except ImportError:
        try:
            __import__(module_name)
            return sys.modules[module_name]
        except ImportError as exc:
            raise ImportError('Could not find price source module "{}": {}'.format(
                module_name, exc))


def find_currencies_declared(entries, date=None):
    """Return currencies declared in Commodity directives.

    Args:
      entries: A list of directives.
      date: A datetime.date instance.
    Returns:
      A set of (base, quote, sources-string) currencies.
    """
    currencies = set()
    for entry in entries:
        if not isinstance(entry, data.Commodity):
            continue
        if date and entry.date >= date:
            break
        if 'quote' not in entry.meta:
            continue
        currencies.add((entry.currency, entry.meta['quote'], entry.meta.get('ticker', None)))
    return currencies


def find_currencies_at_cost(entries):
    """Return all currencies that were held at cost at some point.

    This returns all of them, over all time.

    Args:
      entries: A list of directives.
      date: A datetime.date instance.
    Returns:
      A list of (base, quote) currencies.
    """
    currencies = set()
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        for posting in entry.postings:
            lot = posting.position.lot
            if lot.cost:
                currencies.add((lot.currency, lot.cost.currency))
    return currencies


def find_currencies_converted(entries, date=None):
    """Return currencies that were price converted.

    This considers only the price conversions that occurred up to the given
    date.

    Args:
      entries: A list of directives.
      date: A datetime.date instance.
    Returns:
      A list of (base, quote) currencies.

    """
    currencies = set()
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        if date and entry.date >= date:
            break
        for posting in entry.postings:
            lot = posting.position.lot
            price = posting.price
            if lot.cost is not None or price is None:
                continue
            currencies.add((lot.currency, price.currency))
    return currencies


def find_balance_currencies(entries, date=None):
    """Return currencies relevant for the given date.

    This computes the account balances as of the date, and returns the union of:
    a) The currencies held at cost, and
    b) Currency pairs from previous conversions, but only for currencies with
       non-zero balances.

    This is intended to produce the list of currencies whose prices are relevant
    at a particular date, based on previous history.

    Args:
      entries: A list of directives.
      date: A datetime.date instance.
    Returns:
      A set of (base, quote) currencies.
    """
    # Find the price conversions until this date.
    converted = find_currencies_converted(entries, date)

    # Compute the balances.
    currencies = set()
    currencies_on_books = set()
    balances, _ = summarize.balance_by_account(entries, date)
    for _, balance in balances.items():
        for pos in balance:
            lot = pos.lot
            if lot.cost is not None:
                # Add currencies held at cost.
                currencies.add((lot.currency, lot.cost.currency))
            else:
                # Add regular currencies.
                currencies_on_books.add(lot.currency)

    # Add currency pairs that contain the currency whose balanace
    for currency in currencies_on_books:
        for base, quote in converted:
            if base == currency or quote == currency:
                currencies.add((base, quote))

    return currencies


def get_price_jobs_at_date(entries, date=None, inactive=False, undeclared=False):
    """Get a list of prices to fetch from a stream of entries.

    The active holdings held on the given date are included.

    Args:
      filename: A string, the name of a file to process.
      date: A datetime.date instance.
    Returns:
      A list of DatedPrice instances.
    """
    # Find the list of declared currencies, and from it build a mapping for
    # tickers for each (base, quote) pair. This is the only place tickers
    # appear.
    declared_triples = find_currencies_declared(entries, date)
    ticker_map = {(base, quote): ticker
                  for base, quote, ticker in declared_triples}

    # Compute full list of currencies to of interest.
    if undeclared:
        cur_at_cost = find_currencies_at_cost(entries)
        cur_converted = find_currencies_converted(entries, date)
        currencies = cur_at_cost | cur_converted
    else:
        currencies = set(ticker_map.keys())

    # By default, restrict to only the currencies with non-zero balances at the
    # given date.
    if not inactive:
        balance_currencies = find_balance_currencies(entries, date)
        currencies = currencies & balance_currencies

    # Build up the list of jobs to fetch prices for.
    jobs = []
    for base_quote in currencies:
        source_list = ticker_map.get(base_quote, None)
        if source_list:
            sources = list(map(parse_source_string, source_list.split(',')))
        else:
            sources = []
        base, quote = base_quote
        jobs.append(DatedPrice(base, quote, date, sources))
    return sorted(jobs)
