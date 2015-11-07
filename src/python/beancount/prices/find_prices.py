"""A library of codes create price fetching jobs from strings and files.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import re
import sys


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


def jobs_from_file(filename, date):
    """Get a list of prices to fetch from a given Beancount file.

    The active holdings held on the given date are included.

    Args:
      filename: A string, the name of a file to process.
      date: A datetime.date instance.
    Returns:
      A list of DatedPrice instances.
    """
    # FIXME - TODO(blais): Implement this.
    return []
