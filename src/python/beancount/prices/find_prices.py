"""A library of codes create price fetching jobs from strings and files.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import re
import sys


# A price fetching job description.
#
# Attributes:
#   module: A string, the name of the Python module used to create a price source.
#   symbol: A ticker symbol in the universe of the source.
#   date: A datetime.date object for the date to be fetched, or None
#     with the meaning of fetching the latest price.
#   invert: A boolean, true if we need to invert the currency.
#   base: A commodity string, the base for the given symbol from the input file.
#     This may be null if we don't have a mapping for it.
#   quote: A commodity string, the quote currency that defines the units of the price.
#     This is also intended to be a commodity from the input file, and similarly,
#     may be null.
Job = collections.namedtuple('Job', 'module symbol date invert base quote')


def parse_source_string(source):
    """Parse a single source string.

    Source specifications follow the syntax: @<module>/[^]<ticker>
    The <module> is resolved against the Python path, but first looked up
    under the package where the default price extractors lie.

    Args:
      source: A single source string specification.
    Returns:
      A Job instance.
    Raises:
      ValueError: If the source is invalid.
    """
    match = re.match(r'([a-zA-Z]+[a-zA-Z0-9\.]+)/(\^?)([a-zA-Z0-9:_\-\.]+)', source)
    if not match:
        raise ValueError('Invalid source name: "{}"'.format(source))
    module_name, invert, symbol = match.groups()
    return Job(module_name, symbol, None, bool(invert), None, None)


# The Python package where the default sources are found.
DEFAULT_SOURCES = 'beancount.prices.sources'


def import_source(module_name):
    """Import the source module defined by the given name.

    The default location is handled here.

    Args:
      module_name: A string, the name of a Python module.
    Returns:
      A corresponding Python module object.
    Raises:
      ImportError: If the module cannot be imported.
    """
    default_name = '{}.{}'.format(DEFAULT_SOURCES, module_name)
    try:
        __import__(default_name)
        return sys.modules[default_name]
    except ImportError:
        __import__(module_name)
        return sys.modules[module_name]


def jobs_from_file(filename, date):
    """Get a list of prices to fetch from a given Beancount file.

    The active holdings held on the given date are included.

    Args:
      filename: A string, the name of a file to process.
      date: A datetime.date instance.
    Returns:
      A list of Job instances.
    """
    # FIXME - TODO(blais): Implement this.
    return []
