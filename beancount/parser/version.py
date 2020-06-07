"""Implement common options across all programs."""

__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import datetime
import re

import beancount
from beancount.parser import _parser


# pylint: disable=invalid-name
def ArgumentParser(*args, **kwargs):
    """Add a standard --version option to an ArgumentParser.

    Args:
      *args: Arguments for the parser.
      *kwargs: Keyword arguments for the parser.
    Returns:
      An instance of ArgumentParser, with our default options set.
    """
    parser = argparse.ArgumentParser(*args, **kwargs)

    parser.add_argument('--version', '-V', action='version',
                        version=compute_version_string(
                            beancount.__version__,
                            getattr(_parser, "__vc_changeset__", None),
                            getattr(_parser, "__vc_timestamp__", 0)))

    return parser


def compute_version_string(version, changeset, timestamp):
    """Compute a version string from the changeset and timestamp baked in the parser.

    Args:
      version: A string, the version number.
      changeset: A string, a version control string identifying the commit of the version.
      timestamp: An integer, the UNIX epoch timestamp of the changeset.
    Returns:
      A human-readable string for the version.
    """
    # Shorten changeset.
    if changeset:
        if re.match('hg:', changeset):
            changeset = changeset[:15]
        elif re.match('git:', changeset):
            changeset = changeset[:12]

    # Convert timestamp to a date.
    date = None
    if timestamp > 0:
        date = datetime.datetime.utcfromtimestamp(timestamp).date()

    version = 'Beancount {}'.format(version)
    if changeset or date:
        version = '{} ({})'.format(
            version, '; '.join(map(str, filter(None, [changeset, date]))))

    return version
