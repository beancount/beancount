"""Implement common options across all programs."""

__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import datetime
import re

import beancount
from beancount.parser import _parser


def ArgumentParser(*args, **kwargs):
    """Add a standard --version option to an ArgumentParser.

    Args:
      *args: Arguments for the parser.
      *kwargs: Keyword arguments for the parser.
    Returns:
      An instance of ArgumentParser, with our default options set.
    """
    parser = argparse.ArgumentParser(*args, **kwargs)

    # Shorten changeset.
    changeset = _parser.__vc_changeset__[:12]
    if re.match(changeset, 'hg:'):
        changeset = changeset[:15]
    elif re.match(changeset, 'git:'):
        changeset = changeset[:12]

    vc_date = datetime.datetime.fromtimestamp(_parser.__vc_timestamp__).date()
    parser.add_argument('--version', '-V', action='version',
                        version='Beancount {} (changeset: {}; {})'.format(
                            beancount.__version__, changeset, vc_date))

    return parser
