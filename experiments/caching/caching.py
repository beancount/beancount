"""Centralized mechanism for all caching.

This module contains functions and common utilities for cache files located by
default under a single directory. In particular, this is used for:

  * Load Cache: Caches the parsing and processing of a top-level Beancount file
    to a pickle.

  * Price Cache: Caches previously fetched prices from external price sources.
    This allows us to re-run price fetching cheaply.

  * Document Conversion Cache: For importing new documents, some
    conversions are very expensive, in particular conversions from PDF. When
    there are problems, we end up having to re-run these slow conversions many
    times. Even without problems, we run them at least twice: Once for
    extraction and once for filing.
"""

__copyright__ = "Copyright (C) 2013-2018, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import os
import re
from os import path

# The default parent directory for all cache files.
BEANCOUNT_DIR = os.environ.get("BEANCOUNT_DIR", path.expanduser("~/.beancount"))


def get_file(filename):
    """Return a filename ensuring its parent directory.

    Args:
      filename: A string, the filename. If the filename is relative, it is
        created relative to BEANCOUNT_DIR.
    """
    if not path.isabs(filename):
        filename = path.join(BEANCOUNT_DIR, filename)
    os.makedirs(path.dirname(filename), exist_ok=True)
    return filename


class Cache:
    """A cache object, contains cache configuration and implements the cache."""

    def __init__(self, prefix):
        """Constructor.

        Args:
          prefix: A string, the name of the cache, e.g. 'load'.
        """
        assert re.match("[a-z]{3,20}$", prefix), "Invalid prefix: {}".format(prefix)
        self.prefix = prefix

    def add_args(self, parser):
        """Adds caching arguments to the parser."""

        name = "{}-cache".format(self.prefix)

        cache_group = parser.add_argument_group(name)
        cache_group.add_argument(
            "--{}".format(name),
            metavar="DIR",
            dest="{}_cache_dir".format(self.prefix),
            action="store",
            default=None,
            help=(
                "Enable the {} cache with the given cache directory. "
                "Set to empty string if you want to disable the cache."
            ).format(self.prefix),
        )

        cache_group.add_argument(
            "--clear-{}".format(name),
            dest="{}_cache_clear".format(self.prefix),
            action="store_true",
            default=False,
            help="Clear the {} cache prior to startup.".format(self.prefix),
        )

    def configure(self, args):
        """Copy the relevant cache configuration.

        Args:
          args: An options object as produced by argparse.parse_args().
        """
        self.enabled = True
        self.dirname = getattr(args, "{}_cache_dir".format(self.prefix), None)
        if self.dirname is None:
            self.dirname = path.join(BEANCOUNT_DIR, self.prefix)
        elif not self.dirname:
            self.dirname = None
            self.enabled = False
        self.clear = getattr(args, "{}_cache_clear".format(self.prefix))

        # Ensure the parent directory exists.
        if self.dirname and not path.exists(self.dirname):
            os.makedirs(self.dirname, exist_ok=True)
