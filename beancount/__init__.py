"""Beancount, a double-entry bookkeeping software."""

from beancount.parser._rust import __version__  # noqa: F401

__copyright__ = "Copyright (C) 2013-2014, 2016-2024  Martin Blais"
__license__ = "GNU GPLv2"


# Check the version requirements.
import sys

if (sys.version_info.major, sys.version_info.minor) < (3, 7):
    raise ImportError("Python 3.7 or above is required")


# Expose the public API.
del sys
from .api import *  # noqa: E402, F403
