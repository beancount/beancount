"""Beancount, a double-entry bookkeeping software."""

__copyright__ = "Copyright (C) 2013-2014, 2016-2018,2021  Martin Blais"
__license__ = "GNU GPLv2"


# Check the version requirements.
import sys

if (sys.version_info.major, sys.version_info.minor) < (3, 3):
    raise ImportError("Python 3.3 or above is required")


# Read in the VERSION number from package data.
from os import path

with open(path.join(path.dirname(__file__), "VERSION")) as version_file:
    __version__ = version_file.read().strip()


# Expose the public API.
del path
del sys
from .api import *  # noqa: E402, F403
