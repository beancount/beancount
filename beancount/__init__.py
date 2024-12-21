"""Beancount, a double-entry bookkeeping software."""

__copyright__ = "Copyright (C) 2013-2014, 2016-2018,2021  Martin Blais"
__license__ = "GNU GPLv2"


# Check the version requirements.
import sys


# Read in the VERSION number from package data.
from os import path

with open(path.join(path.dirname(__file__), "VERSION"), encoding="utf-8") as version_file:
    __version__ = version_file.read().strip()


# Expose the public API.
del path
del sys
from .api import *  # noqa: E402, F403
