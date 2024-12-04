"""Beancount, a double-entry bookkeeping software."""

import importlib.metadata

__copyright__ = "Copyright (C) 2013-2014, 2016-2018,2021  Martin Blais"
__license__ = "GNU GPLv2"

# Read in the VERSION number from package data.
__version__ = importlib.metadata.version("beancount")

# print(__version__)

# Expose the public API.
# from .api import *  # noqa: E402, F403
