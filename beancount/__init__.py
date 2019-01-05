"""Beancount, a double-entry bookkeeping software.

This is v2, a complete rewrite of Beancount v1, simplified and improved
drastically.
"""
__copyright__ = "Copyright (C) 2013-2014, 2016-2018  Martin Blais"
__license__ = "GNU GPLv2"


# Check the version requirements.
import sys
if (sys.version_info.major, sys.version_info.minor) < (3, 3):
    raise ImportError("Python 3.3 or above is required")


__version__ = '2.2.0-dev'


# Remove annoying warnings in third-party modules.
import warnings
warnings.filterwarnings(
    'ignore', module='lxml', category=DeprecationWarning,
    message='Using or importing the ABCs from')
warnings.filterwarnings(
    'ignore', module='html5lib', category=DeprecationWarning,
    message='Using or importing the ABCs from')
warnings.filterwarnings(
    'ignore', module='bs4', category=DeprecationWarning,
    message='Using or importing the ABCs from')
warnings.filterwarnings(
    'ignore', module='bottle', category=DeprecationWarning,
    message='Flags not at the start of the expression')
warnings.filterwarnings(
    'ignore', module='bottle', category=DeprecationWarning,
    message='invalid escape sequence')
