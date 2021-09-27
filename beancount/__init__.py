"""Beancount, a double-entry bookkeeping software.
"""
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


# Remove annoying warnings in third-party modules.
# TODO(blais): Review this, we may not need these anymore.
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


# V3 public definitions.
#
# The future of Beancount (v3) will bind the majority of public symbols on the
# root package, so that one can seamlessly use it like e.g., numpy, with
#
#    import beancount as bn
#
#    bn.load_file(...)

try:
    from beancount.cparser import extmodule
    parse = extmodule.parse
except ImportError:
    warnings.warn("Unable to import v3 extension module.")
