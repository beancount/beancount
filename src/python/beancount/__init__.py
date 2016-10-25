"""Beancount, a double-entry bookkeeping software.

This is v2, a complete rewrite of Beancount v1, simplified and improved
drastically.
"""
__author__ = "Martin Blais <blais@furius.ca>"


# Check the version requirements.
import sys
if (sys.version_info.major, sys.version_info.minor) < (3, 3):
    raise ImportError("Python 3.3 or above is required")

# FIXME: Install an ugly, temporary global dispatch kludge on the inclusion of
# cost-dates in test code. This needs to get removed once the 'booking' branch
# is merged. This is only present in order to make sure the unit tests work with
# default of "SIMPLE" or "FULL".

from beancount.parser.options import OPTIONS_DEFAULTS
def _X(value, default=None):
    return (value
            if OPTIONS_DEFAULTS["booking_algorithm"] == 'FULL'
            else default)

import builtins
builtins._X = _X
