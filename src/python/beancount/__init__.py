"""Beancount, a double-entry bookkeeping software.

This is v2, a complete rewrite of Beancount v1, simplified and improved
drastically.
"""
__author__ = "Martin Blais <blais@furius.ca>"


# Check the version requirements.
import sys
if (sys.version_info.major, sys.version_info.minor) < (3, 3):
    raise ImportError("Python 3.3 or above is required")
