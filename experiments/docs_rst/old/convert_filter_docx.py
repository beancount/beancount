#!/usr/bin/env python3
"""Filter to process docx input."""

__copyright__ = "Copyright (C) 2017-2018, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import sys

import pandocfilters


def caps(key, value, format, meta):
    if key == "BlockQuote":
        print(value, file=sys.stderr)


if __name__ == "__main__":
    pandocfilters.toJSONFilter(caps)
