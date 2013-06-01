"""Load an input file: open the file and parse it, pad, check and validate it.

This file provides convenience routines that do all that's necessary to obtain a
list of entries ready for realization and working with them. This is the most
common entry point.
"""
from beancount2 import utils
from beancount2.parser import parser
from beancount2.core import data
from beancount2.core import realization
from beancount2.core import validation


def load(filename):
    """Parse the input file, pad the entries, check and validate it.
    This also prints out the error messages."""

    # Parse the input file.
    with utils.print_time('parse'):
        contents = parser.parse(filename)
        parse_errors = contents.parse_errors
        data.print_errors(contents.parse_errors)

    # Pad the resulting entries (create synthetic Pad entries to balance checks
    # where desired).
    with utils.print_time('pad'):
        entries, pad_errors = realization.pad(contents.entries)
        data.print_errors(pad_errors)

    with utils.print_time('check'):
        entries, check_errors = realization.check(entries)
        data.print_errors(check_errors)

    # Validate the list of entries.
    with utils.print_time('validation'):
        valid_errors = validation.validate(entries, contents.accounts)
        data.print_errors(valid_errors)

    return contents, entries
