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


def load(filename, do_print_errors=False):
    """Parse the input file, pad the entries, check and validate it.
    This also prints out the error messages."""

    # Parse the input file.
    with utils.print_time('parse'):
        contents = parser.parse(filename)
        parse_errors = contents.parse_errors
        options = contents.options

    # Pad the resulting entries (create synthetic Pad entries to balance checks
    # where desired).
    with utils.print_time('pad'):
        entries, pad_errors = realization.pad(contents.entries)

    with utils.print_time('check'):
        entries, check_errors = realization.check(entries)

    # Validate the list of entries.
    with utils.print_time('validate'):
        valid_errors = validation.validate(entries, contents.accounts)

    errors = parse_errors + pad_errors + check_errors + valid_errors

    if do_print_errors:
        data.print_errors(errors)

    return entries, errors, options
