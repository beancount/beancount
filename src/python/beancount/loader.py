"""Loader code. This is the main entry point to load up a file.
"""
import functools
import textwrap

from beancount import utils
from beancount.core import data
from beancount.parser import parser
from beancount.parser import documents
from beancount.ops import pad
from beancount.ops import validation
from beancount.ops import prices
from beancount.ops import check


def load(filename,
         add_unrealized_gains=False,
         do_print_errors=False,
         quiet=False,
         parse_method='filename'):
    """Load an input file: open the file and parse it, pad, check and validate it.
    This also optionally prints out the error messages.

    This file provides convenience routines that do all that's necessary to obtain a
    list of entries ready for realization and working with them. This is the most
    common entry point.
    """

    # Parse the input file.
    if parse_method == 'filename':
        parse_fun = parser.parse
    elif parse_method == 'string':
        parse_fun = parser.parse_string
    else:
        raise NotImplementedError
    with utils.print_time('parse', quiet):
        entries, parse_errors, options = parse_fun(filename)

    # Pad the resulting entries (create synthetic Pad entries to balance checks
    # where desired).
    with utils.print_time('pad', quiet):
        entries, pad_errors = pad.pad(entries)

    with utils.print_time('check', quiet):
        entries, check_errors = check.check(entries)

    # Process the document entries and find documents automatically.
    with utils.print_time('documents', quiet):
        entries, doc_errors = documents.process_documents(entries,
                                                          filename,
                                                          options['documents'])

    # Validate the list of entries.
    with utils.print_time('validate', quiet):
        valid_errors = validation.validate(entries)

    # Add unrealized gains.
    with utils.print_time('unrealized', quiet):
        entries = prices.unrealized_gains(entries, options['account_unrealized'])

    # Print out the list of errors.
    errors = parse_errors + pad_errors + check_errors + valid_errors + doc_errors
    if do_print_errors:
        data.print_errors(errors)

    return entries, errors, options


def loaddoc(fun):
    """A decorator that will load the docstring and call the wrapped function with
    the results."""
    @functools.wraps(fun)
    def wrapper(self):
        contents = textwrap.dedent(fun.__doc__)
        entries, errors, options = load(contents, parse_method='string')
        return fun(self, entries, errors, options)
    return wrapper
