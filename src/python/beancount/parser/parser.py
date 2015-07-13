"""Beancount syntax parser.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import functools
import inspect
import textwrap
import io
from os import path
from datetime import date

from beancount.core.number import ZERO
from beancount.core.amount import Amount
from beancount.core import display_context
from beancount.core.position import Lot
from beancount.core.position import Position
from beancount.core.data import Transaction
from beancount.core.data import Balance
from beancount.core.data import Open
from beancount.core.data import Close
from beancount.core.data import Commodity
from beancount.core.data import Pad
from beancount.core.data import Event
from beancount.core.data import Price
from beancount.core.data import Note
from beancount.core.data import Document
from beancount.core.data import new_metadata
from beancount.core.data import Posting
from beancount.core.data import BOOKING_METHODS
from beancount.core.interpolate import balance_incomplete_postings
from beancount.core.interpolate import compute_residual
from beancount.core.interpolate import infer_tolerances

from beancount.parser import _parser
from beancount.parser import grammar
from beancount.parser import printer
from beancount.parser import hashsrc

from beancount.parser.grammar import ParserError
from beancount.parser.grammar import ParserSyntaxError
from beancount.parser.grammar import DeprecatedError
# pylint: disable=unused-import
ParserError, ParserSyntaxError, DeprecatedError # pyflakes


# When importing the module, always check that the compiled source matched the
# installed source.
hashsrc.check_parser_source_files()



def parse_file(filename, **kw):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts.

    Args:
      filename: the name of the file to be parsed.
      kw: a dict of keywords to be applied to the C parser.
    Returns:
      A tuple of (
        list of entries parsed in the file,
        list of errors that were encountered during parsing, and
        a dict of the option values that were parsed from the file.)
    """
    abs_filename = path.abspath(filename) if filename else None
    builder = grammar.Builder(abs_filename)
    _parser.parse_file(filename, builder, **kw)
    return builder.finalize()

# Alias, for compatibility.
# pylint: disable=invalid-name
parse = parse_file


def parse_string(string, **kw):
    """Parse a beancount input file and return Ledger with the list of
    transactions and tree of accounts.

    Args:
      string: a str, the contents to be parsed instead of a file's.
      **kw: See parse.c. This function parses out 'dedent' which removes
        whitespace from the front of the text (default is False).
    Return:
      Same as the output of parse_file().
    """
    if kw.pop('dedent', None):
        string = textwrap.dedent(string)
    builder = grammar.Builder(None)
    _parser.parse_string(string, builder, **kw)
    builder.options['filename'] = '<string>'
    return builder.finalize()


def parsedoc(fun, no_errors=False):
    """Decorator that parses the function's docstring as an argument.

    Note that this only runs the parser on the tests, not the loader, so is no
    validation nor fixup applied to the list of entries.

    Args:
      fun: the function object to be decorated.
      no_errors: A boolean, true if we should assert that there are no errors.
    Returns:
      The decorated function.
    """
    filename = inspect.getfile(fun)
    lines, lineno = inspect.getsourcelines(fun)

    # decorator line + function definition line (I realize this is largely
    # imperfect, but it's only for reporting in our tests) - empty first line
    # stripped away.
    lineno += 1

    @functools.wraps(fun)
    def wrapper(self):
        assert fun.__doc__ is not None, (
            "You need to insert a docstring on {}".format(fun.__name__))
        entries, errors, options_map = parse_string(fun.__doc__,
                                                    report_filename=filename,
                                                    report_firstline=lineno,
                                                    dedent=True)
        if no_errors:
            if errors:
                oss = io.StringIO()
                printer.print_errors(errors, file=oss)
                self.fail("Unexpected errors:\n{}".format(oss.getvalue()))
            return fun(self, entries, options_map)
        else:
            return fun(self, entries, errors, options_map)

    wrapper.__input__ = wrapper.__doc__
    wrapper.__doc__ = None
    return wrapper


def parsedoc_noerrors(fun):
    """Decorator like parsedoc but that further ensures no errors.

    Args:
      fun: the function object to be decorated.
    Returns:
      The decorated function.
    """
    return parsedoc(fun, no_errors=True)
