"""Beancount syntax parser.

IMPORTANT: The parser (and its grammar builder) produces "incomplete"
Transaction objects. This means that some of the data can be missing. Those
incomplete entries are then run through the "booking" routines which find
matching lots for reducing postings and interpolates missing numbers, and in
doing so normalize the entries to "complete" entries.

Spefically, the following pieces of data may be incomplete:

- position = None
  e.g., Assets:Account

- position.number = None, with a non-nil lot
  position.lot = LotSpec(currency='USD', compound_cost=None)
  e.g., Assets:Account  USD

- position.price = Amount(None, None)
  position.lot = LotSpec(currency='USD', compound_cost=None)
  e.g., Assets:Account  100 CAD @

- position.price = Amount(None, currency)
  position.lot = LotSpec(currency='CAD', compound_cost=None)
  e.g., Assets:Account  100 CAD @ USD

- position.number = 1
  position.lot = LotSpec(currency='GOOG', compound_cost=CompoundAmount(100, 5, 'USD'))
  e.g., Assets:Account  1 GOOG {100 # 5 USD}

- position.number = 1
  position.lot = LotSpec(currency='GOOG', compound_cost=CompoundAmount(None, None, 'USD'))
  e.g., Assets:Account  1 GOOG {USD}

- position.number = 1
  position.lot = LotSpec(currency='GOOG', compound_cost=CompoundAmount(None, None, None))
  e.g., Assets:Account  1 GOOG {USD}

(Note that 'posting.position.price = None' is not incomplete, it just indicates
the absence of a price clause.)

For incomplete entries, 'posting.position.lot' does not refer to a Lot instance,
but rather to a LotSpec which needs to get resolved to a Lot. The LotSpec has a
CompountAmount for which the 'number_per' and 'number_total' numbers may be both
missing.

See grammar_test.TestIncompleteInputs for examples and corresponding checks.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import functools
import inspect
import textwrap
import io
import warnings
from os import path

from beancount.parser import _parser
from beancount.parser import grammar
from beancount.parser import booking
from beancount.parser import printer
from beancount.parser import hashsrc
from beancount.core import data

from beancount.parser.grammar import ParserError
from beancount.parser.grammar import ParserSyntaxError
from beancount.parser.grammar import DeprecatedError
# pylint: disable=unused-import
ParserError, ParserSyntaxError, DeprecatedError # pyflakes


# When importing the module, always check that the compiled source matched the
# installed source.
hashsrc.check_parser_source_files()


def has_auto_postings(entries):
    """Detect the presence of elided amounts in Transactions.

    Args:
      entries: A list of directives.
    Returns:
      A boolean, true if there are some auto-postings found.
    """
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        for posting in entry.postings:
            if posting.position is None:
                return True
    return False


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


# FIXME: Deprecate this eventually.
def parsedoc(*args, **kw):
    warnings.warn("parsedoc() is obsolete; use parse_doc() instead.")
    return parse_doc(*args, **kw)

def parse_doc(expect_errors=False, allow_incomplete=False):
    """Factory of decorators that parse the function's docstring as an argument.

    Note that the decorators thus generated only run the parser on the tests,
    not the loader, so is no validation, balance checks, nor plugins applied to
    the parsed text.

    Args:
      expect_errors: A boolean or None, with the following semantics,
        True: Expect errors and fail if there are none.
        False: Expect no errors and fail if there are some.
        None: Do nothing, no check.
      allow_incomplete: A boolean, if true, allow incomplete input. Otherwise
        barf if the input would require interpolation. The default value is set
        not to allow it because we want to minimize the features tests depend on.
    Returns:
      A decorator for test functions.
    """
    def decorator(fun):
        """A decorator that parses the function's docstring as an argument.

        Args:
          fun: the function object to be decorated.
        Returns:
          A decorated test function.
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

            if not allow_incomplete and has_auto_postings(entries):
                self.fail("parse_doc() may not use interpolation.")

            if expect_errors is not None:
                if expect_errors is False and errors:
                    oss = io.StringIO()
                    printer.print_errors(errors, file=oss)
                    self.fail("Unexpected errors found:\n{}".format(oss.getvalue()))
                elif expect_errors is True and not errors:
                    self.fail("Expected errors, none found:")

            return fun(self, entries, errors, options_map)

        wrapper.__input__ = wrapper.__doc__
        wrapper.__doc__ = None
        return wrapper

    return decorator
