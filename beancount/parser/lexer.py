"""Beancount syntax lexer.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import io

from beancount.core.data import new_metadata
from beancount.parser import _parser


LexerError = collections.namedtuple('LexerError', 'source message entry')


class LexBuilder:
    """A builder used only for building lexer objects.

    Attributes:
      long_string_maxlines_default: Number of lines for a string to trigger a
          warning. This is meant to help users detecting dangling quotes in
          their source.
    """

    def __init__(self):
        # Errors that occurred during lexing and parsing.
        self.errors = []

    # Note: We could simplify the code by removing this if we could find a good
    # way to have the lexer communicate the error contents to the parser.
    def build_lexer_error(self, filename, lineno, message): # {0e31aeca3363}
        """Build a lexer error and appends it to the list of pending errors.

        Args:
          message: The message of the error.
          exc_type: An exception type, if an exception occurred.
        """
        self.errors.append(
            LexerError(new_metadata(filename, lineno), str(message), None))


def lex_iter(file, builder=None, encoding=None):
    """An iterator that yields all the tokens in the given file.

    Args:
      file: A string, the filename to run the lexer on, or a file object.
      builder: A builder of your choice. If not specified, a LexBuilder is
        used and discarded (along with its errors).
      encoding: A string (or None), the default encoding to use for strings.
    Yields:
      Tuples of the token (a string), the matched text (a string), and the line
      no (an integer).
    """
    # It would be more appropriate here to check for io.RawIOBase but
    # that does not work for io.BytesIO despite it implementing the
    # readinto() method.
    if not isinstance(file, io.IOBase):
        file = open(file, 'rb')
    if builder is None:
        builder = LexBuilder()
    parser = _parser.Parser(builder)
    yield from parser.lex(file, encoding=encoding)


def lex_iter_string(string, builder=None, encoding=None):
    """Parse an input string and print the tokens to an output file.

    Args:
      input_string: a str or bytes, the contents of the ledger to be parsed.
      builder: A builder of your choice. If not specified, a LexBuilder is
        used and discarded (along with its errors).
      encoding: A string (or None), the default encoding to use for strings.
    Returns:
      A iterator on the string. See lex_iter() for details.
    """
    if not isinstance(string, bytes):
        string = string.encode('utf8')
    file = io.BytesIO(string)
    yield from lex_iter(file, builder, encoding)
