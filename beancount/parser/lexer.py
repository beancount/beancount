"""Beancount syntax lexer.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime
import io
import re
from decimal import Decimal

from beancount.core.data import new_metadata
from beancount.core import account
from beancount.parser import _parser


LexerError = collections.namedtuple('LexerError', 'source message entry')


class LexBuilder:
    """A builder used only for building lexer objects.

    Attributes:
      long_string_maxlines_default: Number of lines for a string to trigger a
          warning. This is meant to help users detecting dangling quotes in
          their source.
    """
    # pylint: disable=invalid-name

    def __init__(self):
        # A regexp for valid account names.
        self.account_regexp = re.compile(account.ACCOUNT_RE)

        # A regexp for valid numbers.
        self.number_regexp = re.compile(r'\d{0,3}(,\d{3})+(\.\d+)?$')

        # Errors that occurred during lexing and parsing.
        self.errors = []

        # Default number of lines in string literals.
        self.long_string_maxlines_default = 64

    # Note: We could simplify the code by removing this if we could find a good
    # way to have the lexer communicate the error contents to the parser.
    def build_lexer_error(self, filename, lineno, message, exc_type=None): # {0e31aeca3363}
        """Build a lexer error and appends it to the list of pending errors.

        Args:
          message: The message of the error.
          exc_type: An exception type, if an exception occurred.
        """
        if not isinstance(message, str):
            message = str(message)
        if exc_type is not None:
            message = '{}: {}'.format(exc_type.__name__, message)
        self.errors.append(
            LexerError(new_metadata(filename, lineno), message, None))


    def DATE(self, year, month, day):
        """Process a DATE token.

        Args:
          year: integer year.
          month: integer month.
          day: integer day
        Returns:
          A new datetime object.
        """
        return datetime.date(year, month, day)

    def ACCOUNT(self, account_name):
        """Process an ACCOUNT token.

        Args:
          account_name: a str, the name of an account.
        Returns:
          A string, the name of the account.
        """
        return account_name

    def CURRENCY(self, currency_name):
        """Process a CURRENCY token.

        Args:
          currency_name: the name of the currency.
        Returns:
          A new currency object; for now, these are simply represented
          as the currency name.
        """
        return currency_name

    def STRING(self, string):
        """Process a STRING token.

        Args:
          string: the string to process.
        Returns:
          The string. Nothing to be done or cleaned up. Eventually we might
          do some decoding here.
        """
        # If a multiline string, warm over a certain number of lines.
        if '\n' in string:
            num_lines = string.count('\n') + 1
            if num_lines > self.long_string_maxlines_default:
                raise ValueError("String too long ({} lines)".format(num_lines))
        return string

    def NUMBER(self, number):
        """Process a NUMBER token. Convert into Decimal.

        Args:
          number: a str, the number to be converted.
        Returns:
          A Decimal instance built of the number string.
        """
        # Note: We don't use D() for efficiency here.
        # The lexer will only yield valid number strings.
        if ',' in number:
            # Check for a number with commas as thousands separator.
            if not self.number_regexp.match(number):
                raise ValueError("Invalid number format: '{}'".format(number))
            # Remove commas.
            number = number.replace(',', '')
        return Decimal(number)

    def TAG(self, tag):
        """Process a TAG token.

        Args:
          tag: a str, the tag to be processed.
        Returns:
          The tag string itself. For now we don't need an object to represent
          those; keeping it simple.
        """
        return tag

    def LINK(self, link):
        """Process a LINK token.

        Args:
          link: a str, the name of the string.
        Returns:
          The link string itself. For now we don't need to represent this by
          an object.
        """
        return link

    def KEY(self, ident):
        """Process an identifier token.

        Args:
          ident: a str, the name of the key string.
        Returns:
          The link string itself. For now we don't need to represent this by
          an object.
        """
        return ident


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
