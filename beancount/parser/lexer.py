"""Beancount syntax lexer.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime
import re
import tempfile

from beancount.core import data
from beancount.core import account
from beancount.core.number import Decimal
from beancount.parser import _parser


LexerError = collections.namedtuple('LexerError', 'source message entry')


class LexBuilder(object):
    """A builder used only for building lexer objects.

    Attributes:
      long_string_maxlines_default: Number of lines for a string to trigger a
          warning. This is meant to help users detecting dangling quotes in
          their source.
    """
    # pylint: disable=invalid-name

    def __init__(self):
        # A mapping of all the accounts created.
        self.accounts = {}

        # A regexp for valid account names.
        self.account_regexp = re.compile(account.ACCOUNT_RE)

        # A set of all the commodities that we have seen in the file.
        self.commodities = set()

        # Errors that occurred during lexing and parsing.
        self.errors = []

        # Default number of lines as threshold to warn over long strings.
        self.long_string_maxlines_default = 64

    def get_invalid_account(self):
        """Return the name of an invalid account placeholder.

        When an account name is not deemed a valid one, replace it by
        this account name. This can be overridden by the parser to
        take into account the options.

        Returns:
          A string, the name of the root/type for invalid account names.
        """
        return 'Equity:InvalidAccountName'

    def get_lexer_location(self):
        return data.new_metadata(_parser.get_yyfilename(),
                                 _parser.get_yylineno())

    # Note: We could simplify the code by removing this if we could find a good
    # way to have the lexer communicate the error contents to the parser.
    def build_lexer_error(self, message, exc_type=None): # {0e31aeca3363}
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
            LexerError(self.get_lexer_location(), message, None))

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

        This function attempts to reuse an existing account if one exists,
        otherwise creates one on-demand.

        Args:
          account_name: a str, the valid name of an account.
        Returns:
          A string, the name of the account.
        """
        # Check account name validity.
        if not self.account_regexp.match(account_name):
            raise ValueError("Invalid account name: {}".format(account_name))

        # Reuse (intern) account strings as much as possible. This potentially
        # reduces memory usage a fair bit, because these strings are repeated
        # liberally.
        return self.accounts.setdefault(account_name, account_name)

    def CURRENCY(self, currency_name):
        """Process a CURRENCY token.

        Args:
          currency_name: the name of the currency.
        Returns:
          A new currency object; for now, these are simply represented
          as the currency name.
        """
        self.commodities.add(currency_name)
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
                # This is just a warning; accept the string anyhow.
                self.errors.append(
                    LexerError(
                        self.get_lexer_location(),
                        "String too long ({} lines); possible error".format(num_lines),
                        None))
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
            # Extract the integer part and check the commas match the
            # locale-aware formatted version. This
            match = re.match(r"([\d,]*)(\.\d*)?$", number)
            if not match:
                # This path is never taken because the lexer will parse a comma
                # in the fractional part as two NUMBERs with a COMMA token in
                # between.
                self.errors.append(
                    LexerError(self.get_lexer_location(),
                               "Invalid number format: '{}'".format(number), None))
            else:
                int_string, float_string = match.groups()
                reformatted_number = r"{:,.0f}".format(int(int_string.replace(",", "")))
                if int_string != reformatted_number:
                    self.errors.append(
                        LexerError(self.get_lexer_location(),
                                   "Invalid commas: '{}'".format(number), None))

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
    if isinstance(file, str):
        filename = file
    else:
        filename = file.name
    if builder is None:
        builder = LexBuilder()
    _parser.lexer_initialize(filename, builder, encoding)
    try:
        while 1:
            token_tuple = _parser.lexer_next()
            if token_tuple is None:
                break
            yield token_tuple
    finally:
        _parser.lexer_finalize()


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
    tmp_file = tempfile.NamedTemporaryFile('w' if isinstance(string, str) else 'wb')
    tmp_file.write(string)
    tmp_file.flush()
    # Note: We pass in the file object in order to keep it alive during parsing.
    return lex_iter(tmp_file, builder, encoding)
