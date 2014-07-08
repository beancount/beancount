"""Beancount syntax lexer.
"""
import collections
import datetime
import re
import tempfile

from beancount.core import data
from beancount.core.amount import Decimal
from beancount.parser import _parser


LexerError = collections.namedtuple('LexerError', 'fileloc message entry')


class LexBuilder(object):
    """A builder used only for getting the lexer to pass.
    The methods do nothing."""

    def __init__(self):
        # A mapping of all the accounts created.
        self.accounts = {}

        # A regexp for valid account names.
        self.account_regexp = re.compile('([A-Z][A-Za-z0-9\-]+)(:[A-Z][A-Za-z0-9\-]+)*$')

        # Errors that occurred during lexing and parsing.
        self.errors = []

    def get_lexer_location(self):
        import sys
        print(_parser.get_yyfilename())
        sys.stdout.flush()
        print(_parser.get_yylineno())
        sys.stdout.flush()
        return data.FileLocation(_parser.get_yyfilename(),
                                 _parser.get_yylineno())

    def ERROR(self, string):
        print("ERROR", self.get_lexer_location())
        if 0:
            self.errors.append(
                LexerError(self.get_lexer_location(),
                           "Error token: {}".format(string),
                           None))

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
          A new Account object.
        """
        # Check account name validity.
        if not self.account_regexp.match(account_name):
            self.errors.append(
                LexerError(self.get_lexer_location(),
                           "Invalid account name: {}".format(account_name),
                           None))
            return account.join(self.options['name_equity'], 'InvalidAccountName')

        # Create an account, reusing their strings as we go.
        try:
            account_name = self.accounts[account_name]
        except KeyError:
            self.accounts[account_name] = account_name

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
        return string

    def NUMBER(self, number):
        """Process a NUMBER token. Convert into Decimal.

        Args:
          number: a str, the number to be converted.
        Returns:
          A Decimal instance built of the number string.
        """
        try:
            return Decimal(number)
        except Exception as e:
            self.errors.append(
                LexerError(self.get_lexer_location(),
                           "Error parsing NUMBER for token '{}': {}".format(number, e),
                           None))

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


def lex_iter(file):
    """An iterator that yields all the tokens in the given file.

    Args:
      file: A string, the filename to run the lexer on, or a file object.
    Yields:
      Tuples of the token (a string), the matched text (a string), and the line
      no (an integer).
    """
    if isinstance(file, str):
        filename = file
    else:
        filename = file.name
    _parser.lexer_init(filename, LexBuilder())
    while 1:
        token_tuple = _parser.lexer_next()
        if token_tuple is None:
            break
        yield token_tuple


def lex_iter_string(string):
    """Parse an input string and print the tokens to an output file.

    Args:
      input_string: a str, the contents of the ledger to be parsed.
    Returns:
      A iterator on the string. See lex_iter() for details.
    """
    tmp_file = tempfile.NamedTemporaryFile('w')
    tmp_file.write(string)
    tmp_file.flush()
    return lex_iter(tmp_file) # Note: Pass in the file object in order to keep
                              # it alive during parsing.
