"""Beancount syntax lexer.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import contextlib
import io
import os

from beancount.core.data import new_metadata
from beancount.parser import _parser
from beancount.utils import encryption


LexerError = collections.namedtuple('LexerError', 'source message entry')


class LexBuilder:
    """A builder used only for building lexer objects."""

    def __init__(self):
        # Errors that occurred during lexing and parsing.
        self.errors = []
        # Included file paths.
        self.includes = []

    # Note: We could simplify the code by removing this if we could find a good
    # way to have the lexer communicate the error contents to the parser.
    def build_lexer_error(self, filename, lineno, message): # {0e31aeca3363}
        """Build a lexer error and appends it to the list of pending errors.

        Args:
          message: The message of the error.
        """
        self.errors.append(
            LexerError(new_metadata(filename, lineno), str(message), None))

    def include(self, filename, parent=None):
        if not os.path.isabs(filename):
            # If the filesystem path of the included file is not
            # absolute, it must be resolved relative to the path of
            # the file including it.
            if not parent or parent == "<string>":
                parent = os.getcwd()
            filename = os.path.join(os.path.dirname(parent), filename)

        if encryption.is_encrypted_file(filename):
            # read_encrypted_file() decodes the GPG output and we
            # encode it again here. This could be easily avoided. We
            # could even avoid loading all the plain text in memory
            # returning directly the GPG stdout file descriptor
            # here. To check for errors we would need to wrap it
            # similarly to what os.popen() does. Leave this
            # optimizations for later.
            content = encryption.read_encrypted_file(filename).encode('utf8')
            fd = io.BytesIO(content)
        else:
            fd = open(filename, 'rb')

        # Keep track of the included files and perform the correct
        # cache invalidation when the ledger is reloaded. This adds
        # the included file path to the list even if opening it will
        # result in an error later. Maybe this needs to be revisited.
        self.includes.append(filename)

        return filename, fd


def lex_iter(file, builder=None, encoding=None):
    """An iterator that yields all the tokens in the given file.

    Args:
      file: A string, the filename to run the lexer on, or a file object.
      builder: A builder of your choice. If not specified, a LexBuilder is
        used and discarded (along with its errors).
      encoding: A string (or None), the default encoding to use for strings.
    Yields:
      All the tokens in the input file as ``(token, lineno, text,
      value)`` tuples where ``token`` is a string representing the
      token kind, ``lineno`` is the line number in the input file
      where the token was matched, ``mathed`` is a bytes object
      containing the exact text matched, and ``value`` is the semantic
      value of the token or None.
    """
    with contextlib.ExitStack() as ctx:
        # It would be more appropriate here to check for io.RawIOBase but
        # that does not work for io.BytesIO despite it implementing the
        # readinto() method.
        if not isinstance(file, io.IOBase):
            file = ctx.enter_context(open(file, 'rb'))
        if builder is None:
            builder = LexBuilder()
        parser = _parser.Parser(builder)
        yield from parser.lex(file, encoding=encoding)


def lex_iter_string(string, builder=None, **kwargs):
    """An iterator that yields all the tokens in the given string.

    Args:
      string: a str or bytes, the contents of the ledger to be parsed.
    Returns:
      An iterator, see ``lex_iter()`` for details.
    """
    if not isinstance(string, bytes):
        string = string.encode('utf8')
    file = io.BytesIO(string)
    yield from lex_iter(file, builder=builder, **kwargs)
