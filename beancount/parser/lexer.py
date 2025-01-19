"""Beancount syntax lexer."""

from __future__ import annotations

__copyright__ = "Copyright (C) 2014-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import contextlib
import io
from typing import TYPE_CHECKING
from typing import NamedTuple

from beancount.core.data import Meta
from beancount.core.data import new_metadata
from beancount.parser import _parser

if TYPE_CHECKING:
    from beancount.core.data import BeancountError


class LexerError(NamedTuple):
    """A named tuple to represent lexer errors."""

    source: Meta
    message: str
    entry: None = None


class LexBuilder:
    """A builder used only for building lexer objects."""

    def __init__(self) -> None:
        # Errors that occurred during lexing and parsing.
        self.errors: list[BeancountError] = []

    # Note: We could simplify the code by removing this if we could find a good
    # way to have the lexer communicate the error contents to the parser.
    def build_lexer_error(self, filename: str, lineno: int, message):  # {0e31aeca3363}
        """Build a lexer error and appends it to the list of pending errors.

        Args:
          message: The message of the error.
        """
        self.errors.append(LexerError(new_metadata(filename, lineno), str(message)))


def lex_iter(file, builder=None):
    """An iterator that yields all the tokens in the given file.

    Args:
      file: A string, the filename to run the lexer on, or a file object.
      builder: A builder of your choice. If not specified, a LexBuilder is
        used and discarded (along with its errors).
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
            file = ctx.enter_context(open(file, "rb"))
        if builder is None:
            builder = LexBuilder()
        parser = _parser.Parser(builder)
        yield from parser.lex(file)


def lex_iter_string(string, builder=None, **kwargs):
    """An iterator that yields all the tokens in the given string.

    Args:
      string: a str or bytes, the contents of the ledger to be parsed.
    Returns:
      An iterator, see ``lex_iter()`` for details.
    """
    if not isinstance(string, bytes):
        string = string.encode("utf8")
    file = io.BytesIO(string)
    yield from lex_iter(file, builder=builder, **kwargs)
