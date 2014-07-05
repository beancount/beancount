"""
Tests for lexer.
"""
import datetime
import textwrap
import unittest

from beancount.parser import lexer


class TestLexer(unittest.TestCase):
    """Test output of the lexer."""

    def test_lex_iter(self):
        tokens = list(lexer.lex_iter_string(textwrap.dedent("""\
          2013-05-18 * "Nice dinner at Mermaid Inn"
        """)))
        self.assertEqual([
            ('DATE', 1, '2013-05-18', datetime.date(2013, 5, 18)),
            ('FLAG', 1, '*', None),
            ('STRING', 1, '"Nice dinner at Mermaid Inn"', 'Nice dinner at Mermaid Inn'),
            ('EOL', 2, '\n', None),
            ], tokens)


__incomplete__ = True
