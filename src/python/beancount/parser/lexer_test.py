"""
Tests for lexer.
"""
import datetime
import functools
import textwrap
import unittest

from beancount.core.amount import Decimal
from beancount.parser import lexer
from beancount.parser import printer



class TestLexer(unittest.TestCase):
    """Test output of the lexer."""

    def lex_tokens(fun):
        @functools.wraps(fun)
        def wrapped(self):
            string = fun.__doc__
            builder = lexer.LexBuilder()
            tokens = list(lexer.lex_iter_string(textwrap.dedent(string),
                                                builder))
            return fun(self, tokens, builder.errors)
        wrapped.__doc__ = None
        return wrapped

    @lex_tokens
    def test_lex_iter(self, tokens, errors):
        """\
          2013-05-18
          2014-01-02
          Assets:US:Bank:Checking
          Liabilities:US:Bank:CreditCard
          Other:Bank
          USD
          GOOG
          TEST-3
          TEST_3
          "Nice dinner at Mermaid Inn"
          ""
          123
          123.45
          123.456789
          -123
          -123.456789
          #sometag123
          ^sometag123
        """
        self.assertEqual([
            ('DATE', 1, '2013-05-18', datetime.date(2013, 5, 18)),
            ('EOL', 2, '\n', None),
            ('DATE', 2, '2014-01-02', datetime.date(2014, 1, 2)),
            ('EOL', 3, '\n', None),
            ('ACCOUNT', 3, 'Assets:US:Bank:Checking', 'Assets:US:Bank:Checking'),
            ('EOL', 4, '\n', None),
            ('ACCOUNT', 4, 'Liabilities:US:Bank:CreditCard',
             'Liabilities:US:Bank:CreditCard'),
            ('EOL', 5, '\n', None),
            ('ACCOUNT', 5, 'Other:Bank', 'Other:Bank'),
            ('EOL', 6, '\n', None),
            ('CURRENCY', 6, 'USD', 'USD'),
            ('EOL', 7, '\n', None),
            ('CURRENCY', 7, 'GOOG', 'GOOG'),
            ('EOL', 8, '\n', None),
            ('CURRENCY', 8, 'TEST', 'TEST'),
            ('NUMBER', 8, '-3', Decimal('-3')),
            ('EOL', 9, '\n', None),
            ('CURRENCY', 9, 'TEST_3', 'TEST_3'),
            ('EOL', 10, '\n', None),
            ('STRING', 10, '"Nice dinner at Mermaid Inn"', 'Nice dinner at Mermaid Inn'),
            ('EOL', 11, '\n', None),
            ('STRING', 11, '""', ''),
            ('EOL', 12, '\n', None),
            ('NUMBER', 12, '123', Decimal('123')),
            ('EOL', 13, '\n', None),
            ('NUMBER', 13, '123.45', Decimal('123.45')),
            ('EOL', 14, '\n', None),
            ('NUMBER', 14, '123.456789', Decimal('123.456789')),
            ('EOL', 15, '\n', None),
            ('NUMBER', 15, '-123', Decimal('-123')),
            ('EOL', 16, '\n', None),
            ('NUMBER', 16, '-123.456789', Decimal('-123.456789')),
            ('EOL', 17, '\n', None),
            ('TAG', 17, '#sometag123', 'sometag123'),
            ('EOL', 18, '\n', None),
            ('LINK', 18, '^sometag123', 'sometag123'),
            ('EOL', 19, '\n', None),
            ], tokens)

    @lex_tokens
    def test_lex_indent(self, tokens, errors):
        """\
          2014-07-05 *
            Equity:Something
        """
        self.assertEqual([
            ('DATE', 1, '2014-07-05', datetime.date(2014, 7, 5)),
            ('FLAG', 1, '*', None),
            ('EOL', 2, '\n', None),
            ('INDENT', 2, '  ', None),
            ('ACCOUNT', 2, 'Equity:Something', 'Equity:Something'),
            ('EOL', 3, '\n', None),
            ], tokens)

    @lex_tokens
    def test_number_okay(self, tokens, errors):
        """\
          1001 USD
          1002.00 USD
          -1001 USD
          -1002.00 USD
          +1001 USD
          +1002.00 USD
        """
        self.assertFalse(errors)

    @lex_tokens
    def test_number_space(self, tokens, errors):
        """\
          - 1002.00 USD
        """
        self.assertTrue(errors)

    @lex_tokens
    def test_number_dots(self, tokens, errors):
        """\
          1.234.00 USD
        """
        self.assertTrue(errors)
