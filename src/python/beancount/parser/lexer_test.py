"""
Tests for lexer.
"""
import datetime
import functools
import textwrap
import unittest
import re

from beancount.core.amount import D
from beancount.parser import lexer



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
            ('CURRENCY', 8, 'TEST_3', 'TEST_3'),
            ('EOL', 9, '\n', None),
            ('STRING', 9, '"Nice dinner at Mermaid Inn"', 'Nice dinner at Mermaid Inn'),
            ('EOL', 10, '\n', None),
            ('STRING', 10, '""', ''),
            ('EOL', 11, '\n', None),
            ('NUMBER', 11, '123', D('123')),
            ('EOL', 12, '\n', None),
            ('NUMBER', 12, '123.45', D('123.45')),
            ('EOL', 13, '\n', None),
            ('NUMBER', 13, '123.456789', D('123.456789')),
            ('EOL', 14, '\n', None),
            ('NUMBER', 14, '-123', D('-123')),
            ('EOL', 15, '\n', None),
            ('NUMBER', 15, '-123.456789', D('-123.456789')),
            ('EOL', 16, '\n', None),
            ('TAG', 16, '#sometag123', 'sometag123'),
            ('EOL', 17, '\n', None),
            ('LINK', 17, '^sometag123', 'sometag123'),
            ('EOL', 18, '\n', None),
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
    def test_comma_currencies(self, tokens, errors):
        """\
          USD,CAD,AUD
        """
        self.assertEqual([
            ('CURRENCY', 1, 'USD', 'USD'),
            ('COMMA', 1, ',', None),
            ('CURRENCY', 1, 'CAD', 'CAD'),
            ('COMMA', 1, ',', None),
            ('CURRENCY', 1, 'AUD', 'AUD'),
            ('EOL', 2, '\n', None),
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

    @lex_tokens
    def test_currency_number(self, tokens, errors):
        """\
          555.00 CAD.11
        """
        self.assertEqual([
            ('NUMBER', 1, '555.00', D('555.00')),
            ('CURRENCY', 1, 'CAD.11', 'CAD.11'),
            ('EOL', 2, '\n', None),
            ], tokens)

    @lex_tokens
    def test_currency_dash(self, tokens, errors):
        """\
          TEST-DA
        """
        self.assertEqual(1, len(errors))
        # FIXME: Improve the tokenizer not to return 'TEST' here.
        # self.assertEqual([('ERROR', 1, 'TEST-DA', None),
        #                   ('EOL', 2, '\n', None)], tokens)

    @lex_tokens
    def test_bad_date(self, tokens, errors):
        """\
          2013-12-98
        """
        self.assertEqual([
            ('DATE', 1, '2013-12-98', datetime.date(1970, 1, 1)),
            ('EOL', 2, '\n', None),
        ], tokens)
        self.assertTrue(errors)
        self.assertTrue(re.search('out of range', errors[0].message))

    @lex_tokens
    def test_date_followed_by_number(self, tokens, errors):
        """\
          2013-12-228
        """
        # FIXME: Figure out how to parse word boundary properly in lexer.
        # print()
        # print()
        # for tk in tokens: print(tk)
        # print()
        # self.assertEqual([], tokens)

    @lex_tokens
    def test_single_letter_account(self, tokens, errors):
        """\
          Assets:A
        """
        self.assertEqual([
            ('ERROR', 1, 'A', None),
            ('EOL', 2, '\n', None),
        ], tokens)
        self.assertTrue(errors)
        self.assertTrue(re.search('erroneous token', errors[0].message))

    @lex_tokens
    def test_invalid_directive(self, tokens, errors):
        """\
          2008-03-01 check Assets:BestBank:Savings 2340.19 USD
        """
        for t in tokens:
            print(t)
        # self.assertEqual([
        #     ('ERROR', 1, 'A', None),
        #     ('EOL', 2, '\n', None),
        # ], tokens)
        # self.assertTrue(errors)
        # self.assertTrue(re.search('erroneous token', errors[0].message))
