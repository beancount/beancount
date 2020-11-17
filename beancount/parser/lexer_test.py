"""
Tests for lexer.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import functools
import textwrap
import unittest

from beancount.core.number import D
from beancount.parser import lexer


def print_tokens(tokens):
    """A function for printing a list of tokens, for testing.

    Args:
      tokens: A list of token tuples.
    """
    print()
    print(',--------------------------------')
    for token in tokens:
        print('{},'.format(token))
    print('`--------------------------------')


def lex_tokens(fun):
    """Decorator for test functions that will invoke a lexer on them.

    The lexer passes the list of tokens and errors to the test function.

    Args:
      fun: A test function to be decorated.
    Returns:
      The decorated function.
    """
    @functools.wraps(fun)
    def wrapped(self):
        string = fun.__doc__
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(textwrap.dedent(string),
                                            builder))
        return fun(self, tokens, builder.errors)
    wrapped.__doc__ = None
    return wrapped

class TestLexer(unittest.TestCase):
    """Test output of the lexer."""

    maxDiff = None

    @lex_tokens
    def test_lex_iter(self, tokens, errors):
        """\
          2013-05-18 2014-01-02 2014/01/02
          Assets:US:Bank:Checking
          Liabilities:US:Bank:Credit
          Other:Bank
          USD HOOL TEST_D TEST_3 TEST-D TEST-3 NT
          "Nice dinner at Mermaid Inn"
          ""
          123 123.45 123.456789 -123 -123.456789
          #sometag123
          ^sometag123
          somekey:
        """
        self.assertEqual([
            ('DATE', 1, b'2013-05-18', datetime.date(2013, 5, 18)),
            ('DATE', 1, b'2014-01-02', datetime.date(2014, 1, 2)),
            ('DATE', 1, b'2014/01/02', datetime.date(2014, 1, 2)),
            ('EOL', 2, b'\n', None),
            ('ACCOUNT', 2, b'Assets:US:Bank:Checking', 'Assets:US:Bank:Checking'),
            ('EOL', 3, b'\n', None),
            ('ACCOUNT', 3, b'Liabilities:US:Bank:Credit', 'Liabilities:US:Bank:Credit'),
            ('EOL', 4, b'\n', None),
            ('ACCOUNT', 4, b'Other:Bank', 'Other:Bank'),
            ('EOL', 5, b'\n', None),
            ('CURRENCY', 5, b'USD', 'USD'),
            ('CURRENCY', 5, b'HOOL', 'HOOL'),
            ('CURRENCY', 5, b'TEST_D', 'TEST_D'),
            ('CURRENCY', 5, b'TEST_3', 'TEST_3'),
            ('CURRENCY', 5, b'TEST-D', 'TEST-D'),
            ('CURRENCY', 5, b'TEST-3', 'TEST-3'),
            ('CURRENCY', 5, b'NT', 'NT'),
            ('EOL', 6, b'\n', None),
            ('STRING', 6, b'"Nice dinner at Mermaid Inn"', 'Nice dinner at Mermaid Inn'),
            ('EOL', 7, b'\n', None),
            ('STRING', 7, b'""', ''),
            ('EOL', 8, b'\n', None),
            ('NUMBER', 8, b'123', D('123')),
            ('NUMBER', 8, b'123.45', D('123.45')),
            ('NUMBER', 8, b'123.456789', D('123.456789')),
            ('MINUS', 8, b'-', None),
            ('NUMBER', 8, b'123', D('123')),
            ('MINUS', 8, b'-', None),
            ('NUMBER', 8, b'123.456789', D('123.456789')),
            ('EOL', 9, b'\n', None),
            ('TAG', 9, b'#sometag123', 'sometag123'),
            ('EOL', 10, b'\n', None),
            ('LINK', 10, b'^sometag123', 'sometag123'),
            ('EOL', 11, b'\n', None),
            ('KEY', 11, b'somekey', 'somekey'),
            ('COLON', 11, b':', None),
            ('EOL', 12, b'\n', None),
            ], tokens)

    @lex_tokens
    def test_lex_unicode_account(self, tokens, errors):
        """\
          Other:Bank Óthяr:Bあnk
          abc1:abc1 ΑβγⅠ:ΑβγⅠ ابجا:ابجا
        """
        self.assertEqual([
            ('ACCOUNT', 1, b'Other:Bank', 'Other:Bank'),
            ('ACCOUNT', 1, 'Óthяr:Bあnk'.encode('utf8'), 'Óthяr:Bあnk'),
            ('EOL', 2, b'\n', None),
            ('KEY', 2, b'abc1', 'abc1'),
            ('COLON', 2, b':', None),
            ('error', 2, b'abc1', None),
            ('ACCOUNT', 2, 'ΑβγⅠ:ΑβγⅠ'.encode('utf8'), 'ΑβγⅠ:ΑβγⅠ'),
            ('ACCOUNT', 2, 'ابجا:ابجا'.encode('utf8'), 'ابجا:ابجا'),
            ('EOL', 3, b'\n', None),
            ], tokens)

    @lex_tokens
    def test_lex_indent(self, tokens, errors):
        """\
          2014-07-05 *
            Equity:Something
        """
        self.assertEqual([
            ('DATE', 1, b'2014-07-05', datetime.date(2014, 7, 5)),
            ('ASTERISK', 1, b'*', None),
            ('EOL', 2, b'\n', None),
            ('INDENT', 2, b'  ', None),
            ('ACCOUNT', 2, b'Equity:Something', 'Equity:Something'),
            ('EOL', 3, b'\n', None),
            ], tokens)

    @lex_tokens
    def test_comma_currencies(self, tokens, errors):
        """\
          USD,CAD,AUD
        """
        self.assertEqual([
            ('CURRENCY', 1, b'USD', 'USD'),
            ('COMMA', 1, b',', None),
            ('CURRENCY', 1, b'CAD', 'CAD'),
            ('COMMA', 1, b',', None),
            ('CURRENCY', 1, b'AUD', 'AUD'),
            ('EOL', 2, b'\n', None),
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
          1,001 USD
          1,002.00 USD
          -1,001 USD
          -1,002.00 USD
          +1,001 USD
          +1,002.00 USD
        """
        self.assertFalse(errors)

    @lex_tokens
    def test_number_space(self, tokens, errors):
        """\
          - 1002.00 USD
        """
        self.assertFalse(errors)

    @lex_tokens
    def test_number_dots(self, tokens, errors):
        """\
          1.234.00 USD
        """
        self.assertTrue(errors)

    @lex_tokens
    def test_number_no_integer(self, tokens, errors):
        """\
          .2347 USD
        """
        # Note: this is not supported in order to make the lexer regexp more tractable.
        # If we write our own lexer eventually we should be able to accept these.
        self.assertTrue(errors)

    @lex_tokens
    def test_currency_number(self, tokens, errors):
        """\
          555.00 CAD.11
        """
        self.assertEqual([
            ('NUMBER', 1, b'555.00', D('555.00')),
            ('CURRENCY', 1, b'CAD.11', 'CAD.11'),
            ('EOL', 2, b'\n', None),
            ], tokens)

    @lex_tokens
    def test_currency_dash(self, tokens, errors):
        """\
          TEST-DA
        """
        self.assertEqual([
            ('CURRENCY', 1, b'TEST-DA', 'TEST-DA'),
            ('EOL', 2, b'\n', None),
            ], tokens)
        self.assertEqual(0, len(errors))

    @lex_tokens
    def test_bad_date(self, tokens, errors):
        """\
          2013-12-98
        """
        self.assertEqual([
            ('error', 1, b'2013-12-98', None),
            ('EOL', 2, b'\n', None),
        ], tokens)
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, '(out of range|month must be)')

    @lex_tokens
    def test_date_followed_by_number(self, tokens, errors):
        """\
          2013-12-228
        """
        self.assertEqual([
            ('error', 1, b'2013-12-228', None),
            ('EOL', 2, b'\n', None),
            ], tokens)

    @lex_tokens
    def test_single_letter_account(self, tokens, errors):
        """\
          Assets:A
        """
        self.assertEqual([
            ('ACCOUNT', 1, b'Assets:A', 'Assets:A'),
            ('EOL', 2, b'\n', None),
        ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_account_names_with_numbers(self, tokens, errors):
        """\
          Assets:Vouchers:99Ranch
          Assets:99Test
          Assets:signals
        """
        self.assertEqual([
            ('ACCOUNT', 1, b'Assets:Vouchers:99Ranch', 'Assets:Vouchers:99Ranch'),
            ('EOL', 2, b'\n', None),
            ('ACCOUNT', 2, b'Assets:99Test', 'Assets:99Test'),
            ('EOL', 3, b'\n', None),
            ('error', 3, b'Assets:signals', None),
            ('EOL', 4, b'\n', None),
        ], tokens)
        self.assertEqual(1, len(errors))

    @lex_tokens
    def test_account_names_with_dash(self, tokens, errors):
        """\
          Equity:Beginning-Balances
        """
        self.assertEqual([
            ('ACCOUNT', 1, b'Equity:Beginning-Balances', 'Equity:Beginning-Balances'),
            ('EOL', 2, b'\n', None),
        ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_invalid_directive(self, tokens, errors):
        """\
          2008-03-01 check Assets:BestBank:Savings 2340.19 USD
        """
        self.assertEqual([
            ('DATE', 1, b'2008-03-01', datetime.date(2008, 3, 1)),
            ('error', 1, b'check', None),
            ('ACCOUNT', 1, b'Assets:BestBank:Savings', 'Assets:BestBank:Savings'),
            ('NUMBER', 1, b'2340.19', D('2340.19')),
            ('CURRENCY', 1, b'USD', 'USD'),
            ('EOL', 2, b'\n', None),
            ], tokens)
        self.assertTrue(errors)
        self.assertRegex(errors[0].message, r'\bcheck\b')

    def test_string_too_long_warning(self):
        test_input = """
          ;; This is a typical error that should get detected for long strings.
          2014-01-01 note Assets:Temporary "Bla bla" "
          2014-02-01 open Liabilities:US:BankWithLongName:Credit-Card:Account01
        """ + "\n" * 64 + """
          2014-02-02 note Assets:Temporary "Bla bla"
        """
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(textwrap.dedent(test_input), builder))
        self.assertLessEqual(1, len(builder.errors))
        self.assertEqual(builder.errors[0].message,
                         'ValueError: String too long (68 lines)')

    def test_very_long_string(self):
        # This tests lexing with a string of 256k.
        test_input = '"' + ('1234567890ABCDEF' * (256*64)) + '"'
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(textwrap.dedent(test_input), builder))
        self.assertEqual(tokens[0][3], test_input[1:-1])
        self.assertLessEqual(0, len(builder.errors))

    @lex_tokens
    def test_no_final_newline(self, tokens, errors):
        """\
          2014-01-01 open Assets:Temporary \
        """
        self.assertEqual([
            ('DATE', 1, b'2014-01-01', datetime.date(2014, 1, 1)),
            ('OPEN', 1, b'open', None),
            ('ACCOUNT', 1, b'Assets:Temporary', 'Assets:Temporary'),
            ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_string_escaped(self, tokens, errors):
        r'''
          "The Great \"Juju\""
          "The Great \t\n\r\f\b"
        '''
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('STRING', 2, br'"The Great \"Juju\""', 'The Great "Juju"'),
            ('EOL', 3, b'\n', None),
            ('STRING', 3, br'"The Great \t\n\r\f\b"', 'The Great \t\n\r\x0c\x08'),
            ('EOL', 4, b'\n', None),
            ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_string_newline(self, tokens, errors):
        '"The Great\nJuju"'
        # Note that this test contains an _actual_ newline, not an escape one as
        # in the previous test. This should allow us to parse multiline strings.
        self.assertEqual([
            ('STRING', 2, b'"The Great\nJuju"', 'The Great\nJuju'),
            ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_string_newline_long(self, tokens, errors):
        '''
        "Forty
        world
        leaders
        and
        hundreds"
        '''
        # Note that this test contains an _actual_ newline, not an escape one as
        # in the previous test. This should allow us to parse multiline strings.
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('STRING', 6,
             b'"Forty\nworld\nleaders\nand\nhundreds"',
             'Forty\nworld\nleaders\nand\nhundreds'),
            ('EOL', 7, b'\n', None),
            ], tokens)
        self.assertFalse(errors)

    def test_string_newline_toolong(self):
        # Testing a string that busts the limits.
        line = 'a' * 127 + '\n'
        string = '"' + line * 128 + '"\n'
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(string, builder))
        self.assertTrue(tokens[0], 'error')
        self.assertTrue(tokens[1], 'EOL')

    @lex_tokens
    def test_popmeta(self, tokens, errors):
        '''
        popmeta location:
        '''
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('POPMETA', 2, b'popmeta', None),
            ('KEY', 2, b'location', 'location'),
            ('COLON', 2, b':', None),
            ('EOL', 3, b'\n', None),
        ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_null_true_false(self, tokens, errors):
        '''
        TRUE FALSE NULL
        '''
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('BOOL', 2, b'TRUE', None),
            ('BOOL', 2, b'FALSE', None),
            ('NONE', 2, b'NULL', None),
            ('EOL', 3, b'\n', None),
        ], tokens)
        self.assertFalse(errors)


class TestIgnoredLines(unittest.TestCase):

    @lex_tokens
    def test_ignored__long_comment(self, tokens, errors):
        """
        ;; Long comment line about something something.
        """
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('EOL', 3, b'\n', None),
            ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_ignored__indented_comment(self, tokens, errors):
        """
        option "title" "The Title"
          ;; Something something.
        """
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('OPTION', 2, b'option', None),
            ('STRING', 2, b'"title"', 'title'),
            ('STRING', 2, b'"The Title"', 'The Title'),
            ('EOL', 3, b'\n', None),
            ('INDENT', 3, b'  ', None),
            ('EOL', 4, b'\n', None),
        ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_ignored__something_else(self, tokens, errors):
        """
        Regular prose appearing mid-file which starts with a flag character.
        """
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('EOL', 3, b'\n', None),
            ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_ignored__something_else_non_flag(self, tokens, errors):
        """
        Xxx this sentence starts with a non-flag character.
        """
        self.assertTrue(errors)

    @lex_tokens
    def test_ignored__org_mode_title(self, tokens, errors):
        """
        * This sentence is an org-mode title.
        """
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('EOL', 3, b'\n', None),
        ], tokens)
        self.assertFalse(errors)

    @lex_tokens
    def test_ignored__org_mode_drawer(self, tokens, errors):
        """
        :PROPERTIES:
        :this: is an org-mode property drawer
        :END:
        """
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('EOL', 3, b'\n', None),
            ('EOL', 4, b'\n', None),
            ('EOL', 5, b'\n', None),
        ], tokens)
        self.assertFalse(errors)


class TestLexerErrors(unittest.TestCase):
    """Test lexer error handling.
    """

    @lex_tokens
    def test_lexer_invalid_token(self, tokens, errors):
        """
          2000-01-01 open ` USD
        """
        self.assertEqual([('EOL', 2, b'\n', None),
                          ('DATE', 2, b'2000-01-01', datetime.date(2000, 1, 1)),
                          ('OPEN', 2, b'open', None),
                          ('error', 2, b'`', None),
                          ('CURRENCY', 2, b'USD', 'USD'),
                          ('EOL', 3, b'\n', None)], tokens)
        self.assertEqual(1, len(errors))

    @lex_tokens
    def test_lexer_exception__recovery(self, tokens, errors):
        """
          2000-13-32 open Assets:Something

          2000-01-02 open Assets:Working
        """
        self.assertEqual([('EOL', 2, b'\n', None),
                          ('error', 2, b'2000-13-32', None),
                          ('OPEN', 2, b'open', None),
                          ('ACCOUNT', 2, b'Assets:Something', 'Assets:Something'),
                          ('EOL', 3, b'\n', None),
                          ('EOL', 4, b'\n', None),
                          ('DATE', 4, b'2000-01-02', datetime.date(2000, 1, 2)),
                          ('OPEN', 4, b'open', None),
                          ('ACCOUNT', 4, b'Assets:Working', 'Assets:Working'),
                          ('EOL', 5, b'\n', None)], tokens)
        self.assertEqual(1, len(errors))

    @lex_tokens
    def test_lexer_exception_DATE(self, tokens, errors):
        """
          2000-13-32 open Assets:Something
        """
        self.assertEqual([('EOL', 2, b'\n', None),
                          ('error', 2, b'2000-13-32', None),
                          ('OPEN', 2, b'open', None),
                          ('ACCOUNT', 2, b'Assets:Something', 'Assets:Something'),
                          ('EOL', 3, b'\n', None)], tokens)
        self.assertEqual(1, len(errors))

    def test_lexer_exception_substring_with_quotes(self):
        test_input = """
          2016-07-15 query "hotels" "SELECT * WHERE account ~ 'Expenses:Accommodation'"
        """
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(textwrap.dedent(test_input), builder))
        self.assertEqual([
            ('EOL', 2, b'\n', None),
            ('DATE', 2, b'2016-07-15', datetime.date(2016, 7, 15)),
            ('QUERY', 2, b'query', None),
            ('STRING', 2, b'"hotels"', 'hotels'),
            ('STRING', 2, b'"SELECT * WHERE account ~ \'Expenses:Accommodation\'"',
             'SELECT * WHERE account ~ \'Expenses:Accommodation\''),
            ('EOL', 3, b'\n', None)], tokens)
        self.assertEqual(0, len(builder.errors))


class TestLexerUnicode(unittest.TestCase):

    test_utf8_string = textwrap.dedent("""
      2015-05-23 note Assets:Something "a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"
    """)
    expected_utf8_string = "a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"

    test_latin1_string = textwrap.dedent("""
      2015-05-23 note Assets:Something "école Floß søllerød"
    """)
    expected_latin1_string = "école Floß søllerød"

    # Test providing utf8 bytes to the lexer.
    def test_bytes_encoded_utf8(self):
        utf8_bytes = self.test_utf8_string.encode('utf8')
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(utf8_bytes, builder))

        # The lexer outputs no errors.
        self.assertFalse(builder.errors)

        # Check that the lexer correctly parsed the UTF8 string.
        str_tokens = [token for token in tokens if token[0] == 'STRING']
        self.assertEqual(self.expected_utf8_string, str_tokens[0][3])

    # Test providing latin1 bytes to the lexer when it is expecting utf8.
    def test_bytes_encoded_latin1_invalid(self):
        latin1_bytes = self.test_utf8_string.encode('latin1')
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(latin1_bytes, builder))

        # The lexer outputs no errors.
        self.assertFalse(builder.errors)

        # Check that the lexer failed to convert the string but did not cause
        # other errors.
        str_tokens = [token for token in tokens if token[0] == 'STRING']
        self.assertNotEqual(self.expected_utf8_string, str_tokens[0][3])

    # Test providing latin1 bytes to the lexer with an encoding.
    def test_bytes_encoded_latin1(self):
        latin1_bytes = self.test_latin1_string.encode('latin1')
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(latin1_bytes, builder, encoding='latin1'))

        # The lexer outputs no errors.
        self.assertFalse(builder.errors)

        # Check that the lexer correctly parsed the latin1 string.
        str_tokens = [token for token in tokens if token[0] == 'STRING']
        self.assertEqual(self.expected_latin1_string, str_tokens[0][3])

    # Test providing utf16 bytes to the lexer when it is expecting utf8.
    def test_bytes_encoded_utf16_invalid(self):
        utf16_bytes = self.test_utf8_string.encode('utf16')
        builder = lexer.LexBuilder()
        tokens = list(lexer.lex_iter_string(utf16_bytes, builder))
        self.assertTrue(builder.errors)


class TestLexerMisc(unittest.TestCase):

    @lex_tokens
    def test_valid_commas_in_number(self, tokens, errors):
        """\
          45,234.00
        """
        self.assertEqual([
            ('NUMBER', 1, b'45,234.00', D('45234.00')),
            ('EOL', 2, b'\n', None),
        ], tokens)
        self.assertEqual(0, len(errors))

    @lex_tokens
    def test_invalid_commas_in_integral(self, tokens, errors):
        """\
          452,34.00
        """
        self.assertEqual(1, len(errors))
        self.assertEqual([
            ('error', 1, b'452,34.00', None),
            ('EOL', 2, b'\n', None),
        ], tokens)

    @lex_tokens
    def test_invalid_commas_in_fractional(self, tokens, errors):
        """\
          45234.000,000
        """
        # Unfortunately this is going to get parsed as two numbers but that will
        # cause an error downstream in the parser. Nevertheless, keep this test
        # case here in case eventually we improve the lexer.
        self.assertEqual(0, len(errors))
        self.assertEqual([
            ('NUMBER', 1, b'45234.000', D('45234.000')),
            ('COMMA', 1, b',', None),
            ('NUMBER', 1, b'000', D('0')),
            ('EOL', 2, b'\n', None),
        ], tokens)


if __name__ == '__main__':
    unittest.main()
