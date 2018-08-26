"""
Tests for parser.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import tempfile
import textwrap
import sys
import subprocess

from pytest import mark

from beancount.core.number import D
from beancount.core import data
from beancount.parser import parser
from beancount.utils import test_utils


class TestCompareTestFunctions(unittest.TestCase):

    def test_is_entry_incomplete(self):
        entries, _, __ = parser.parse_string("""

          2014-01-27 * "UNION MARKET"
            Liabilities:US:Amex:BlueCash    -22.02 USD
            Expenses:Food:Grocery            22.02 USD

          2014-01-27 * "UNION MARKET"
            Liabilities:US:Amex:BlueCash    -22.02 USD
            Expenses:Food:Grocery

        """, dedent=True)
        self.assertFalse(parser.is_entry_incomplete(entries[0]))
        self.assertTrue(parser.is_entry_incomplete(entries[1]))


class TestParserDoc(unittest.TestCase):

    @parser.parse_doc(expect_errors=None)
    def test_parse_doc__disabled(self, entries, errors, options_map):
        """
        2013-05-40 * "Nice dinner at Mermaid Inn"
          Expenses:Restaurant         100 USD
          Assets:US:Cash
        """
        self.assertTrue(errors)

    @mark.xfail
    @parser.parse_doc(expect_errors=False)
    def test_parse_doc__errors(self, _, __, ___):
        """
        2013-05-40 * "Invalid date for parser"
          Expenses:Restaurant         100 USD
          Assets:US:Cash             -100 USD
        """

    @mark.xfail
    @parser.parse_doc(expect_errors=True)
    def test_parse_doc__noerrors(self, _, __, ___):
        """
        2013-05-01 * "Valid date for parser"
          Expenses:Restaurant         100 USD
          Assets:US:Cash             -100 USD
        """


class TestParserInputs(unittest.TestCase):
    """Try difference sources for the parser's input."""

    INPUT = """
      2013-05-18 * "Nice dinner at Mermaid Inn"
        Expenses:Restaurant         100 USD
        Assets:US:Cash
    """

    def test_parse_string(self):
        entries, errors, _ = parser.parse_string(self.INPUT)
        self.assertEqual(1, len(entries))
        self.assertEqual(0, len(errors))

    def test_parse_file(self):
        with tempfile.NamedTemporaryFile('w', suffix='.beancount') as file:
            file.write(self.INPUT)
            file.flush()
            entries, errors, _ = parser.parse_file(file.name)
            self.assertEqual(1, len(entries))
            self.assertEqual(0, len(errors))

    @classmethod
    def parse_stdin(cls):
        entries, errors, _ = parser.parse_file("-")
        assert entries, "Empty entries: {}".format(entries)
        assert not errors, "Errors: {}".format(errors)

    def test_parse_stdin(self):
        code = ('import beancount.parser.parser_test as p; '
                'p.TestParserInputs.parse_stdin()')
        pipe = subprocess.Popen([sys.executable, '-c', code, __file__],
                                env=test_utils.subprocess_env(),
                                stdin=subprocess.PIPE)
        output, errors = pipe.communicate(self.INPUT.encode('utf-8'))
        self.assertEqual(0, pipe.returncode)

    def test_parse_string_None(self):
        input_string = report_filename = None
        with self.assertRaises(TypeError):
            entries, errors, _ = parser.parse_string(input_string)
        with self.assertRaises(TypeError):
            entries, errors, _ = parser.parse_string("something", None, report_filename)


class TestUnicodeErrors(unittest.TestCase):

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
        entries, errors, _ = parser.parse_string(utf8_bytes)
        self.assertEqual(1, len(entries))
        self.assertFalse(errors)
        # Check that the lexer correctly parsed the UTF8 string.
        self.assertEqual(self.expected_utf8_string, entries[0].comment)

    # Test providing latin1 bytes to the lexer when it is expecting utf8.
    def test_bytes_encoded_incorrect(self):
        latin1_bytes = self.test_utf8_string.encode('latin1')
        entries, errors, _ = parser.parse_string(latin1_bytes)
        self.assertEqual(1, len(entries))
        self.assertFalse(errors)
        # Check that the lexer failed to convert the string but did not cause
        # other errors.
        self.assertNotEqual(self.expected_utf8_string, entries[0].comment)

    # Test providing latin1 bytes to the lexer with an encoding.
    def test_bytes_encoded_latin1(self):
        latin1_bytes = self.test_latin1_string.encode('latin1')
        entries, errors, _ = parser.parse_string(latin1_bytes, encoding='latin1')
        self.assertEqual(1, len(entries))
        self.assertFalse(errors)
        # Check that the lexer correctly parsed the latin1 string.
        self.assertEqual(self.expected_latin1_string, entries[0].comment)

    # Test using a garbage invalid encoding.
    def test_bytes_encoded_invalid(self):
        latin1_bytes = self.test_latin1_string.encode('latin1')
        entries, errors, _ = parser.parse_string(latin1_bytes, encoding='garbage')
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "unknown encoding")
        self.assertFalse(entries)


class TestTestUtils(unittest.TestCase):

    def test_parse_many(self):
        with self.assertRaises(AssertionError):
            entries = parser.parse_many("""
              2014-12-15 * 2014-12-15
            """)

        number = D('101.23')
        entries = parser.parse_many("""
          2014-12-15 * "Payee" "Narration"
            Assets:Checking   {number} USD
            Equity:Blah
        """)
        self.assertEqual(1, len(entries))
        self.assertEqual(number, entries[0].postings[0].units.number)

    def test_parse_one(self):
        with self.assertRaises(AssertionError):
            parser.parse_one("""
              2014-12-15 * 2014-12-15
            """)

        entry = parser.parse_one("""
          2014-12-15 * "Payee" "Narration"
            Assets:Checking   101.23 USD
            Equity:Blah
        """)
        self.assertTrue(isinstance(entry, data.Transaction))
