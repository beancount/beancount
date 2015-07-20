"""
Tests for parser.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import tempfile
import textwrap
import sys
import subprocess

from beancount.parser import parser
from beancount.utils import test_utils


class TestCompareTestFunctions(unittest.TestCase):

    def test_has_auto_postings(self):
        entries, _, __ = parser.parse_string("""

          2014-01-27 * "UNION MARKET"
            Liabilities:US:Amex:BlueCash    -22.02 USD
            Expenses:Food:Grocery            22.02 USD

        """, dedent=True)
        self.assertFalse(parser.has_auto_postings(entries))

        entries, _, __ = parser.parse_string("""

          2014-01-27 * "UNION MARKET"
            Liabilities:US:Amex:BlueCash    -22.02 USD
            Expenses:Food:Grocery

        """, dedent=True)
        self.assertTrue(parser.has_auto_postings(entries))


class TestParserDoc(unittest.TestCase):

    @parser.parsedoc()
    def test_parsedoc(self, entries, errors, options_map):
        """
        2013-05-40 * "Nice dinner at Mermaid Inn"
          Expenses:Restaurant         100 USD
          Assets:US:Cash
        """
        self.assertTrue(errors)

    # Note: nose does not honor expectedFailure as of 1.3.4. We would use it
    # here instead of doing this manually.
    def test_parsedoc__errors(self):
        @parser.parsedoc(errors=True)
        def test_function(self, entries, errors, options_map):
            """
            2013-05-40 * "Nice dinner at Mermaid Inn"
              Expenses:Restaurant         100 USD
              Assets:US:Cash
            """
        try:
            test_function(unittest.TestCase())
            self.fail("Test should have failed.")
        except AssertionError:
            pass

    # Note: nose does not honor expectedFailure as of 1.3.4. We would use it
    # here instead of doing this manually.
    def test_parsedoc__noerrors(self):
        @parser.parsedoc(errors=False)
        def test_function(self, entries, errors, options_map):
            """
            2013-05-40 * "Nice dinner at Mermaid Inn"
              Expenses:Restaurant         100 USD
              Assets:US:Cash
            """
        try:
            test_function(unittest.TestCase())
            self.fail("Test should have failed.")
        except AssertionError:
            pass


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
        assert entries
        assert not errors

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
        self.assertRegexpMatches(errors[0].message, "unknown encoding")
        self.assertFalse(entries)
