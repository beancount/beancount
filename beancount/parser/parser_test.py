"""
Tests for parser.
"""

__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import io
import unittest
import textwrap
import sys

from beancount.core.number import D
from beancount.core import data
from beancount.parser import parser
from beancount.utils import test_utils


class TestCompareTestFunctions(unittest.TestCase):
    def test_is_entry_incomplete(self):
        entries, _, __ = parser.parse_string(
            """

          2014-01-27 * "UNION MARKET"
            Liabilities:US:Amex:BlueCash    -22.02 USD
            Expenses:Food:Grocery            22.02 USD

          2014-01-27 * "UNION MARKET"
            Liabilities:US:Amex:BlueCash    -22.02 USD
            Expenses:Food:Grocery

        """,
            dedent=True,
        )
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

    @unittest.skipIf("/bazel/" in __file__, "Skipping test in Bazel")
    @unittest.expectedFailure
    @parser.parse_doc(expect_errors=False)
    def test_parse_doc__errors(self, _, __, ___):
        """
        2013-05-40 * "Invalid date for parser"
          Expenses:Restaurant         100 USD
          Assets:US:Cash             -100 USD
        """

    @unittest.skipIf("/bazel/" in __file__, "Skipping test in Bazel")
    @unittest.expectedFailure
    @parser.parse_doc(expect_errors=True)
    def test_parse_doc__noerrors(self, _, __, ___):
        """
        2013-05-01 * "Valid date for parser"
          Expenses:Restaurant         100 USD
          Assets:US:Cash             -100 USD
        """


class TestParserInputs(unittest.TestCase):
    """Try difference sources for the parser's input."""

    INPUT = textwrap.dedent(
        """
      2013-05-18 * "Nice dinner at Mermaid Inn"
        Expenses:Restaurant         100 USD
        Assets:US:Cash
    """
    )

    def test_parse_string(self):
        entries, errors, _ = parser.parse_string(self.INPUT)
        self.assertEqual(1, len(entries))
        self.assertEqual(0, len(errors))

    def test_parse_filename(self):
        with test_utils.temp_file(suffix=".beancount") as file:
            file.write_text(self.INPUT, encoding="utf8")
            entries, errors, _ = parser.parse_file(str(file))
            self.assertEqual(1, len(entries))
            self.assertEqual(0, len(errors))

    def test_parse_file(self):
        with test_utils.temp_file(suffix=".beancount") as file:
            file.write_text(self.INPUT, encoding="utf8")
            with open(file, "rb") as obj:
                entries, errors, _ = parser.parse_file(obj)
            self.assertEqual(1, len(entries))
            self.assertEqual(0, len(errors))

    def test_parse_stdin(self):
        stdin = sys.stdin
        try:
            sys.stdin = io.TextIOWrapper(io.BytesIO(self.INPUT.encode("utf-8")))
            entries, errors, _ = parser.parse_file("-")
            self.assertEqual(1, len(entries))
            self.assertEqual(0, len(errors))
        finally:
            sys.stdin = stdin

    def test_parse_None(self):
        # None is treated as the empty string...
        entries, errors, _ = parser.parse_string(None)
        self.assertEqual(0, len(entries))
        self.assertEqual(0, len(errors))
        # ...however None in not a valid file like object
        with self.assertRaises(TypeError):
            entries, errors, _ = parser.parse_file(None)


class TestUnicodeErrors(unittest.TestCase):
    test_utf8_string = textwrap.dedent(
        """
      2015-05-23 note Assets:Something "a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"
    """
    )
    expected_utf8_string = "a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"

    test_latin1_string = textwrap.dedent(
        """
      2015-05-23 note Assets:Something "école Floß søllerød"
    """
    )
    expected_latin1_string = "école Floß søllerød"

    # Test providing utf8 bytes to the lexer.
    def test_bytes_encoded_utf8(self):
        utf8_bytes = self.test_utf8_string.encode("utf8")
        entries, errors, _ = parser.parse_string(utf8_bytes)
        self.assertEqual(1, len(entries))
        self.assertFalse(errors)
        # Check that the lexer correctly parsed the UTF8 string.
        self.assertEqual(self.expected_utf8_string, entries[0].comment)

    # Test providing latin1 bytes to the lexer when it is expecting utf8.
    def test_bytes_encoded_incorrect(self):
        latin1_bytes = self.test_utf8_string.encode("latin1")
        entries, errors, _ = parser.parse_string(latin1_bytes)
        # Check that the lexer failed to convert the string and reported an error.
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "^UnicodeDecodeError: 'utf-8' codec ")
        self.assertFalse(entries)


class TestTestUtils(unittest.TestCase):
    def test_parse_many(self):
        with self.assertRaises(AssertionError):
            entries = parser.parse_many(
                """
              2014-12-15 * 2014-12-15
            """
            )

        number = D("101.23")
        entries = parser.parse_many(
            """
          2014-12-15 * "Payee" "Narration"
            Assets:Checking   {number} USD
            Equity:Blah
        """
        )
        self.assertEqual(1, len(entries))
        self.assertEqual(number, entries[0].postings[0].units.number)

    def test_parse_one(self):
        with self.assertRaises(AssertionError):
            parser.parse_one(
                """
              2014-12-15 * 2014-12-15
            """
            )

        entry = parser.parse_one(
            """
          2014-12-15 * "Payee" "Narration"
            Assets:Checking   101.23 USD
            Equity:Blah
        """
        )
        self.assertTrue(isinstance(entry, data.Transaction))


if __name__ == "__main__":
    unittest.main()
