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


class TestParserDoc(unittest.TestCase):

    @parser.parsedoc
    def test_parsedoc(self, entries, errors, options_map):
        """
        2013-05-40 * "Nice dinner at Mermaid Inn"
          Expenses:Restaurant         100 USD
          Assets:US:Cash
        """
        self.assertTrue(errors)

    # Note: nose does not honor expectedFailure as of 1.3.4. We would use it
    # here instead of doing this manually.
    def test_parsedoc_noerrors(self):
        @parser.parsedoc_noerrors
        def test_function(self, entries, options_map):
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

    utf8_test_string = textwrap.dedent("""
      2015-01-01 open Assets:Something
      2015-05-23 note Assets:Something "¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ "
    """)

    def test_bytes_encoded_utf8(self):
        parser.parse_string(self.utf8_test_string.encode('utf-8'))

    def test_bytes_encoded_latin1(self):
        # Note: This should fail without dumping core. This should fail
        # elegantly. This should be failing in the lexer.
        with self.assertRaises(UnicodeDecodeError):
            parser.parse_string(self.utf8_test_string.encode('latin1', 'ignore'))
