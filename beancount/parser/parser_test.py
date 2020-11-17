"""
Tests for parser.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import io
import unittest
import tempfile
import textwrap
import sys
import subprocess

from pytest import mark

from beancount.core.number import D
from beancount.core import data
from beancount.parser import parser, _parser, lexer, grammar
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

    @unittest.skipIf('/bazel/' in __file__, "Skipping test in Bazel")
    @mark.xfail
    @parser.parse_doc(expect_errors=False)
    def test_parse_doc__errors(self, _, __, ___):
        """
        2013-05-40 * "Invalid date for parser"
          Expenses:Restaurant         100 USD
          Assets:US:Cash             -100 USD
        """

    @unittest.skipIf('/bazel/' in __file__, "Skipping test in Bazel")
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

    INPUT = textwrap.dedent("""
      2013-05-18 * "Nice dinner at Mermaid Inn"
        Expenses:Restaurant         100 USD
        Assets:US:Cash
    """)

    def test_parse_string(self):
        entries, errors, _ = parser.parse_string(self.INPUT)
        self.assertEqual(1, len(entries))
        self.assertEqual(0, len(errors))

    def test_parse_filename(self):
        with tempfile.NamedTemporaryFile('w', suffix='.beancount') as file:
            file.write(self.INPUT)
            file.flush()
            entries, errors, _ = parser.parse_file(file.name)
            self.assertEqual(1, len(entries))
            self.assertEqual(0, len(errors))

    def test_parse_file(self):
        with tempfile.TemporaryFile('w+b', suffix='.beancount') as file:
            file.write(self.INPUT.encode('utf-8'))
            file.seek(0)
            entries, errors, _ = parser.parse_file(file)
            self.assertEqual(1, len(entries))
            self.assertEqual(0, len(errors))

    @classmethod
    def parse_stdin(cls):
        entries, errors, _ = parser.parse_file("-")
        assert entries, "Empty entries: {}".format(entries)
        assert not errors, "Errors: {}".format(errors)

    def test_parse_stdin(self):
        env = test_utils.subprocess_env() if 'bazel' not in __file__ else None
        code = ('import beancount.parser.parser_test as p; '
                'p.TestParserInputs.parse_stdin()')
        pipe = subprocess.Popen([sys.executable, '-c', code, __file__],
                                env=env,
                                stdin=subprocess.PIPE)
        output, errors = pipe.communicate(self.INPUT.encode('utf-8'))
        self.assertEqual(0, pipe.returncode)

    def test_parse_None(self):
        # None is treated as the empty string...
        entries, errors, _ = parser.parse_string(None)
        self.assertEqual(0, len(entries))
        self.assertEqual(0, len(errors))
        # ...however None in not a valid file like object
        with self.assertRaises(TypeError):
            entries, errors, _ = parser.parse_file(None)


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


class TestReferenceCounting(unittest.TestCase):

    def test_parser_lex(self):
        # Do not use a string to avoid issues due to string interning.
        name = object()
        # Note that passing name as an argument to sys.getrefcount()
        # counts as one reference, thus the minimum reference count
        # returned for any object is 2.
        self.assertEqual(sys.getrefcount(name), 2)

        f = io.BytesIO(b"")
        f.name = name
        # One more refernece from the 'name' attriute.
        self.assertEqual(sys.getrefcount(name), 3)
        # Just one reference to the BytesIO object.
        self.assertEqual(sys.getrefcount(f), 2)

        builder = lexer.LexBuilder()
        parser = _parser.Parser(builder)
        iterator = parser.lex(f)
        # The Parser object keeps references to the input file and to
        # the name while iterating over the tokens in the input file.
        self.assertEqual(sys.getrefcount(name), 4)
        self.assertEqual(sys.getrefcount(f), 3)
        # The iterator holds one reference to the parser.
        self.assertEqual(sys.getrefcount(parser), 3)

        tokens = list(iterator)
        # No tokens returned for an empty input.
        self.assertEqual(len(tokens), 0)
        # Once done scanning is completed the Parser object still has
        # references to the input file and to the name.
        self.assertEqual(sys.getrefcount(name), 4)
        self.assertEqual(sys.getrefcount(f), 3)

        del parser
        del iterator
        # Once the Parser object is gone we should have just the local
        # reference to the file object and two references to name.
        self.assertEqual(sys.getrefcount(name), 3)
        self.assertEqual(sys.getrefcount(f), 2)

        del f
        # With the file object gone there is one reference to name.
        self.assertEqual(sys.getrefcount(name), 2)

    def test_parser_lex_filename(self):
        # Do not use a string to avoid issues due to string interning.
        name = object()
        self.assertEqual(sys.getrefcount(name), 2)

        f = io.BytesIO(b"")
        f.name = object()
        self.assertEqual(sys.getrefcount(f.name), 2)

        builder = lexer.LexBuilder()
        parser = _parser.Parser(builder)
        iterator = parser.lex(f, filename=name)
        tokens = list(iterator)
        # The Parser object keeps references to the input file and to
        # the name while iterating over the tokens in the input file.
        self.assertEqual(sys.getrefcount(name), 3)
        self.assertEqual(sys.getrefcount(f), 3)
        # The name attribute of the file object is not referenced.
        self.assertEqual(sys.getrefcount(f.name), 2)

        del parser
        del iterator
        # Once the Parser object is gone we should have just the local
        # reference to the file object and two references to name.
        self.assertEqual(sys.getrefcount(name), 2)
        self.assertEqual(sys.getrefcount(f), 2)

    def test_parser_lex_multi(self):
        file1 = io.BytesIO(b"")
        file1.name = object()
        self.assertEqual(sys.getrefcount(file1.name), 2)

        file2 = io.BytesIO(b"")
        file2.name = object()
        self.assertEqual(sys.getrefcount(file2.name), 2)

        builder = lexer.LexBuilder()
        parser = _parser.Parser(builder)
        tokens = list(parser.lex(file1))
        tokens = list(parser.lex(file2))

        del parser
        # Once the Parser object is gone we should have just the local
        # references to the file objects and one references to the names.
        self.assertEqual(sys.getrefcount(file1), 2)
        self.assertEqual(sys.getrefcount(file1.name), 2)
        self.assertEqual(sys.getrefcount(file2), 2)
        self.assertEqual(sys.getrefcount(file2.name), 2)

    def test_parser_parse(self):
        # Do not use a string to avoid issues due to string interning.
        name = object()
        self.assertEqual(sys.getrefcount(name), 2)

        f = io.BytesIO(b"")
        f.name = name
        self.assertEqual(sys.getrefcount(f.name), 3)

        builder = grammar.Builder()
        parser = _parser.Parser(builder)
        parser.parse(f)
        # The Parser object keeps a reference to the input file.
        self.assertEqual(sys.getrefcount(f), 3)
        # There are references to the file name from the Parser object
        # and from the the parsing results. In the case of an empty
        # input file from the options dictionary stored in the builder.
        self.assertEqual(sys.getrefcount(name), 5)
        builder.options = {}
        self.assertEqual(sys.getrefcount(name), 4)

        del parser
        # Once the Parser object is gone we should have just the local
        # reference to the file object and two references to name.
        self.assertEqual(sys.getrefcount(name), 3)
        self.assertEqual(sys.getrefcount(f), 2)


class TestLineno(unittest.TestCase):

    def test_lex(self):
        f = io.BytesIO(b"1.0")
        builder = lexer.LexBuilder()
        parser = _parser.Parser(builder)
        tokens = list(parser.lex(f))
        token, lineno, matched, value = tokens[0]
        self.assertEqual(lineno, 1)

    def test_lex_lineno(self):
        f = io.BytesIO(b"1.0")
        builder = lexer.LexBuilder()
        parser = _parser.Parser(builder)
        tokens = list(parser.lex(f, lineno=42))
        token, lineno, matched, value = tokens[0]
        self.assertEqual(lineno, 42)

    def test_parse(self):
        f = io.BytesIO(b"2020-07-30 open Assets:Test")
        builder = grammar.Builder()
        parser = _parser.Parser(builder)
        parser.parse(f)
        self.assertEqual(builder.entries[0].meta['lineno'], 1)

    def test_parse_lineno(self):
        f = io.BytesIO(b"2020-07-30 open Assets:Test")
        builder = grammar.Builder()
        parser = _parser.Parser(builder)
        parser.parse(f, lineno=42)
        self.assertEqual(builder.entries[0].meta['lineno'], 42)

    def test_parse_string(self):
        entries, errors, options = parser.parse_string(b"2020-07-30 open Assets:Test")
        self.assertEqual(entries[0].meta['lineno'], 1)

    def test_parse_string_lineno(self):
        entries, errors, options = parser.parse_string(b"2020-07-30 open Assets:Test",
                                                       report_firstline=42)
        self.assertEqual(entries[0].meta['lineno'], 42)

    @parser.parse_doc()
    def test_parse_doc(self, entries, errors, _):
        """
          2013-01-01 open Assets:US:Cash

          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash             -100 USD

          2013-05-19 balance  Assets:US:Cash   -100 USD

          2013-05-20 note  Assets:US:Cash   "Something"
        """

        with open(__file__) as f:
            for lineno, line in enumerate(f, 1):
                if line.strip() == "2013-01-01 open Assets:US:Cash":
                    break

        self.assertEqual(len(entries), 4)
        self.assertEqual(entries[0].meta['lineno'], lineno)
        self.assertEqual(entries[1].meta['lineno'], lineno + 2)
        self.assertEqual(entries[2].meta['lineno'], lineno + 6)
        self.assertEqual(entries[3].meta['lineno'], lineno + 8)


if __name__ == '__main__':
    unittest.main()
