"""
Tests for parser.
"""
import unittest
import textwrap
import inspect
import sys

from beancount.parser import parsedoc
from beancount.parser import parser
from beancount.core.data import Transaction, Check, Open, Close, Pad, Event, Price, Note
from beancount.core.data import format_entry


class TestParserEntries(unittest.TestCase):
    """Basic smoke test one entry of each kind."""

    @parsedoc
    def test_entry_transaction_1(self, entries, errors, options):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        self.assertTrue(isinstance(entries[0], Transaction))

    @parsedoc
    def __test_entry_transaction_2(self, entries, errors, options):
        """
          2013-05-18 txn "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        self.assertTrue(isinstance(entries[0], Transaction))

    @parsedoc
    def test_entry_transaction_invalid_npostings(self, entries, errors, options):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD

        """
        self.assertEqual(0, len(entries))
        self.assertEqual(1, len(errors))

    @parsedoc
    def test_entry_check(self, entries, errors, options):
        """
          2013-05-18 check Assets:US:BestBank:Checking  200 USD
        """
        self.assertTrue(isinstance(entries[0], Check))

    @parsedoc
    def test_entry_open_1(self, entries, errors, options):
        """
          2013-05-18 open Assets:US:BestBank:Checking
        """
        self.assertTrue(isinstance(entries[0], Open))

    @parsedoc
    def test_entry_open_2(self, entries, errors, options):
        """
          2013-05-18 open Assets:US:BestBank:Checking   USD
        """
        self.assertTrue(isinstance(entries[0], Open))

    @parsedoc
    def test_entry_open_3(self, entries, errors, options):
        """
          2013-05-18 open Assets:Cash   USD,CAD,EUR
        """
        self.assertTrue(isinstance(entries[0], Open))

    @parsedoc
    def test_entry_close(self, entries, errors, options):
        """
          2013-05-18 close Assets:US:BestBank:Checking
        """
        self.assertTrue(isinstance(entries[0], Close))

    @parsedoc
    def test_entry_pad(self, entries, errors, options):
        """
          2013-05-18 pad Assets:US:BestBank:Checking  Equity:Opening-Balancess
        """
        self.assertTrue(isinstance(entries[0], Pad))

    @parsedoc
    def test_entry_event(self, entries, errors, options):
        """
          2013-05-18 event "location" "New York, USA"
        """
        self.assertTrue(isinstance(entries[0], Event))

    @parsedoc
    def test_entry_note(self, entries, errors, options):
        """
          2013-05-18 note Assets:US:BestBank:Checking  "Blah, di blah."
        """
        self.assertTrue(isinstance(entries[0], Note))

    @parsedoc
    def test_entry_price(self, entries, errors, options):
        """
          2013-05-18 price USD   1.0290 CAD
        """
        self.assertTrue(isinstance(entries[0], Price))


class TestUglyBugs(unittest.TestCase):
    """Test all kinds of stupid sh*t that will inevitably occur in practice."""

    @parsedoc
    def test_empty_1(self, entries, errors, options):
        ""
        self.assertEqual(entries, [])
        self.assertEqual(errors, [])

    @parsedoc
    def test_empty_2(self, entries, errors, options):
        """

        """
        self.assertEqual(entries, [])
        self.assertEqual(errors, [])

    @parsedoc
    def test_comment(self, entries, errors, options):
        """
        ;; This is some comment.
        """
        self.assertEqual(entries, [])
        self.assertEqual(errors, [])

    def test_extra_whitespace_note(self):
        input_ = '\n2013-07-11 note Assets:Cash "test"\n\n  ;;\n'
        entries, errors, options = parser.parse_string(input_)
        self.assertEqual(1, len(entries))
        self.assertEqual(errors, [])

    def test_extra_whitespace_transaction(self):
        input_ = '\n'.join([
          '2013-05-18 * "Nice dinner at Mermaid Inn"',
          '  Expenses:Restaurant         100 USD',
          '  Assets:US:Cash',
          '  ',
          ';; End of file',
          ])

        entries, errors, options = parser.parse_string(input_, yydebug=0)
        self.assertEqual(1, len(entries))
        self.assertEqual(errors, [])

    def test_extra_whitespace_comment(self):
        input_ = '\n'.join([
          '2013-05-18 * "Nice dinner at Mermaid Inn"',
          '  Expenses:Restaurant         100 USD',
          '  Assets:US:Cash',
          '  ;;',
          ])
        entries, errors, options = parser.parse_string(input_)
        self.assertEqual(1, len(entries))
        self.assertEqual(errors, [])


class TestSyntaxErrors(unittest.TestCase):
    """Test syntax errors that occur within the parser.
    One of our goals is to recover and report without ever
    bailing out with an exception."""

    @parsedoc
    def test_lexer_default_rule_1(self, entries, errors, options):
        """
          Account:*:Bla
        """
        self.assertEqual(entries, [])
        self.assertEqual(1, len(errors))

    @parsedoc
    def test_lexer_default_rule_2(self, entries, errors, options):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Resta(urant        100 USD
            Expenses:Tips                10 USD
            Assets:US:Cash             -110 USD

          2013-05-20 check Assets:US:Cash  -110 USD
        """
        # This should fail to parse the "Expenses:Resta(urant" account name.

        # Check that we indeed read the 'check' entry that comes after the one
        # with the error.
        self.assertEqual(1, len(entries))
        self.assertTrue(isinstance(entries[0], Check))

        # Make sure at least one error is reported.
        self.assertEqual(1, len(errors))


class TestLineNumbers(unittest.TestCase):
    """Check that the line numbers line up correctly."""

    @parsedoc
    def test_line_numbers(self, entries, errors, options):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash

          2013-05-19 check  Assets:US:Cash   -100 USD

          2013-05-20 note  Assets:US:Cash   "Something"

        """
        _, first_line = inspect.getsourcelines(
            TestLineNumbers.test_line_numbers.__wrapped__)
        first_line += 1

        self.assertEqual(2, entries[0].fileloc.lineno - first_line)
        self.assertEqual(6, entries[1].fileloc.lineno - first_line)
        self.assertEqual(8, entries[2].fileloc.lineno - first_line)


class TestParserOptions(unittest.TestCase):

    @parsedoc
    def test_empty_1(self, entries, errors, options):
        """
          option "title" "Super Rich"

        """
        option = options['title']
        self.assertEqual(option, 'Super Rich')

    @parsedoc
    def test_invalid_option(self, entries, errors, options):
        """
          option "bladibla_invalid" "Some value"

        """
        self.assertEqual(len(errors), 1)
        self.assertTrue(isinstance(errors[0], parser.ParserError))


class TestParserLinks(unittest.TestCase):

    @parsedoc
    def test_links(self, entries, errors, options):
        """
          2013-05-18 * "Something something" ^38784734873
            Expenses:Restaurant         100 USD
            Assets:US:Cash

        """
        # FIXME: check it here.
        ##print(entries[0])


class TestSimple(unittest.TestCase):

    @parsedoc
    def test_simple_1(self, entries, errors, options):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        self.assertEqual(len(entries), 1)
        self.assertEqual(errors, [])

    @parsedoc
    def test_simple_2(self, entries, errors, options):
        """

          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash

          2013-05-20 * "Duane Reade" | "Toothbrush"
            Expenses:BathroomSupplies         4 USD
            Assets:US:BestBank:Checking

        """
        self.assertEqual(len(entries), 2)
        self.assertEqual(errors, [])
