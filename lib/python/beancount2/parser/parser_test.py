"""
Tests for parser.
"""
import unittest
from textwrap import dedent

from beancount2 import parser
from beancount2.data import *


class TestParserEntries(unittest.TestCase):

    def parsetest_entry_transaction(self, contents):
        """

          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash

        """
        self.assertTrue(isinstance(contents.entries[0], Transaction))

    def parsetest_entry_check(self, contents):
        """

          2013-05-18 check Assets:US:BestBank:Checking  200 USD

        """
        self.assertTrue(isinstance(contents.entries[0], Check))

    def parsetest_entry_open_1(self, contents):
        """

          2013-05-18 open Assets:US:BestBank:Checking

        """
        self.assertTrue(isinstance(contents.entries[0], Open))

    def parsetest_entry_open_2(self, contents):
        """

          2013-05-18 open Assets:US:BestBank:Checking  "123456789"

        """
        self.assertTrue(isinstance(contents.entries[0], Open))

    def parsetest_entry_close(self, contents):
        """

          2013-05-18 close Assets:US:BestBank:Checking

        """
        self.assertTrue(isinstance(contents.entries[0], Close))

    def parsetest_entry_pad(self, contents):
        """

          2013-05-18 pad Assets:US:BestBank:Checking  Equity:OpeningBalances

        """
        self.assertTrue(isinstance(contents.entries[0], Pad))

    def parsetest_entry_event(self, contents):
        """

          2013-05-18 event "location" "New York, USA"

        """
        self.assertTrue(isinstance(contents.entries[0], Event))

    def parsetest_entry_note(self, contents):
        """

          2013-05-18 note Assets:US:BestBank:Checking  "Blah, di blah."

        """
        self.assertTrue(isinstance(contents.entries[0], Note))

    def parsetest_entry_price(self, contents):
        """

          2013-05-18 price USD   1.0290 CAD

        """
        self.assertTrue(isinstance(contents.entries[0], Price))



class TestParserMisc(unittest.TestCase):

    def parsetest_empty_1(self, contents):
        ""
        self.assertEqual(contents.entries, [])
        self.assertEqual(contents.accounts, [])
        self.assertEqual(contents.parse_errors, [])

    def parsetest_empty_2(self, contents):
        """

        """
        self.assertEqual(contents.entries, [])
        self.assertEqual(contents.accounts, [])
        self.assertEqual(contents.parse_errors, [])

    def parsetest_comment(self, contents):
        """
        ;; This is some comment.
        """
        self.assertEqual(contents.entries, [])
        self.assertEqual(contents.accounts, [])
        self.assertEqual(contents.parse_errors, [])

    def parsetest_simple_1(self, contents):
        """

          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash

        """
        self.assertEqual(len(contents.entries), 1)
        self.assertEqual(len(contents.accounts), 2)
        self.assertEqual(contents.parse_errors, [])

    def parsetest_simple_2(self, contents):
        """

          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash

          2013-05-20 * "Duane Reade" | "Toothbrush"
            Expenses:BathroomSupplies         4 USD
            Assets:US:BestBank:Checking

        """
        self.assertEqual(len(contents.entries), 2)
        self.assertEqual(len(contents.accounts), 4)
        self.assertEqual(contents.parse_errors, [])


def create_parsetest_method(method):
    """Create a test method that will automatically parse the
    docstring as beancount syntax and provide the results to
    the wrapped test function."""
    input_string = dedent(method.__doc__)
    def new_method(self):
        contents = parser.parse_string(input_string)
        return method(self, contents)
    new_method.__name__ = method.__name__[5:]
    return new_method


def create_parsetest_methods(klass):
    """Decorate the test class with convenient test methods that
    automatically parse the beancount syntax. This avoids a lot
    of boilerplate."""
    for attrname, attribute in list(klass.__dict__.items()):
        if attrname.startswith('parsetest_'):
            new_method = create_parsetest_method(attribute)
            setattr(klass, attrname[5:], new_method)


create_parsetest_methods(TestParserEntries)
create_parsetest_methods(TestParserMisc)
