"""
Tests for parser.
"""
import unittest
import functools
import textwrap

from beancount2 import parser
from beancount2.core.data import *


def parsedoc(fun):
    """Decorator that parses the function's docstring as an argument."""
    @functools.wraps(fun)
    def newfun(self):
        entries, errors, options = parser.parse_string(textwrap.dedent(fun.__doc__))
        return fun(self, entries, errors, options)
    return newfun


class TestParserEntries(unittest.TestCase):

    @parsedoc
    def test_entry_transaction(self, entries, errors, options):
        """

          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash

        """
        self.assertTrue(isinstance(entries[0], Transaction))

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

          2013-05-18 open Assets:US:BestBank:Checking  "123456789"

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



class TestParserMisc(unittest.TestCase):

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


class TestParserOptions(unittest.TestCase):

    @parsedoc
    def test_empty_1(self, entries, errors, options):
        """
          option "title" "Super Rich"

        """
        option = options['title']
        self.assertEqual(option, 'Super Rich')


class TestParserLinks(unittest.TestCase):

    @parsedoc
    def test_links(self, entries, errors, options):
        """
          2013-05-18 * "Something something" ^38784734873
            Expenses:Restaurant         100 USD
            Assets:US:Cash

        """
        print(entries[0])
