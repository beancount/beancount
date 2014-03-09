"""
Tests for parser.
"""
import unittest
import inspect

from beancount.parser import parsedoc
from beancount.parser import parser, options
from beancount.core.data import Transaction, Balance, Open, Close, Pad, Event, Price, Note
from beancount.core.amount import Amount


def check_list(test, objlist, explist):
    """Assert the list of objects against the expected specification.

    Args:
      test: the instance of the test object, used for generating assertions.
      objlist: the list of objects returned.

      explist: the list of objects expected. 'explist' can be an integer, to
               check the length of the list; if it is a list of types, the types
               are checked against the types of the objects in the list. This is
               meant to be a convenient method.
    """
    if isinstance(explist, int):
        test.assertEqual(explist, len(objlist))
    elif isinstance(explist, (tuple, list)):
        test.assertEqual(len(explist), len(objlist))
        for obj, exp in zip(objlist, explist):
            test.assertTrue(isinstance(type(obj), type(exp)))


class TestParserMisc(unittest.TestCase):
    """Test various functions."""

    def test_get_previous_accounts(self):
        options_ = options.DEFAULT_OPTIONS.copy()
        result = parser.get_previous_accounts(options_)
        self.assertEquals(3, len(result))
        self.assertTrue(all(isinstance(x, str) for x in result))

    def test_get_current_accounts(self):
        options_ = options.DEFAULT_OPTIONS.copy()
        result = parser.get_current_accounts(options_)
        self.assertEquals(2, len(result))
        self.assertTrue(all(isinstance(x, str) for x in result))


class TestParserEntries(unittest.TestCase):
    """Basic smoke test one entry of each kind."""

    @parsedoc
    def test_entry_transaction_1(self, entries, errors, options):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [Transaction])

    @parsedoc
    def test_entry_transaction_2(self, entries, errors, options):
        """
          2013-05-18 txn "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [Transaction])

    @parsedoc
    def test_entry_transaction_invalid_npostings(self, entries, errors, options):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD

        """
        check_list(self, entries, [])
        check_list(self, errors, 1)

    @parsedoc
    def test_entry_check(self, entries, errors, options):
        """
          2013-05-18 check Assets:US:BestBank:Checking  200 USD
        """
        check_list(self, entries, [Balance])

    @parsedoc
    def test_entry_open_1(self, entries, errors, options):
        """
          2013-05-18 open Assets:US:BestBank:Checking
        """
        check_list(self, entries, [Open])

    @parsedoc
    def test_entry_open_2(self, entries, errors, options):
        """
          2013-05-18 open Assets:US:BestBank:Checking   USD
        """
        check_list(self, entries, [Open])

    @parsedoc
    def test_entry_open_3(self, entries, errors, options):
        """
          2013-05-18 open Assets:Cash   USD,CAD,EUR
        """
        check_list(self, entries, [Open])

    @parsedoc
    def test_entry_close(self, entries, errors, options):
        """
          2013-05-18 close Assets:US:BestBank:Checking
        """
        check_list(self, entries, [Close])

    @parsedoc
    def test_entry_pad(self, entries, errors, options):
        """
          2013-05-18 pad Assets:US:BestBank:Checking  Equity:Opening-Balancess
        """
        check_list(self, entries, [Pad])

    @parsedoc
    def test_entry_event(self, entries, errors, options):
        """
          2013-05-18 event "location" "New York, USA"
        """
        check_list(self, entries, [Event])

    @parsedoc
    def test_entry_note(self, entries, errors, options):
        """
          2013-05-18 note Assets:US:BestBank:Checking  "Blah, di blah."
        """
        check_list(self, entries, [Note])

    @parsedoc
    def test_entry_price(self, entries, errors, options):
        """
          2013-05-18 price USD   1.0290 CAD
        """
        check_list(self, entries, [Price])


class TestUglyBugs(unittest.TestCase):
    """Test all kinds of stupid sh*t that will inevitably occur in practice."""

    @parsedoc
    def test_empty_1(self, entries, errors, options):
        ""
        check_list(self, entries, [])
        check_list(self, errors, [])

    @parsedoc
    def test_empty_2(self, entries, errors, options):
        """

        """
        check_list(self, entries, [])
        check_list(self, errors, [])

    @parsedoc
    def test_comment(self, entries, errors, options):
        """
        ;; This is some comment.
        """
        check_list(self, entries, [])
        check_list(self, errors, [])

    def test_extra_whitespace_note(self):
        input_ = '\n2013-07-11 note Assets:Cash "test"\n\n  ;;\n'
        entries, errors, options = parser.parse_string(input_)
        check_list(self, entries, [Note])
        check_list(self, errors, [])

    def test_extra_whitespace_transaction(self):
        input_ = '\n'.join([
          '2013-05-18 * "Nice dinner at Mermaid Inn"',
          '  Expenses:Restaurant         100 USD',
          '  Assets:US:Cash',
          '  ',
          ';; End of file',
          ])

        entries, errors, options = parser.parse_string(input_, yydebug=0)
        check_list(self, entries, [Transaction])
        check_list(self, errors, [])

    def test_extra_whitespace_comment(self):
        input_ = '\n'.join([
          '2013-05-18 * "Nice dinner at Mermaid Inn"',
          '  Expenses:Restaurant         100 USD',
          '  Assets:US:Cash',
          '  ;;',
          ])
        entries, errors, options = parser.parse_string(input_)
        check_list(self, entries, [Transaction])
        check_list(self, errors, [])


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
        check_list(self, entries, [Balance])

        # Make sure at least one error is reported.
        check_list(self, errors, [parser.ParserSyntaxError])


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
        check_list(self, errors, [parser.ParserError])


class TestParserLinks(unittest.TestCase):

    @parsedoc
    def test_links(self, entries, errors, options):
        """
          2013-05-18 * "Something something" ^38784734873
            Expenses:Restaurant         100 USD
            Assets:US:Cash

        """
        check_list(self, entries, [Transaction])
        self.assertEqual(entries[0].links, ['38784734873'])


class TestSimple(unittest.TestCase):

    @parsedoc
    def test_simple_1(self, entries, errors, options):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [Transaction])
        check_list(self, errors, [])

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
        check_list(self, entries, [Transaction, Transaction])
        check_list(self, errors, [])

    @parsedoc
    def test_parse_currencies(self, entries, errors, options):
        """
          2014-01-19 open Assets:Underscore    DJ_EURO
          2014-01-19 open Assets:Period        DJ.EURO
          2014-01-19 open Assets:Apostrophe    DJ'EURO
          2014-01-19 open Assets:Numbers       EURO123
        """
        self.assertFalse(errors)


class TestTotals(unittest.TestCase):

    @parsedoc
    def test_total_price(self, entries, errors, options):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      10 MSFT @@ 2000 USD
            Assets:Investments:Cash
        """
        posting = entries[0].postings[0]
        self.assertEqual(Amount('200', 'USD'), posting.price)
        self.assertEqual(None, posting.position.lot.cost)

    @parsedoc
    def test_total_cost(self, entries, errors, options):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      10 MSFT {{2000 USD}}
            Assets:Investments:Cash

          2013-05-18 * ""
            Assets:Investments:MSFT      10 MSFT {{2000 USD / 2014-02-25}}
            Assets:Investments:Cash
        """
        for entry in entries:
            posting = entry.postings[0]
            self.assertEqual(Amount('200', 'USD'), posting.position.lot.cost)
            self.assertEqual(None, posting.price)
