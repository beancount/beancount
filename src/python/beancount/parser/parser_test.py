"""
Tests for parser.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import unittest
import inspect
import tempfile
import re
import sys
import subprocess
import tempfile

from beancount.core.amount import D
from beancount.parser.parser import parsedoc
from beancount.parser import parser
from beancount.parser import lexer
from beancount.core import data
from beancount.core import amount
from beancount.core import interpolate
from beancount.core import interpolate_test
from beancount.utils import test_utils


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


class TestParserEntryTypes(unittest.TestCase):
    """Basic smoke test one entry of each kind."""

    @parsedoc
    def test_entry_transaction_1(self, entries, _, __):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [data.Transaction])

    @parsedoc
    def test_entry_transaction_2(self, entries, _, __):
        """
          2013-05-18 txn "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [data.Transaction])

    @parsedoc
    def test_entry_balance(self, entries, _, __):
        """
          2013-05-18 balance Assets:US:BestBank:Checking  200 USD
        """
        check_list(self, entries, [data.Balance])

    @parsedoc
    def test_entry_balance_with_cost(self, entries, errors, __):
        """
          2013-05-18 balance Assets:Investments  10 MSFT {45.30 USD}
        """
        check_list(self, entries, [])
        check_list(self, errors, [parser.ParserSyntaxError])

    @parsedoc
    def test_entry_open_1(self, entries, _, __):
        """
          2013-05-18 open Assets:US:BestBank:Checking
        """
        check_list(self, entries, [data.Open])

    @parsedoc
    def test_entry_open_2(self, entries, _, __):
        """
          2013-05-18 open Assets:US:BestBank:Checking   USD
        """
        check_list(self, entries, [data.Open])

    @parsedoc
    def test_entry_open_3(self, entries, errors, __):
        """
          2013-05-18 open Assets:Cash   USD,CAD,EUR
        """
        check_list(self, entries, [data.Open])
        self.assertEqual(entries[0].booking, None)

    @parsedoc
    def test_entry_open_4(self, entries, errors, __):
        """
          2013-05-18 open Assets:US:Vanguard:VIIPX  VIIPX  "STRICT"
        """
        check_list(self, entries, [data.Open])
        self.assertEqual(entries[0].booking, 'STRICT')

    @parsedoc
    def test_entry_open_5(self, entries, errors, __):
        """
          2013-05-18 open Assets:US:Vanguard:VIIPX    "STRICT"
        """
        check_list(self, entries, [data.Open])
        self.assertEqual(entries[0].booking, 'STRICT')

    @parsedoc
    def test_entry_close(self, entries, _, __):
        """
          2013-05-18 close Assets:US:BestBank:Checking
        """
        check_list(self, entries, [data.Close])

    @parsedoc
    def test_entry_pad(self, entries, _, __):
        """
          2013-05-18 pad Assets:US:BestBank:Checking  Equity:Opening-Balances
        """
        check_list(self, entries, [data.Pad])

    @parsedoc
    def test_entry_event(self, entries, _, __):
        """
          2013-05-18 event "location" "New York, USA"
        """
        check_list(self, entries, [data.Event])

    @parsedoc
    def test_entry_note(self, entries, _, __):
        """
          2013-05-18 note Assets:US:BestBank:Checking  "Blah, di blah."
        """
        check_list(self, entries, [data.Note])

    @parsedoc
    def test_entry_price(self, entries, _, __):
        """
          2013-05-18 price USD   1.0290 CAD
        """
        check_list(self, entries, [data.Price])


class TestParserComplete(unittest.TestCase):
    """Tests of completion of balance."""

    @parsedoc
    def test_entry_transaction_single_posting_at_zero(self, entries, errors, _):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         0 USD
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, 0)

    @parsedoc
    def test_entry_transaction_imbalance_from_single_posting(self, entries, errors, _):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, 1 if interpolate_test.ERRORS_ON_RESIDUAL else 0)
        entry = entries[0]
        self.assertEqual(1, len(entry.postings))


class TestUglyBugs(unittest.TestCase):
    """Test all kinds of stupid sh*t that will inevitably occur in practice."""

    @parsedoc
    def test_empty_1(self, entries, errors, _):
        ""
        check_list(self, entries, [])
        check_list(self, errors, [])

    @parsedoc
    def test_empty_2(self, entries, errors, _):
        """

        """
        check_list(self, entries, [])
        check_list(self, errors, [])

    @parsedoc
    def test_comment(self, entries, errors, _):
        """
        ;; This is some comment.
        """
        check_list(self, entries, [])
        check_list(self, errors, [])

    def test_extra_whitespace_note(self):
        input_ = '\n2013-07-11 note Assets:Cash "test"\n\n  ;;\n'
        entries, errors, _ = parser.parse_string(input_)
        check_list(self, entries, [data.Note])
        check_list(self, errors, [])

    def test_extra_whitespace_transaction(self):
        input_ = '\n'.join([
            '2013-05-18 * "Nice dinner at Mermaid Inn"',
            '  Expenses:Restaurant         100 USD',
            '  Assets:US:Cash',
            '  ',
            ';; End of file',
        ])

        entries, errors, _ = parser.parse_string(input_, yydebug=0)
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [])

    def test_extra_whitespace_comment(self):
        input_ = '\n'.join([
            '2013-05-18 * "Nice dinner at Mermaid Inn"',
            '  Expenses:Restaurant         100 USD',
            '  Assets:US:Cash',
            '  ;;',
        ])
        entries, errors, _ = parser.parse_string(input_)
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [])

    @parsedoc
    def test_indent_eof(self, entries, errors, _):
        "\t"
        check_list(self, entries, [])
        check_list(self, errors, [])

    @parsedoc
    def test_comment_eof(self, entries, errors, _):
        "; comment"
        check_list(self, entries, [])
        check_list(self, errors, [])

    @parsedoc
    def test_no_empty_lines(self, entries, errors, _):
        """
          2013-05-01 open Assets:Cash   USD,CAD,EUR
          2013-05-02 close Assets:US:BestBank:Checking
          2013-05-03 pad Assets:US:BestBank:Checking  Equity:Opening-Balancess
          2013-05-04 event "location" "New York, USA"
          2013-05-05 * "Payee" "Narration"
            Assets:US:BestBank:Checking   100.00 USD
            Assets:Cash
          2013-05-06 note Assets:US:BestBank:Checking  "Blah, di blah."
          2013-05-07 price USD   1.0290 CAD
        """
        self.assertEqual(7, len(entries))
        self.assertEqual([], errors)


class TestTagStack(unittest.TestCase):

    @parsedoc
    def test_tag_left_unclosed(self, entries, errors, _):
        """
          pushtag #trip-to-nowhere
        """
        self.assertEqual(1, len(errors))
        self.assertTrue(re.search('Unbalanced tag', errors[0].message))

    @parsedoc
    def test_pop_invalid_tag(self, entries, errors, _):
        """
          poptag #trip-to-nowhere
        """
        self.assertTrue(errors)
        self.assertTrue(re.search('absent tag', errors[0].message))


class TestMultipleLines(unittest.TestCase):

    @parsedoc
    def test_multiline_narration(self, entries, errors, _):
        """
          2014-07-11 * "Hello one line
          and yet another,
          and why not another!"
            Expenses:Restaurant         100 USD
            Assets:Cash
        """
        self.assertEqual(1, len(entries))
        self.assertFalse(errors)
        self.assertFalse(lexer.LexerError in map(type, errors))
        expected_narration = "Hello one line\nand yet another,\nand why not another!"
        self.assertEqual(expected_narration, entries[0].narration)


class TestSyntaxErrors(unittest.TestCase):
    """Test syntax errors that occur within the parser.
    One of our goals is to recover and report without ever
    bailing out with an exception."""

    @parsedoc
    def test_lexer_default_rule_1(self, entries, errors, _):
        """
          Account:*:Bla
        """
        self.assertEqual(entries, [])
        self.assertTrue(errors)
        self.assertTrue(lexer.LexerError in map(type, errors))

    @parsedoc
    def test_lexer_default_rule_2(self, entries, errors, _):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Resta(urant        100 USD
            Expenses:Tips                10 USD
            Assets:US:Cash             -110 USD

          2013-05-20 balance Assets:US:Cash  -110 USD
        """
        # This should fail to parse the "Expenses:Resta(urant" account name.

        # Check that we indeed read the 'check' entry that comes after the one
        # with the error.
        check_list(self, entries, [data.Balance])

        # Make sure at least one error is reported.
        self.assertTrue(parser.ParserSyntaxError in map(type, errors))
        self.assertTrue(lexer.LexerError in map(type, errors))


class TestLineNumbers(unittest.TestCase):
    """Check that the line numbers line up correctly."""

    @parsedoc
    def test_line_numbers(self, entries, errors, _):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash

          2013-05-19 balance  Assets:US:Cash   -100 USD

          2013-05-20 note  Assets:US:Cash   "Something"

        """
        _, first_line = inspect.getsourcelines(
            TestLineNumbers.test_line_numbers.__wrapped__)
        first_line += 1

        self.assertEqual(2, entries[0].source.lineno - first_line)
        self.assertEqual(6, entries[1].source.lineno - first_line)
        self.assertEqual(8, entries[2].source.lineno - first_line)


class TestParserOptions(unittest.TestCase):

    @parsedoc
    def test_option_single_value(self, entries, errors, options_map):
        """
          option "title" "Super Rich"

        """
        option = options_map['title']
        self.assertEqual(option, 'Super Rich')

    @parsedoc
    def test_option_list_value(self, entries, errors, options_map):
        """
          option "documents" "/path/docs/a"
          option "documents" "/path/docs/b"
          option "documents" "/path/docs/c"

        """
        documents = options_map['documents']
        self.assertEqual(['/path/docs/a',
                          '/path/docs/b',
                          '/path/docs/c'], documents)

    @parsedoc
    def test_invalid_option(self, entries, errors, options_map):
        """
          option "bladibla_invalid" "Some value"

        """
        check_list(self, errors, [parser.ParserError])
        self.assertFalse("bladibla_invalid" in options_map)

    @parsedoc
    def test_readonly_option(self, entries, errors, options_map):
        """
          option "filename" "gniagniagniagniagnia"

        """
        check_list(self, errors, [parser.ParserError])
        self.assertNotEqual("filename", "gniagniagniagniagnia")


class TestParserPlugin(unittest.TestCase):

    @parsedoc
    def test_plugin(self, entries, errors, options_map):
        """
          plugin "beancount.plugin.unrealized"
        """
        self.assertFalse(errors)
        self.assertEqual([('beancount.plugin.unrealized', None)],
                         options_map['plugin'])

    @parsedoc
    def test_plugin_with_config(self, entries, errors, options_map):
        """
          plugin "beancount.plugin.unrealized" "Unrealized"
        """
        self.assertFalse(errors)
        self.assertEqual([('beancount.plugin.unrealized', 'Unrealized')],
                         options_map['plugin'])

    # Note: this is testing the old method, which will become obsolete one day.
    @parsedoc
    def test_plugin_as_option(self, entries, errors, options_map):
        """
          option "plugin" "beancount.plugin.unrealized"
        """
        self.assertFalse(errors)
        self.assertEqual([('beancount.plugin.unrealized', None)],
                         options_map['plugin'])

    @parsedoc
    def test_plugin_as_option_with_config(self, entries, errors, options_map):
        """
          option "plugin" "beancount.plugin.unrealized:Unrealized"
        """
        self.assertFalse(errors)
        self.assertEqual([('beancount.plugin.unrealized', 'Unrealized')],
                         options_map['plugin'])


class TestDisplayContextOptions(unittest.TestCase):

    @parsedoc
    def test_render_commas_no(self, _, __, options_map):
        """
          option "render_commas" "0"
        """
        self.assertEqual(False, options_map['render_commas'])

    @parsedoc
    def test_render_commas_yes(self, _, __, options_map):
        """
          option "render_commas" "1"
        """
        self.assertEqual(True, options_map['render_commas'])



class TestParserLinks(unittest.TestCase):

    @parsedoc
    def test_links(self, entries, errors, _):
        """
          2013-05-18 * "Something something" ^38784734873
            Expenses:Restaurant         100 USD
            Assets:US:Cash

        """
        check_list(self, entries, [data.Transaction])
        self.assertEqual(entries[0].links, set(['38784734873']))


class TestTransactions(unittest.TestCase):

    @parsedoc
    def test_simple_1(self, entries, errors, _):
        """
          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [])
        self.assertEqual(None, entries[0].payee)
        self.assertEqual("Nice dinner at Mermaid Inn", entries[0].narration)

    @parsedoc
    def test_simple_2(self, entries, errors, _):
        """

          2013-05-18 * "Nice dinner at Mermaid Inn"
            Expenses:Restaurant         100 USD
            Assets:US:Cash

          2013-05-20 * "Duane Reade" | "Toothbrush"
            Expenses:BathroomSupplies         4 USD
            Assets:US:BestBank:Checking

        """
        check_list(self, entries, [data.Transaction, data.Transaction])
        check_list(self, errors, [])
        self.assertEqual(None, entries[0].payee)
        self.assertEqual("Nice dinner at Mermaid Inn", entries[0].narration)
        self.assertEqual("Duane Reade", entries[1].payee)
        self.assertEqual("Toothbrush", entries[1].narration)

    @parsedoc
    def test_empty_narration(self, entries, errors, _):
        """
          2013-05-18 * ""
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [])
        self.assertEqual("", entries[0].narration)
        self.assertEqual(None, entries[0].payee)

    @parsedoc
    def test_no_narration(self, entries, errors, _):
        """
          2013-05-18 *
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [])
        self.assertEqual("", entries[0].narration)
        self.assertEqual(None, entries[0].payee)

    @parsedoc
    def test_payee_no_narration(self, entries, errors, _):
        """
          2013-05-18 * "Mermaid Inn" |
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        # Make sure a single string and a pipe raises an error, because '|' does
        # not carry any special meaning anymore.
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [parser.ParserError])
        self.assertEqual(None, entries[0].payee)
        self.assertEqual("Mermaid Inn", entries[0].narration)

    @parsedoc
    def test_too_many_strings(self, entries, errors, _):
        """
          2013-05-18 * "A" "B" "C"
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [])
        check_list(self, errors, [parser.ParserError])

    @parsedoc
    def test_link_and_then_tag(self, entries, errors, _):
        """
          2014-04-20 * "Money from CC" ^610fa7f17e7a #trip
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [])
        self.assertEqual("Money from CC", entries[0].narration)
        self.assertEqual(None, entries[0].payee)
        self.assertEqual(set(["610fa7f17e7a"]), entries[0].links)
        self.assertEqual(set(["trip"]), entries[0].tags)

    @parsedoc
    def test_tag_then_link(self, entries, errors, _):
        """
          2014-04-20 * #trip "Money from CC" ^610fa7f17e7a
            Expenses:Restaurant         100 USD
            Assets:US:Cash
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [])
        self.assertEqual("Money from CC", entries[0].narration)
        self.assertEqual(None, entries[0].payee)
        self.assertEqual(set(["610fa7f17e7a"]), entries[0].links)
        self.assertEqual(set(["trip"]), entries[0].tags)

    @parsedoc
    def test_zero_prices(self, entries, errors, _):
        """
          2014-04-20 * "Like a conversion entry"
            Equity:Conversions         100 USD @ 0 XFER
            Equity:Conversions         101 CAD @ 0 XFER
            Equity:Conversions         102 AUD @ 0 XFER
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [])

    @parsedoc
    def test_zero_units(self, entries, errors, _):
        """
          2014-04-20 * "Zero number of units"
            Assets:Investment         0 GOOG {500.00 USD}
            Assets:Cash
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [parser.ParserError])

    @parsedoc
    def test_zero_costs(self, entries, errors, _):
        """
          2014-04-20 * "Like a conversion entry"
            Assets:Investment         10 GOOG {0 USD}
            Assets:Cash
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors, [parser.ParserError])

    @parsedoc
    def test_imbalance(self, entries, errors, _):
        """
          2014-04-20 * "Busted!"
            Assets:Checking         100 USD
            Assets:Checking         -99 USD
        """
        check_list(self, entries, [data.Transaction])
        check_list(self, errors,
                   [interpolate.BalanceError]
                   if interpolate_test.ERRORS_ON_RESIDUAL else [])

    @parsedoc
    def test_no_postings(self, entries, errors, _):
        """
          2014-07-17 * "(JRN) INTRA-ACCOUNT TRANSFER" ^795422780
        """
        self.assertTrue(isinstance(entries[0].postings, list))


class TestCurrencies(unittest.TestCase):

    @parsedoc
    def test_parse_currencies(self, entries, errors, _):
        """
          2014-01-19 open Assets:Underscore    DJ_EURO
          2014-01-19 open Assets:Period        DJ.EURO
          2014-01-19 open Assets:Apostrophe    DJ'EURO
          2014-01-19 open Assets:Numbers       EURO123
        """
        self.assertFalse(errors)


class TestBalance(unittest.TestCase):

    @parsedoc
    def test_total_price(self, entries, errors, _):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      10 MSFT @@ 2000 USD
            Assets:Investments:Cash
        """
        posting = entries[0].postings[0]
        self.assertEqual(amount.from_string('200 USD'), posting.price)
        self.assertEqual(None, posting.position.lot.cost)

    @parsedoc
    def test_total_cost(self, entries, errors, _):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      10 MSFT {{2,000 USD}}
            Assets:Investments:Cash

          2013-05-18 * ""
            Assets:Investments:MSFT      10 MSFT {{2000 USD / 2014-02-25}}
            Assets:Investments:Cash
        """
        for entry in entries:
            posting = entry.postings[0]
            self.assertEqual(amount.from_string('200 USD'), posting.position.lot.cost)
            self.assertEqual(None, posting.price)


class TestMetaData(unittest.TestCase):

    @parsedoc
    def test_metadata_transaction__begin(self, entries, errors, _):
        """
          2013-05-18 * ""
            test: "Something"
            Assets:Investments:MSFT      10 MSFT @@ 2000 USD
            Assets:Investments:Cash
        """
        self.assertEqual(1, len(entries))
        self.assertEqual('Something', entries[0].source['test'])

    @parsedoc
    def test_metadata_transaction__middle(self, entries, errors, _):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      10 MSFT @@ 2000 USD
            test: "Something"
            Assets:Investments:Cash
        """
        self.assertEqual(1, len(entries))
        self.assertEqual({'test': 'Something'},
                         entries[0].postings[0].metadata)

    @parsedoc
    def test_metadata_transaction__end(self, entries, errors, _):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      10 MSFT @@ 2000 USD
            Assets:Investments:Cash
            test: "Something"
        """
        self.assertEqual(1, len(entries))
        self.assertEqual({'test': 'Something'},
                         entries[0].postings[1].metadata)

    @parsedoc
    def test_metadata_transaction__many(self, entries, errors, _):
        """
          2013-05-18 * ""
            test1: "Something"
            Assets:Investments:MSFT      10 MSFT @@ 2000 USD
            test2: "has"
            test3: "to"
            Assets:Investments:Cash
            test4: "come"
            test5: "from"
            test6: "this"
        """
        self.assertEqual(1, len(entries))
        self.assertEqual('Something', entries[0].source['test1'])
        self.assertEqual({'test2': 'has', 'test3': 'to'},
                         entries[0].postings[0].metadata)
        self.assertEqual({'test4': 'come', 'test5': 'from', 'test6': 'this'},
                         entries[0].postings[1].metadata)

    @parsedoc
    def test_metadata_transaction__indented(self, entries, errors, _):
        """
          2013-05-18 * ""
              test1: "Something"
            Assets:Investments:MSFT      10 MSFT @@ 2000 USD
              test2: "has"
              test3: "to"
            Assets:Investments:Cash
              test4: "come"
              test5: "from"
              test6: "this"
        """
        self.assertEqual(1, len(entries))
        self.assertEqual('Something', entries[0].source['test1'])
        self.assertEqual({'test2': 'has', 'test3': 'to'},
                         entries[0].postings[0].metadata)
        self.assertEqual({'test4': 'come', 'test5': 'from', 'test6': 'this'},
                         entries[0].postings[1].metadata)

    @parsedoc
    def test_metadata_transaction__repeated(self, entries, errors, _):
        """
          2013-05-18 * ""
            test: "Bananas"
            test: "Apples"
            test: "Oranges"
            Assets:Investments   100 USD
              test: "Bananas"
              test: "Apples"
            Income:Investments  -100 USD
        """
        self.assertEqual(1, len(entries))
        self.assertEqual('Bananas', entries[0].source['test'])
        self.assertEqual({'test': 'Bananas'}, entries[0].postings[0].metadata)
        self.assertEqual(3, len(errors))
        self.assertTrue(all(re.search('Duplicate.*metadata field', error.message)
                            for error in errors))

    @parsedoc
    def test_metadata_empty(self, entries, errors, _):
        """
          2013-05-18 * "blabla"
            oranges:
            bananas:

          2013-05-19 open Assets:Something
            apples:
        """
        self.assertFalse(errors)
        self.assertEqual(2, len(entries))
        self.assertEqual({'oranges', 'bananas', 'filename', 'lineno'},
                         entries[0].source.keys())
        self.assertEqual(None, entries[0].source['oranges'])
        self.assertEqual(None, entries[0].source['bananas'])
        self.assertEqual(entries[1].source['apples'], None)

    @parsedoc
    def test_metadata_other(self, entries, errors, _):
        """
          2013-01-01 open Equity:Other

          2013-01-01 open Assets:Investments
            test1: "Something"
            test2: "Something"

          2014-01-01 close Assets:Investments
            test1: "Something"

          2013-01-10 note Assets:Investments "Bla"
            test1: "Something"

          2013-01-31 pad Assets:Investments Equity:Other
            test1: "Something"

          2013-02-01 balance Assets:Investments  111.00 USD
            test1: "Something"

          2013-03-01 event "location" "Nowhere"
            test1: "Something"

          2013-03-01 document Assets:Investments "/path/to/something.pdf"
            test1: "Something"

          2013-03-01 price  GOOG  500 USD
            test1: "Something"
        """
        self.assertEqual(9, len(entries))

    @parsedoc
    def test_metadata_data_types(self, entries, errors, _):
        """
          2013-05-18 * ""
            string: "Something"
            account: Assets:Investments:Cash
            date: 2012-01-01
            currency: GOOG
            tag: #trip-florida
            number: 345.67
            amount: 345.67 USD
        """
        self.assertEqual(1, len(entries))
        self.assertTrue('filename' in entries[0].source)
        self.assertTrue('lineno' in entries[0].source)
        del entries[0].source['filename']
        del entries[0].source['lineno']
        self.assertEqual({
            'string': 'Something',
            'account': 'Assets:Investments:Cash',
            'date': datetime.date(2012, 1, 1),
            'currency': 'GOOG',
            'tag': 'trip-florida',
            'number': D('345.67'),
            'amount': amount.from_string('345.67 USD'),
            }, entries[0].source)


class TestLexerErrors(unittest.TestCase):

    @parsedoc
    def test_bad_account(self, entries, errors, _):
        """
          2011-01-01 open Assets:A
        """
        self.assertEqual([], entries)
        self.assertEqual([parser.ParserSyntaxError, lexer.LexerError],
                         list(map(type, errors)))

    @parsedoc
    def test_no_final_newline(self, entries, errors, _):
        """
          2014-11-02 *
            Assets:Something   1 USD
            Assets:Other      -1 USD"""
        self.assertFalse(errors)
        self.assertEqual(1, len(entries))
        self.assertEqual(2, len(entries[0].postings))
