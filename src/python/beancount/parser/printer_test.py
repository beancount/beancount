__author__ = "Martin Blais <blais@furius.ca>"

import io
from datetime import date
import unittest
import re
import textwrap
import sys

from beancount.parser import printer
from beancount.parser import parser
from beancount.parser import cmptest
from beancount.core import data
from beancount.core import interpolate
from beancount.utils import test_utils


SOURCE = data.Source('beancount/core/testing.beancount', 12345)

class TestPrinter(unittest.TestCase):

    def test_render_source(self):
        source_str = printer.render_source(SOURCE)
        self.assertTrue(isinstance(source_str, str))
        self.assertTrue(re.search('12345', source_str))
        self.assertTrue(re.search(SOURCE.filename, source_str))

    def test_format_and_print_error(self):
        entry = data.Open(SOURCE, date(2014, 1, 15), 'Assets:Bank:Checking', [], None)
        error = interpolate.BalanceError(SOURCE, "Example balance error", entry)
        error_str = printer.format_error(error)
        self.assertTrue(isinstance(error_str, str))

        oss = io.StringIO()
        printer.print_error(error, oss)
        self.assertTrue(isinstance(oss.getvalue(), str))

        oss = io.StringIO()
        printer.print_errors([error], oss)
        self.assertTrue(isinstance(oss.getvalue(), str))


class TestEntryPrinter(cmptest.TestCase):

    def assertRoundTrip(self, entries1, errors1):
        self.assertFalse(errors1)

        # Print out the entries and parse them back in.
        oss1 = io.StringIO()
        printer.print_entries(entries1, file=oss1)
        entries2, errors, __ = parser.parse_string(oss1.getvalue())

        self.assertEqualEntries(entries1, entries2)
        self.assertFalse(errors)

        # Print out those reparsed and parse them back in.
        oss2 = io.StringIO()
        printer.print_entries(entries2, file=oss2)
        entries3, errors, __ = parser.parse_string(oss2.getvalue())

        self.assertEqualEntries(entries1, entries3)
        self.assertFalse(errors)

        # Compare the two output texts.
        self.assertEqual(oss2.getvalue(), oss1.getvalue())

    @parser.parsedoc
    def test_Transaction(self, entries, errors, __):
        """
        2014-06-08 *
          Assets:Account1       111.00 BEAN
          Assets:Cash

        2014-06-08 * "Narration"
          Assets:Account1       111.00 BEAN
          Assets:Cash

        2014-06-08 * "Payee" | "Narration"
          Assets:Account1       111.00 BEAN
          Assets:Cash

        2014-06-08 * "Payee" "Narration" ^link1 ^link2 #tag1 #tag2
          Assets:Account1       111.00 BEAN
          Assets:Cash

        2014-06-08 * "Narration"
          Assets:Account1       111.00 BEAN {53.24 USD}
          Assets:Cash

        2014-06-08 !
          Assets:Account1       111.00 BEAN {53.24 USD} @ 55.02 USD
          Assets:Account2       111.00 BEAN {53.24 USD}
          Assets:Account3       111.00 BEAN @ 55.02 USD
          Assets:Account4       111.00 BEAN
          Assets:Cash

        2014-06-08 *
          Assets:Account1         111.00 BEAN
          ! Assets:Account2       111.00 BEAN
          * Assets:Account3       111.00 BEAN
          ? Assets:Account4      -333.00 BEAN

        2014-06-09 * "An entry like a conversion entry"
          Assets:Account1         1 USD @ 0 OTHER
          Assets:Account2         1 CAD @ 0 OTHER
        """
        self.assertRoundTrip(entries, errors)

    @parser.parsedoc
    def test_Balance(self, entries, errors, __):
        """
        2014-06-08 balance Assets:Account1     53.24 USD
        """
        self.assertRoundTrip(entries, errors)

    @parser.parsedoc
    def test_Note(self, entries, errors, __):
        """
        2014-06-08 note Assets:Account1 "Note"
        """
        self.assertRoundTrip(entries, errors)

    @parser.parsedoc
    def test_Document(self, entries, errors, __):
        """
        2014-06-08 document Assets:Account1 "/path/to/document.pdf"
        2014-06-08 document Assets:Account1 "path/to/document.csv"
        """
        self.assertRoundTrip(entries, errors)

    @parser.parsedoc
    def test_Pad(self, entries, errors, __):
        """
        2014-06-08 pad Assets:Account1 Assets:Account2
        """
        self.assertRoundTrip(entries, errors)

    @parser.parsedoc
    def test_Open(self, entries, errors, __):
        """
        2014-06-08 open Assets:Account1
        2014-06-08 open Assets:Account1  USD
        2014-06-08 open Assets:Account1  USD,CAD,EUR
        """
        self.assertRoundTrip(entries, errors)

    @parser.parsedoc
    def test_Close(self, entries, errors, __):
        """
        2014-06-08 close Assets:Account1
        """
        self.assertRoundTrip(entries, errors)

    @parser.parsedoc
    def test_Price(self, entries, errors, __):
        """
        2014-06-08 price  BEAN   53.24 USD
        2014-06-08 price  USD   1.09 CAD
        """
        self.assertRoundTrip(entries, errors)

    @parser.parsedoc
    def test_Event(self, entries, errors, __):
        """
        2014-06-08 event "location" "New York, NY, USA"
        2014-06-08 event "employer" "Four Square"
        """
        self.assertRoundTrip(entries, errors)


def characterize_spaces(text):
    """Classify each line to a particular type.

    Args:
      text: A string, the text to classify.
    Returns:
      A list of line types, one for each line.
    """
    lines = []
    for line in text.splitlines():
        if re.match(r'\d\d\d\d-\d\d-\d\d open', line):
            linecls = 'open'
        elif re.match(r'\d\d\d\d-\d\d-\d\d price', line):
            linecls = 'price'
        elif re.match(r'\d\d\d\d-\d\d-\d\d', line):
            linecls = 'txn'
        elif re.match(r'[ \t]$', line):
            linecls = 'empty'
        else:
            linecls = None
        lines.append(linecls)
    return lines


class TestPrinterSpacing(unittest.TestCase):

    maxDiff = 8192

    def test_interline_spacing(self):
        input_text = textwrap.dedent("""\
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-01-01 open Assets:Cash

        2014-06-08 *
          Assets:Account1       111.00 BEAN
          Assets:Cash

        2014-06-08 * "Narration"
          Assets:Account1       111.00 BEAN
          Assets:Cash

        2014-06-08 * "Payee" | "Narration"
          Assets:Account2       111.00 BEAN
          Assets:Cash

        2014-10-01 close Assets:Account2

        2014-10-11 price BEAN   10 USD
        2014-10-12 price BEAN   11 USD
        2014-10-13 price BEAN   11 USD
        """)
        entries, _, __ = parser.parse_string(input_text)

        oss = io.StringIO()
        printer.print_entries(entries, file=oss)

        expected_classes = characterize_spaces(input_text)
        actual_classes = characterize_spaces(oss.getvalue())

        self.assertEqual(expected_classes, actual_classes)


class TestDisplayContext(test_utils.TestCase):

    maxDiff = 2048

    @parser.parsedoc
    def test_precision(self, entries, errors, options_map):
        """
        2014-07-01 *
          Assets:Account              1 INT
          Assets:Account            1.1 FP1
          Assets:Account          22.22 FP2
          Assets:Account        333.333 FP3
          Assets:Account      4444.4444 FP4
          Assets:Account    55555.55555 FP5
          Assets:Cash
        """
        dcontext = options_map['display_context']
        oss = io.StringIO()
        printer.print_entries(entries, dcontext, file=oss)

        expected_str = textwrap.dedent("""
        2014-07-01 *
          Assets:Account             1 INT
          Assets:Account           1.1 FP1
          Assets:Account         22.22 FP2
          Assets:Account       333.333 FP3
          Assets:Account     4444.4444 FP4
          Assets:Account   55555.55555 FP5
          Assets:Cash               -1 INT
          Assets:Cash             -1.1 FP1
          Assets:Cash           -22.22 FP2
          Assets:Cash         -333.333 FP3
          Assets:Cash       -4444.4444 FP4
          Assets:Cash     -55555.55555 FP5
        """)
        self.assertLines(expected_str, oss.getvalue())


class TestPrinterAlignment(test_utils.TestCase):

    def test_align_position_strings(self):
        aligned_strings, width = printer.align_position_strings([
            '45 GOOG {504.30 USD}',
            '4 GOOG {504.30 USD / 2014-11-11}',
            '9.9505 USD',
            '',
            '-22473.32 CAD @ 1.10 USD',
            'UNKNOWN',
            '76400.203',
        ])
        self.assertEqual(40, width)
        self.assertEqual([
            '       45 GOOG {504.30 USD}             ',
            '        4 GOOG {504.30 USD / 2014-11-11}',
            '   9.9505 USD                           ',
            '                                        ',
            '-22473.32 CAD @ 1.10 USD                ',
            'UNKNOWN                                 ',
            '76400.203                               ',
            ], aligned_strings)

    @parser.parsedoc
    def test_align(self, entries, errors, options_map):
        """
        2014-07-01 * "Something"
          Expenses:Commissions  20000 USD
          Expenses:Commissions  9.9505 USD
        """
        dcontext = options_map['display_context']
        oss = io.StringIO()
        printer.print_entries(entries, dcontext, file=oss)
        expected_str = textwrap.dedent("""
        2014-07-01 * "Something"
          Expenses:Commissions  20000 USD
          Expenses:Commissions     10 USD
        """)
        self.assertEqual(expected_str, oss.getvalue())

    @parser.parsedoc
    def test_align_min_width_account(self, entries, errors, options_map):
        """
        2014-07-01 * "Something"
          Expenses:Commissions  20000 USD
          Expenses:Commissions  9.9505 USD
        """
        dcontext = options_map['display_context']
        oss = io.StringIO()
        eprinter = printer.EntryPrinter(dcontext, min_width_account=40)
        oss.write(eprinter(entries[0]))
        expected_str = textwrap.dedent("""\
        2014-07-01 * "Something"
          Expenses:Commissions                      20000 USD
          Expenses:Commissions                         10 USD
        """)
        self.assertEqual(expected_str, oss.getvalue())

    @parser.parsedoc
    def test_align_with_weight(self, entries, errors, options_map):
        """
        2014-07-01 * "Something"
          Assets:US:Investments:GOOG          45 GOOG {504.30 USD}
          Assets:US:Investments:GOOG           4 GOOG {504.30 USD / 2014-11-11}
          Expenses:Commissions            9.9505 USD
          Assets:US:Investments:Cash   -22473.32 CAD @ 1.10 USD
        """
        dcontext = options_map['display_context']

        oss = io.StringIO()
        printer.print_entries(entries, dcontext, render_weights=False, file=oss)
        expected_str = ''.join([
            '\n',
            '2014-07-01 * "Something"\n',
            '  Assets:US:Investments:GOOG         45 GOOG {504.30 USD}             \n',
            '  Assets:US:Investments:GOOG          4 GOOG {504.30 USD / 2014-11-11}\n',
            '  Expenses:Commissions             9.95 USD                           \n',
            '  Assets:US:Investments:Cash  -22473.32 CAD @ 1.10 USD                \n',
            ])
        self.assertEqual(expected_str, oss.getvalue())

        oss = io.StringIO()
        printer.print_entries(entries, dcontext, render_weights=True, file=oss)
        expected_str = textwrap.dedent("""
        2014-07-01 * "Something"
          Assets:US:Investments:GOOG         45 GOOG {504.30 USD}               ;  22693.50 USD
          Assets:US:Investments:GOOG          4 GOOG {504.30 USD / 2014-11-11}  ;   2017.20 USD
          Expenses:Commissions             9.95 USD                             ;      9.95 USD
          Assets:US:Investments:Cash  -22473.32 CAD @ 1.10 USD                  ; -24720.65 USD
        """)
        self.assertEqual(expected_str, oss.getvalue())
