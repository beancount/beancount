__copyright__ = "Copyright (C) 2014-2020, 2024-2025  Martin Blais"
__license__ = "GNU GPLv2"

import io
import os
import re
import tempfile
import textwrap
import unittest
from datetime import date
from decimal import Decimal

from beancount import loader
from beancount.core import data
from beancount.core.amount import Amount
from beancount.ops.balance import BalanceError
from beancount.parser import cmptest
from beancount.parser import printer
from beancount.utils import test_utils

META = data.new_metadata("beancount/core/testing.beancount", 12345)


class TestPrinter(unittest.TestCase):
    def test_methods_coverage(self):
        for klass in data.ALL_DIRECTIVES:
            self.assertTrue(hasattr(printer.EntryPrinter, klass.__name__))

    def test_render_source(self):
        source_str = printer.render_source(META)
        self.assertTrue(isinstance(source_str, str))
        self.assertRegex(source_str, "12345")
        self.assertRegex(source_str, META["filename"])

    def test_format_and_print_error(self):
        entry = data.Balance(
            META,
            date(2014, 1, 15),
            "Assets:Bank:Checking",
            Amount(Decimal("0"), "USD"),
            None,
            None,
        )
        error = BalanceError(META, "Example balance error", entry)
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
        oss1.write('option "plugin_processing_mode" "raw"\n')
        printer.print_entries(entries1, file=oss1)
        entries2, errors, __ = loader.load_string(oss1.getvalue())

        self.assertEqualEntries(entries1, entries2)
        self.assertFalse(errors)

        # Print out those reparsed and parse them back in.
        oss2 = io.StringIO()
        oss2.write('option "plugin_processing_mode" "raw"\n')
        printer.print_entries(entries2, file=oss2)
        entries3, errors, __ = loader.load_string(oss2.getvalue())

        self.assertEqualEntries(entries1, entries3)
        self.assertFalse(errors)

        # Compare the two output texts.
        self.assertEqual(oss2.getvalue(), oss1.getvalue())

    def assertRoundTripViaRealFile(self, entries1, errors1, test_write_source=True):
        """
        The same as assertRoundTrip, but the 1st time saves ledger in string
        format to a real file as an opposite to io.StringIO().
        This ensures, that when parsed back from file, entries will contain file
        name and line number metadata.

        Args:
            test_write_source - if True, will test funtionality write_source,
            which activates printing of the source file and line number for
            every transaction
        """
        self.assertFalse(errors1)

        # Not using context manager to make this test compatible with Windows
        # Contex manager can be used as of python 3.12
        # see https://github.com/python/cpython/issues/58451
        # https://github.com/beancount/beancount/issues/222
        oss1 = tempfile.NamedTemporaryFile(
            "w", suffix=".beancount", delete=False, encoding="utf-8"
        )

        oss1.write('option "plugin_processing_mode" "raw"\n')
        printer.print_entries(entries1, file=oss1)

        oss1.close()

        with open(oss1.name, "r", encoding="utf-8") as file:
            oss1_value = file.read()

        # entries2 will contain information about filename and line number in
        # metadata
        entries2, errors, __ = loader.load_file(oss1.name)

        self.assertEqualEntries(entries1, entries2)
        self.assertFalse(errors)

        oss2 = io.StringIO()
        oss2.write('option "plugin_processing_mode" "raw"\n')

        # converting entries2 to string now also with the commented out source
        # information
        printer.print_entries(entries2, file=oss2, write_source=test_write_source)
        oss2_value = oss2.getvalue()
        entries3, errors, __ = loader.load_string(oss2_value)

        # testing, that qnt of '; source' subscrings  is equal to qnt of
        # entries
        if test_write_source:
            self.assertEqual(oss2_value.count("; source"), len(entries3))

        self.assertEqualEntries(entries1, entries3)
        self.assertFalse(errors)

        if test_write_source:
            self.assertNotEqual(oss2_value, oss1_value)
        else:
            self.assertEqual(oss2.getvalue(), oss1_value)

        # Finally deleting temporary file
        os.unlink(oss1.name)

    def test_write_source(self):
        """
        Targeted testing of the write_source, functionality which activates
        printing of the source file and line number for every transaction in
        a commented out form
        """
        ORIGINAL_LEDGER = textwrap.dedent("""\
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Cash

        2014-06-08 *
          Assets:Account1       111.00 BEAN
          Assets:Cash          -111.00 BEAN
        """)

        EXPECTED_OUTPUT_LEDGER_TEMPL = textwrap.dedent("""
        ; source: __path_to_file__:1:
        2014-01-01 open Assets:Account1

        ; source: __path_to_file__:2:
        2014-01-01 open Assets:Cash

        ; source: __path_to_file__:4:
        2014-06-08 *
          Assets:Account1   111.00 BEAN
          Assets:Cash      -111.00 BEAN
        """)

        # Not using context manager to make this test compatible with Windows
        # Contex manager can be used as of python 3.12
        # see https://github.com/python/cpython/issues/58451
        # https://github.com/beancount/beancount/issues/222
        oss1 = tempfile.NamedTemporaryFile(
            "w", suffix=".beancount", delete=False, encoding="utf-8"
        )
        oss1.write(ORIGINAL_LEDGER)
        oss1.close()
        # entries2 will contain the file name and line number in metadata
        entries2, errors, __ = loader.load_file(oss1.name)
        self.assertFalse(errors)

        expected_output_ledger = EXPECTED_OUTPUT_LEDGER_TEMPL.replace(
            "__path_to_file__", oss1.name
        ).splitlines()

        # Print ledger with the source location information.
        oss2 = io.StringIO()
        printer.print_entries(entries2, file=oss2, write_source=True)
        oss2.seek(0)

        # Get printed lines. Strip trailing whitespace to allow the
        # expected output to do not contain trailing whitespace and
        # appease linters.
        printed = [line.rstrip() for line in oss2.readlines()]

        self.assertEqual(expected_output_ledger, printed)

    @loader.load_doc()
    def test_Transaction(self, entries, errors, __):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-01-01 open Assets:Account3
        2014-01-01 open Assets:Account4
        2014-01-01 open Assets:Cash

        2014-06-08 *
          Assets:Account1       111.00 BEAN
          Assets:Cash          -111.00 BEAN

        2014-06-08 * "Narration"
          Assets:Account1       111.00 BEAN
          Assets:Cash          -111.00 BEAN

        2014-06-08 * "Payee" "Narration"
          Assets:Account1       111.00 BEAN
          Assets:Cash          -111.00 BEAN

        2014-06-08 * "Payee" "Narration" ^link1 ^link2 #tag1 #tag2
          Assets:Account1       111.00 BEAN
          Assets:Cash          -111.00 BEAN

        2014-06-08 * "Narration"
          Assets:Account1       111.00 BEAN {53.24 USD}
          Assets:Cash         -5909.64 USD

        2014-06-08 !
          Assets:Account1       111.00 BEAN {53.24 USD} @ 55.02 USD
          Assets:Account2       111.00 BEAN {53.24 USD}
          Assets:Account3       111.00 BEAN @ 55.02 USD
          Assets:Account4       111.00 BEAN
          Assets:Cash          -111.00 BEAN
          Assets:Cash         -17926.5 USD

        2014-06-08 *
          Assets:Account1         111.00 BEAN
          ! Assets:Account2       111.00 BEAN
          * Assets:Account3       111.00 BEAN
          ? Assets:Account4      -333.00 BEAN

        2014-06-09 * "An entry like a conversion entry"
          Assets:Account1         1 USD @ 0 OTHER
          Assets:Account2         1 CAD @ 0 OTHER

        2014-06-10 * "Entry with escaped \\"quotes\\""
          Assets:Account1       111.00 BEAN
          Assets:Cash          -111.00 BEAN

        2014-06-20 custom "budget" Assets:Account2 "balance < 200.00 USD" 200.00 10.00 USD
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        # TODO: This test fails on Windows for the entry with escaped symbols
        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_Balance(self, entries, errors, __):
        """
        2014-06-01 open Assets:Account1
        2014-06-08 balance Assets:Account1     0.00 USD
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_BalanceTolerance(self, entries, errors, __):
        """
        2014-06-01 open Assets:Account1
        2014-06-01 open Assets:Cash

        2014-06-02 * "Deposit"
          Assets:Account1       199.95 USD
          Assets:Cash          -199.95 USD

        2014-06-04 balance Assets:Account1     200.00 ~0.05 USD
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_Note(self, entries, errors, __):
        """
        2014-06-01 open Assets:Account1
        2014-06-08 note Assets:Account1 "Note"
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    def test_Document(self):
        # The beancount parser processes escaped characters in all strings,
        # including the file path in the ``document`` directive.  Windows uses
        # backslashes as file path separator character.  Unless these are
        # escaped, the path separators are threated as the beginning of an
        # escape sequence by the parser, producing the wrong result.
        #
        # The path separator characters can be escaped as double backslashes.
        # However ``beancount.parser.print_entries()`` does not do that,
        # breaking the round-trip test.  To change ``print_entries()`` to escape
        # special characters would be an incompatible change.
        #
        # The solution could be to use a simple file name in the test, however,
        # the beancount loader transforms all relative paths into absolute
        # paths, introducing path separator characters.  Windows also accepts
        # forward slashes as path separators, therefore, the solution is to
        # craft the test to use absolute paths with forward slashes on all
        # platforms.
        path = os.path.join(os.path.dirname(__file__), "document.pdf").replace("\\", "/")

        ledger = textwrap.dedent("""\
        option "plugin_processing_mode" "raw"
        2014-06-01 open Assets:Account1
        2014-06-08 document Assets:Account1 "{path}"
        2014-06-08 document Assets:Account1 "{path}" #tag1 #tag2 ^link1 ^link2
        2014-06-08 document Assets:Account1 "{path}" #tag1
        2014-06-08 document Assets:Account1 "{path}" ^link1
        """).format(path=path)
        entries, errors, __ = loader.load_string(ledger)

        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_Query(self, entries, errors, __):
        """
        2014-06-08 query "cash" "SELECT sum(position) WHERE currency = 'USD'"
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_Pad(self, entries, errors, __):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-06-08 pad Assets:Account1 Assets:Account2
        2014-10-01 balance Assets:Account1  1 USD
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_Open(self, entries, errors, __):
        """
        2014-06-08 open Assets:Account1
        2014-06-08 open Assets:Account2  USD
        2014-06-08 open Assets:Account3  USD,CAD,EUR
        2014-06-08 open Assets:Account4  HOOL   "NONE"
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_Close(self, entries, errors, __):
        """
        2014-01-01 open  Assets:Account1
        2014-06-08 close Assets:Account1
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_Price(self, entries, errors, __):
        """
        2014-06-08 price  BEAN   53.24 USD
        2014-06-08 price  USD   1.09 CAD
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    @loader.load_doc()
    def test_Event(self, entries, errors, __):
        """
        2014-06-08 event "location" "New York, NY, USA"
        2014-06-08 event "employer" "Four Square"
        """
        with self.subTest("RoundTrip test via StringIO"):
            self.assertRoundTrip(entries, errors)

        with self.subTest("RoundTrip test via real file"):
            self.assertRoundTripViaRealFile(entries, errors)

    def test_metadata(self):
        meta = data.new_metadata("beancount/core/testing.beancount", 12345)
        meta["something"] = r'a"\c'
        oss = io.StringIO()
        printer.EntryPrinter().write_metadata(meta, oss)
        self.assertEqual('  something: "a\\"\\\\c"\n', oss.getvalue())


def characterize_spaces(text):
    """Classify each line to a particular type.

    Args:
      text: A string, the text to classify.
    Returns:
      A list of line types, one for each line.
    """
    lines = []
    for line in text.splitlines():
        if re.match(r"\d\d\d\d-\d\d-\d\d open", line):
            linecls = "open"
        elif re.match(r"\d\d\d\d-\d\d-\d\d price", line):
            linecls = "price"
        elif re.match(r"\d\d\d\d-\d\d-\d\d", line):
            linecls = "txn"
        elif re.match(r"[ \t]$", line):
            linecls = "empty"
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

        2014-06-08 * "Payee" "Narration"
          Assets:Account2       111.00 BEAN
          Assets:Cash

        2014-10-01 close Assets:Account2

        2014-10-11 price BEAN   10 USD
        2014-10-12 price BEAN   11 USD
        2014-10-13 price BEAN   11 USD
        """)
        entries, _, __ = loader.load_string(input_text)

        oss = io.StringIO()
        printer.print_entries(entries, file=oss)

        expected_classes = characterize_spaces(input_text)
        actual_classes = characterize_spaces(oss.getvalue())

        self.assertEqual(expected_classes, actual_classes)


class TestDisplayContext(test_utils.TestCase):
    maxDiff = 2048

    @loader.load_doc()
    def test_precision(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:Account
        2014-01-01 open Assets:Cash

        2014-07-01 *
          Assets:Account              1 INT
          Assets:Account            1.1 FP1
          Assets:Account          22.22 FP2
          Assets:Account        333.333 FP3
          Assets:Account      4444.4444 FP4
          Assets:Account    55555.55555 FP5
          Assets:Cash               -1 INT
          Assets:Cash             -1.1 FP1
          Assets:Cash           -22.22 FP2
          Assets:Cash         -333.333 FP3
          Assets:Cash       -4444.4444 FP4
          Assets:Cash     -55555.55555 FP5
        """
        dcontext = options_map["dcontext"]
        oss = io.StringIO()
        printer.print_entries(entries, dcontext, file=oss)

        expected_str = textwrap.dedent("""
        2014-01-01 open Assets:Account
        2014-01-01 open Assets:Cash

        """) + textwrap.dedent("""\
        2014-07-01 *
          Assets:Account             1 INT
          Assets:Cash               -1 INT
          Assets:Account           1.1 FP1
          Assets:Cash             -1.1 FP1
          Assets:Account         22.22 FP2
          Assets:Cash           -22.22 FP2
          Assets:Account       333.333 FP3
          Assets:Cash         -333.333 FP3
          Assets:Account     4444.4444 FP4
          Assets:Cash       -4444.4444 FP4
          Assets:Account   55555.55555 FP5
          Assets:Cash     -55555.55555 FP5
        """)
        self.assertLines(expected_str, oss.getvalue())


class TestPrinterAlignment(test_utils.TestCase):
    maxDiff = None

    def test_align_position_strings(self):
        aligned_strings, width = printer.align_position_strings(
            [
                "45 HOOL {504.30 USD}",
                "4 HOOL {504.30 USD / 2014-11-11}",
                "9.9505 USD",
                "",
                "-22473.32 CAD @ 1.10 USD",
                "UNKNOWN",
                "76400.203",
            ]
        )
        self.assertEqual(40, width)
        self.assertEqual(
            [
                "       45 HOOL {504.30 USD}             ",
                "        4 HOOL {504.30 USD / 2014-11-11}",
                "   9.9505 USD                           ",
                "                                        ",
                "-22473.32 CAD @ 1.10 USD                ",
                "UNKNOWN                                 ",
                "76400.203                               ",
            ],
            aligned_strings,
        )

    @loader.load_doc()
    def test_align(self, entries, errors, options_map):
        """
        2014-01-01 open Expenses:Commissions

        2014-07-01 * "Something"
          Expenses:Commissions  20000 USD
          Expenses:Commissions  9.9505 USD
          Expenses:Commissions  -20009.9505 USD
        """
        dcontext = options_map["dcontext"]
        oss = io.StringIO()
        printer.print_entries(entries, dcontext, file=oss)
        expected_str = textwrap.dedent("""\
        2014-01-01 open Expenses:Commissions

        2014-07-01 * "Something"
          Expenses:Commissions   20000.0000 USD
          Expenses:Commissions       9.9505 USD
          Expenses:Commissions  -20009.9505 USD
        """)
        self.assertEqual(expected_str, oss.getvalue())

    @loader.load_doc()
    def test_align_min_width_account(self, entries, errors, options_map):
        """
        2014-01-01 open Expenses:Commissions

        2014-07-01 * "Something"
          Expenses:Commissions  20000 USD
          Expenses:Commissions  9.9505 USD
          Expenses:Commissions  -20009.9505 USD
        """
        dcontext = options_map["dcontext"]
        oss = io.StringIO()
        eprinter = printer.EntryPrinter(dcontext, min_width_account=40)
        oss.write(eprinter(entries[1]))
        expected_str = textwrap.dedent("""\
        2014-07-01 * "Something"
          Expenses:Commissions                       20000.0000 USD
          Expenses:Commissions                           9.9505 USD
          Expenses:Commissions                      -20009.9505 USD
        """)
        self.assertEqual(expected_str, oss.getvalue())

    @loader.load_doc()
    def test_align_with_weight(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:US:Investments:HOOL
        2014-01-01 open Expenses:Commissions
        2014-01-01 open Assets:US:Investments:Cash

        2014-07-01 * "Something"
          Assets:US:Investments:HOOL          45 HOOL {504.30 USD}
          Assets:US:Investments:HOOL           4 HOOL {504.30 USD, 2014-11-11}
          Expenses:Commissions            9.9520 USD
          Assets:US:Investments:Cash   -22473.32 CAD @ 1.10 USD
        """
        self.assertFalse(errors)
        dcontext = options_map["dcontext"]

        # oss = io.StringIO()
        # printer.print_entries(entries, dcontext, render_weights=False, file=oss)
        # expected_str = ''.join([
        #     '2014-01-01 open Assets:US:Investments:HOOL\n',
        #     '2014-01-01 open Expenses:Commissions\n',
        #     '2014-01-01 open Assets:US:Investments:Cash\n',
        #     '\n',
        #     '2014-07-01 * "Something"\n',
        #     '  Assets:US:Investments:HOOL         45 HOOL {504.30 USD}            \n',
        #     '  Assets:US:Investments:HOOL          4 HOOL {504.30 USD, 2014-11-11}\n',
        #     '  Expenses:Commissions             9.95 USD                          \n',
        #     '  Assets:US:Investments:Cash  -22473.32 CAD @ 1.1000 USD             \n',
        #     ])
        # self.assertEqual(expected_str, oss.getvalue())

        oss = io.StringIO()
        printer.print_entries(entries, dcontext, render_weights=True, file=oss)
        expected_str = textwrap.dedent("""\
        2014-01-01 open Assets:US:Investments:HOOL
        2014-01-01 open Expenses:Commissions
        2014-01-01 open Assets:US:Investments:Cash

        2014-07-01 * "Something"
          Assets:US:Investments:HOOL         45 HOOL {504.30 USD, 2014-07-01}  ;    22693.50 USD
          Assets:US:Investments:HOOL          4 HOOL {504.30 USD, 2014-11-11}  ;     2017.20 USD
          Expenses:Commissions             9.95 USD                            ;      9.9520 USD
          Assets:US:Investments:Cash  -22473.32 CAD @ 1.1000 USD               ; -24720.6520 USD
        """)
        self.assertEqual(expected_str, oss.getvalue())


class TestPrinterMisc(test_utils.TestCase):
    @loader.load_doc(expect_errors=True)
    def test_no_valid_account(self, entries, errors, options_map):
        """
        2000-01-01 * "Test"
          Assets:Foo

        2000-01-01 * "Test"
          Assets:Foo
          Assets:Bar
        """
        oss = io.StringIO()
        printer.print_entries(entries, file=oss)

    def test_metadata(self):
        input_string = textwrap.dedent("""

        2000-01-01 open Assets:US:Investments:Cash
          name: "Investment account"
        2000-01-01 open Assets:US:Investments:HOOL

        2000-01-02 commodity VHT
          asset-class: "Stocks"
          name: "Vanguard Health Care ETF"

        2000-01-03 * "Something"
          doc: "some-statement.pdf"
          Assets:US:Investments:Cash  -23.45 USD
            note: "No commission"
          Assets:US:Investments:HOOL    1 HOOL {23.45 USD, 2000-01-03}
            settlement: 2000-01-05

        """)
        entries, errors, options_map = loader.load_string(input_string)
        self.assertFalse(errors)
        oss = io.StringIO()
        printer.print_entries(entries, file=oss)
        self.assertLines(input_string, oss.getvalue())

    def test_zero_cost(self):
        input_string = textwrap.dedent("""

        2000-01-01 open Assets:Invest:Cash
        2000-01-01 open Assets:Invest:Options

        2000-01-03 *
          Assets:Invest:Options  100 HOOLOPT {0 USD, 2000-01-03}
          Assets:Invest:Cash       0 USD
        """)
        entries, errors, options_map = loader.load_string(input_string)
        self.assertFalse(errors)
        oss = io.StringIO()
        printer.print_entries(entries, file=oss)
        self.assertLines(input_string, oss.getvalue())

    def test_very_small_number(self):
        # We want to make sure we never render with scientific notation.
        input_string = textwrap.dedent("""

          2016-11-05 open Expenses:Bank:Conversion
          2016-11-05 open Expenses:Gifts
          2016-11-05 open Expenses:Entertainment:Travel
          2016-11-05 open Assets:Current:Bank:SomeBank

          2016-11-05 * "Aquarium"
              Expenses:Gifts                               435 DKK
              Expenses:Entertainment:Travel                340 DKK
              Expenses:Bank:Conversion
              Assets:Current:Bank:SomeBank            -204.17 BGN @@ 775.00 DKK
        """)
        entries, errors, options_map = loader.load_string(input_string)
        self.assertFalse(errors)
        oss = io.StringIO()
        printer.print_entries(entries, file=oss)
        self.assertRegex(oss.getvalue(), "0.0000000000000000000000001 DKK")

    def test_render_missing(self):
        # We want to make sure we never render with scientific notation.
        input_string = textwrap.dedent("""

          2019-01-19 * "Fitness First" "Last training session"
            Expenses:Sports:Gym:Martin
            Assets:Martin:Cash

        """)
        entries, errors, options_map = loader.load_string(input_string)
        txn = errors[0].entry
        oss = io.StringIO()
        printer.print_entry(txn, file=oss)

    def test_render_meta_with_None(self):
        # Issue 378.
        input_string = textwrap.dedent("""

          2019-01-01 open Assets:A
          2019-01-01 open Assets:B

          2019-02-28 txn "Test"
            Assets:A                       10.00 USD
            Assets:B                      -10.00 USD
            foo:

        """)
        entries, errors, options_map = loader.load_string(input_string)
        self.assertFalse(errors)
        self.assertIs(entries[-1].postings[-1].meta["foo"], None)


if __name__ == "__main__":
    unittest.main()
