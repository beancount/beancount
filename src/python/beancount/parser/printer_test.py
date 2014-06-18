import io
from datetime import date
import unittest
import re

from beancount.parser import printer
from beancount.parser import parser
from beancount.core import data
from beancount.core import complete
from beancount.parser import cmptest


FILELOC = data.FileLocation('beancount/core/testing.beancount', 12345)

class TestPrinter(unittest.TestCase):

    def test_render_fileloc(self):
        fileloc_str = printer.render_fileloc(FILELOC)
        self.assertTrue(isinstance(fileloc_str, str))
        self.assertTrue(re.search('12345', fileloc_str))
        self.assertTrue(re.search(FILELOC.filename, fileloc_str))

    def test_format_and_print_error(self):
        entry = data.Open(FILELOC, date(2014, 1, 15), 'Assets:Bank:Checking', [])
        error = complete.BalanceError(FILELOC, "Example balance error", entry)
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
        printer.print_entries(entries1, oss1)
        entries2, errors, __ = parser.parse_string(oss1.getvalue())

        self.assertEqualEntries(entries1, entries2)
        self.assertFalse(errors)

        # Print out those reparsed and parse them back in.
        oss2 = io.StringIO()
        printer.print_entries(entries2, oss2)
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
