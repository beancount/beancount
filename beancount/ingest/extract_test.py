__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import unittest
import datetime
import io
import os
import textwrap
from unittest import mock

import click

from beancount.utils import test_utils
from beancount.utils import misc_utils
from beancount.parser import parser
from beancount import loader
from beancount.ingest import extract
from beancount.ingest import importer
from beancount.ingest import Ingest
from beancount.ingest.test_utils import TestScriptsBase, TestExamplesBase

class TestScriptExtractFromFile(test_utils.TestCase):

    def test_extract_from_file__empty(self):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.extract = mock.MagicMock(return_value=[])
        new_entries = extract.extract_from_file('/tmp/blabla.ofx', imp)
        self.assertEqual([], new_entries)

    def test_extract_from_file__ensure_sorted(self):
        entries, _, __ = loader.load_string("""

          2016-02-03 * "C"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

          2016-02-01 * "A"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

          2016-02-02 * "B"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

        """)

        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.extract = mock.MagicMock(return_value=entries)
        new_entries = extract.extract_from_file('/tmp/blabla.ofx', imp)
        self.assertEqual(3, len(entries))
        self.assertTrue(misc_utils.is_sorted(new_entries, key=lambda entry: entry.date))

    def test_extract_from_file__ensure_sanity(self):
        entries, _, __ = loader.load_string("""
          2016-02-01 * "A"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD
        """)
        # Break something.
        entries[-1] = entries[-1]._replace(narration=42)

        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.extract = mock.MagicMock(return_value=entries)
        with self.assertRaises(AssertionError):
            extract.extract_from_file('blabla.ofx', imp)

    def test_extract_from_file__min_date(self):
        entries, _, __ = loader.load_string("""

          2016-02-01 * "A"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

          2016-02-02 * "B"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

          2016-02-03 * "C"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

        """)
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.extract = mock.MagicMock(return_value=entries)
        new_entries = extract.extract_from_file(
            '/tmp/blabla.ofx', imp, min_date=datetime.date(2016, 2, 2))
        self.assertEqual(2, len(new_entries))
        self.assertEqual([datetime.date(2016, 2, 2), datetime.date(2016, 2, 3)],
                         [entry.date for entry in new_entries])

    @unittest.skip("FIXME: more this to call extract()")
    def test_extract_from_file__existing_entries(self):
        entries, _, __ = loader.load_string("""

          2016-02-01 * "A"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

          2016-02-02 * "B"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

          2016-02-03 * "C"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

          2016-02-04 * "D"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

        """)
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.extract = mock.MagicMock(return_value=[entries[1], entries[3]])

        new_entries = extract.extract_from_file(
            '/tmp/blabla.ofx', imp, entries)
        self.assertEqual(2, len(new_entries))
        self.assertEqual([datetime.date(2016, 2, 2), datetime.date(2016, 2, 4)],
                         [entry.date for entry in new_entries])

        # Check that the entries have also been marked.
        marked_entries = [entry
                          for entry in new_entries
                          if extract.DUPLICATE_META in entry.meta]
        self.assertEqual(new_entries, marked_entries)

    @unittest.skip("FIXME: Change this to call extract()")
    def test_extract_from_file__explicitly_marked_duplicates_entries(self):
        entries, _, __ = loader.load_string("""

          2016-02-01 * "A"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

          2016-02-02 * "B"
            Assets:Account1    10.00 USD
            Assets:Account2   -10.00 USD

        """)
        entries[1].meta[extract.DUPLICATE_META] = True
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.extract = mock.MagicMock(return_value=entries)

        new_entries, dup_entries = extract.extract_from_file('/tmp/blabla.ofx', imp, [])
        self.assertEqual(1, len(dup_entries))
        self.assertEqual([datetime.date(2016, 2, 1), datetime.date(2016, 2, 2)],
                         [entry.date for entry in new_entries])
        self.assertEqual([datetime.date(2016, 2, 2)],
                         [entry.date for entry in dup_entries])

    def test_extract_from_file__raises_exception(self):
        imp = mock.MagicMock()
        imp.identify = mock.MagicMock(return_value=True)
        imp.extract = mock.MagicMock(side_effect=ValueError("Unexpected error!"))
        with self.assertRaises(ValueError):
            extract.extract_from_file('/tmp/blabla.ofx', imp, [])


class TestPrintExtractedEntries(TestScriptsBase, unittest.TestCase):

    class ExtractTestImporter(importer.ImporterProtocol):

        def file_account(self, _):
            return 'Assets:Account1'

    @mock.patch.object(extract, 'extract_from_file')
    def test_print_extracted_entries(self, mock_extract_from_file):
        entries, _, __ = parser.parse_string("""

          2016-02-01 * "A"
            Assets:Account1    11.11 USD
            Assets:Account2   -11.11 USD

          2016-02-01 * "B"
            Assets:Account1    22.22 USD
            Assets:Account3   -22.22 USD

        """)
        mock_extract_from_file.return_value = entries

        entries[-2].meta[extract.DUPLICATE_META] = True

        oss = io.StringIO()
        extract.print_extracted_entries(entries, oss)

        self.assertEqual(textwrap.dedent("""\

        ; 2016-02-01 * "A"
        ;   Assets:Account1   11.11 USD
        ;   Assets:Account2  -11.11 USD

        2016-02-01 * "B"
          Assets:Account1   22.22 USD
          Assets:Account3  -22.22 USD

        """).strip(), oss.getvalue().strip())



class _LoaderImporter(importer.ImporterProtocol):
    """A mock importer which loads the file contents as a Beancount file itself.
    This is an odd trick for testing: I just put the expected extracted entries
    in the file and read them.
    """
    def __init__(self, filename, account):
        super().__init__()
        self.filename = filename
        self.account = account

    def identify(self, file):
        return path.basename(file.name) == path.basename(self.filename)

    def extract(self, file, existing_entries=None):
        entries, _, __ = loader.load_file(file.name)
        return entries

    def file_account(self, _):
        return self.account


class TestScriptExtract(test_utils.TestTempdirMixin, unittest.TestCase):

    def ingest(self, *args):
        runner = click.testing.CliRunner()
        result = runner.invoke(self.cli, args)
        return result

    def setUp(self):
        super().setUp()

        importers = [
            _LoaderImporter('checking.dl', 'Assets:Checking'),
            _LoaderImporter('credit.dl', 'Liabilities:CreditCard'),
        ]
        self.cli = Ingest(importers).cli()

        self.downloads = path.join(self.tempdir, 'Downloads')
        os.mkdir(self.downloads)

        self.dl_checking = path.join(self.downloads, 'checking.dl')
        with open(self.dl_checking, 'w') as file:
            file.write(textwrap.dedent("""\

              plugin "beancount.plugins.auto_accounts"

              2016-06-08 * "Withdrawal"
                Assets:Checking           -300.00 USD
                Assets:Cash

              2016-06-10 * "Electricity"
                Assets:Checking            -48.34 USD
                Expenses:Electricity

              2016-06-14 * "Internet"
                Assets:Checking            -48.34 USD
                Expenses:Internet

            """))

        self.dl_credit = path.join(self.downloads, 'credit.dl')
        with open(self.dl_credit, 'w') as file:
            file.write(textwrap.dedent("""\

              plugin "beancount.plugins.auto_accounts"

              2016-06-04 * "Drinks"
                Liabilities:CreditCard     -32.23 USD
                Expenses:Alcohol

              2016-06-07 * "Books"
                Liabilities:CreditCard     -87.30 USD
                Expenses:Books

              2016-06-10 * "Clothing"
                Liabilities:CreditCard     -87.30 USD
                Expenses:Clothing

            """))

    def test_extract(self):
        result = self.ingest('extract', path.join(self.tempdir, 'Downloads'))
        self.assertEqual(result.exit_code, 0)
        output = result.stdout

        self.assertRegex(output, r'/checking.dl')
        self.assertRegex(output, r'Assets:Cash +300.00 USD')
        self.assertRegex(output, r'Expenses:Electricity +48.34 USD')
        self.assertRegex(output, r'Expenses:Internet +48.34 USD')

        self.assertRegex(output, r'/credit.dl')
        self.assertRegex(output, r'Expenses:Alcohol +32.23 USD')
        self.assertRegex(output, r'Expenses:Books +87.30 USD')
        self.assertRegex(output, r'Expenses:Clothing +87.30 USD')

    @mock.patch.object(extract, 'find_duplicate_entries',
                       wraps=extract.find_duplicate_entries)
    def test_extract_find_dups_once_only_with_many_files(self, mock):
        result = self.ingest('extract', path.join(self.tempdir, 'Downloads'))
        self.assertEqual(result.exit_code, 0)
        mock.assert_called_once()

    def test_extract_with_previous_entries(self):
        existing_filename = path.join(self.tempdir, 'existing.beancount')
        with open(existing_filename, 'w') as file:
            file.write(textwrap.dedent("""\

              plugin "beancount.plugins.auto_accounts"

              2016-06-02 * "Rent"
                Assets:Checking          -1000.00 USD
                Expenses:Rent

              2016-06-08 * "Withdrawal"
                Assets:Checking           -300.00 USD
                Assets:Cash


              2016-06-01 * "Dinner"
                Liabilities:CreditCard     -47.34 USD
                Expenses:Restaurant

              2016-06-03 * "Groceries"
                Liabilities:CreditCard     -82.32 USD
                Expenses:Groceries

              2016-06-04 * "Drinks"
                Liabilities:CreditCard     -32.23 USD
                Expenses:Alcohol

            """))

        downloads = path.join(self.tempdir, 'Downloads')
        result = self.ingest('extract', downloads, '--existing', existing_filename)
        self.assertEqual(result.exit_code, 0)
        output = result.stdout

        self.assertRegex(output, r'/checking.dl')
        self.assertRegex(output, r'; +Assets:Cash +300.00 USD')
        self.assertRegex(output, r'Expenses:Electricity +48.34 USD')
        self.assertRegex(output, r'Expenses:Internet +48.34 USD')

        self.assertRegex(output, r'/credit.dl')
        self.assertRegex(output, r'; +Expenses:Alcohol +32.23 USD')
        self.assertRegex(output, r'Expenses:Books +87.30 USD')
        self.assertRegex(output, r'Expenses:Clothing +87.30 USD')

    def test_extract_no_files(self):
        emptydir = path.join(self.tempdir, 'Empty')
        os.makedirs(emptydir)
        result = self.ingest('extract', emptydir)
        self.assertEqual(result.exit_code, 0)
        self.assertRegex(result.stdout, r';; -\*- mode: beancount -\*-')


class TestExtractExamples(TestExamplesBase):

    def test_extract_examples(self):
        existing = path.join(self.example_dir, 'office', 'example.beancount')
        downloads = path.join(self.example_dir, 'Downloads')
        result = self.ingest('extract', downloads, '--existing', existing)
        self.assertEqual(result.exit_code, 0)
        output = result.stdout

        self.assertRegex(output, r';; -\*- mode: beancount -\*-')

        self.assertRegex(output, 'Downloads/UTrade20160215.csv')
        self.assertRegex(output, 'ORDINARY DIVIDEND~CSKO')
        self.assertRegex(output, 'Income:US:UTrade:CSKO:Gains')
        self.assertRegex(output, '2016-02-08 balance Assets:US:UTrade:Cash .*4665.89 USD')

        self.assertRegex(output, 'Downloads/ofxdownload.ofx')
        self.assertRegex(output, r'2013-12-16 \* "LES CAFES 400 LAFAYENEW YORK /')


if __name__ == '__main__':
    unittest.main()
