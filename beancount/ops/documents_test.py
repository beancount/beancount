"""
Tests for documents.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import textwrap
import unittest
from os import path

from beancount.utils import test_utils
from beancount.core import data
from beancount.ops import documents
from beancount.parser import cmptest
from beancount import loader


class TestDocuments(test_utils.TmpFilesTestBase, cmptest.TestCase):

    TEST_DOCUMENTS = [
        'root/Assets/US/Bank/Checking/other.txt',
        'root/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf',
        'root/Assets/US/Bank/Checking/otherdir/',
        'root/Assets/US/Bank/Checking/otherdir/another.txt',
        'root/Assets/US/Bank/Checking/otherdir/2014-06-08.bank-statement.pdf',
        'root/Assets/US/Bank/Savings/2014-07-01.savings.pdf',
        'root/Liabilities/US/Bank/',  # Empty directory.
    ]

    def test_process_documents(self):
        input_filename = path.join(self.root, 'input.beancount')
        with open(input_filename, 'w') as f:
            f.write(textwrap.dedent("""

              option "plugin_processing_mode" "raw"
              option "documents" "ROOT"

              2014-01-01 open Assets:US:Bank:Checking
              2014-01-01 open Liabilities:US:Bank

              2014-07-10 document Liabilities:US:Bank  "does-not-exist.pdf"

            """).replace('ROOT', self.root))
        entries, _, options_map = loader.load_file(input_filename)

        # In this test we set the root to the directory root, but only the
        # checking account is declared, and so only that entry should get
        # auto-generated from the files (the '2014-07-01.savings.pdf' file
        # should be ignored).
        #
        # Moreover, we generate an error from a non-existing file and we
        # assert that the entry is still indeed present.
        entries, errors = documents.process_documents(entries, options_map)

        # Check entries.
        expected_entries, _, __ = loader.load_string(textwrap.dedent("""
          option "plugin_processing_mode" "raw"
          2014-06-08 document Assets:US:Bank:Checking "ROOT/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf"
          2014-07-10 document Liabilities:US:Bank "ROOT/does-not-exist.pdf"
        """).replace('ROOT', self.root))
        self.assertEqualEntries(expected_entries,
                                [entry
                                 for entry in entries
                                 if isinstance(entry, data.Document)])

        self.assertEqual(0, len(errors))

    def test_process_documents_trailing_slash(self):
        input_filename = path.join(self.root, 'input.beancount')
        with open(input_filename, 'w') as f:
            f.write(textwrap.dedent("""

              option "plugin_processing_mode" "raw"
              option "documents" "ROOT/"

              2014-01-01 open Assets:US:Bank:Checking
              2014-01-01 open Liabilities:US:Bank

            """).replace('ROOT', self.root))
        entries, _, options_map = loader.load_file(input_filename)
        entries, errors = documents.process_documents(entries, options_map)
        doc_entries = [entry for entry in entries if isinstance(entry, data.Document)]
        self.assertEqual(1, len(doc_entries))

    def test_verify_document_files_exist(self):
        entries, _, options_map = loader.load_string(textwrap.dedent("""
          option "plugin_processing_mode" "raw"
          2014-06-08 document Assets:US:Bank:Checking "ROOT/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf"
          2014-07-01 document Assets:US:Bank:Savings  "ROOT/Assets/US/Bank/Savings/2014-07-01.savings.pdf"
          2014-07-10 document Assets:US:Bank:Savings  "ROOT/Assets/US/Bank/Savings/2014-07-10.something-else.pdf"
        """).replace('ROOT', self.root))

        _, errors = documents.verify_document_files_exist(entries, options_map)
        self.assertEqual(1, len(errors))
        document_error = errors[0]
        self.assertTrue(
            document_error.entry.filename.endswith('2014-07-10.something-else.pdf'))

    def test_find_documents(self):
        # Test with an absolute directory name.
        entries1, errors1 = documents.find_documents(
            self.root, '/tmp/input.beancount')
        self.assertEqual(2, len(entries1))
        self.assertEqual([], errors1)

        entry = entries1[0]
        self.assertTrue(isinstance(entry, data.Document))
        self.assertTrue(entry.filename.endswith(
            'Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf'))
        self.assertEqual('Assets:US:Bank:Checking', entry.account)
        self.assertEqual(datetime.date(2014, 6, 8), entry.date)

        entry = entries1[1]
        self.assertTrue(isinstance(entry, data.Document))
        self.assertTrue(entry.filename.endswith(
            'Assets/US/Bank/Savings/2014-07-01.savings.pdf'))
        self.assertEqual('Assets:US:Bank:Savings', entry.account)
        self.assertEqual(datetime.date(2014, 7, 1), entry.date)

        # Test with a relative directory name, the entries should be the same,
        # as all the filenames attached to document directives are absolute
        # paths.
        entries2, errors2 = documents.find_documents(
            'root', path.join(self.tempdir, 'input.beancount'))
        self.assertEqualEntries(entries1, entries2)

        # Test it out with dot-dots.
        entries3, errors3 = documents.find_documents(
            '..', path.join(self.root, 'Assets', 'input.beancount'))
        self.assertEqualEntries(entries1, entries3)

        # Try with a directory that does not exist, should generate an error.
        entries4, errors4 = documents.find_documents(
            'i-do-not-exist', path.join(self.tempdir, 'input.beancount'))
        self.assertEqual([], entries4)
        self.assertEqual(1, len(errors4))

        # Try with a directory with no matching names. Should generate empty.
        entries5, errors5 = documents.find_documents(
            self.tempdir, '/tmp/input.beancount')
        self.assertEqual([], entries5)
        self.assertEqual([], errors5)

        # Test it out with an account restriction.
        accounts = {'Assets:US:Bank:Checking'}
        entries6, errors6 = documents.find_documents(
            self.root, '/tmp/input.beancount', accounts)
        self.assertEqualEntries(entries1[:1], entries6)
        self.assertEqual([], errors1)


class TestDocumentsDate(test_utils.TmpFilesTestBase, cmptest.TestCase):

    TEST_DOCUMENTS = [
        'root/Assets/US/Bank/Checking/',
        'root/Assets/US/Bank/Checking/2014-02-31.bank-statement.pdf',
    ]

    def test_invalid_date(self):
        entries, errors = documents.find_documents(self.root, '/tmp/input.beancount')
        self.assertEqual(0, len(entries))
        self.assertRegex(errors[0].message, "Invalid date on document file")



class TestDocumentsConstraints(test_utils.TmpFilesTestBase, cmptest.TestCase):

    TEST_DOCUMENTS = [
        'root/Assets/',
        'root/Assets/US/2014-01-01.us.pdf',
        'root/Assets/US/Bank/2014-02-01.bank.pdf',
        'root/Assets/US/Bank/Checking/2014-03-01.checking.pdf',
        'root/Assets/US/Bank/Savings/2014-03-02.savings.pdf',
    ]

    def test_find_documents__no_constraint(self):
        expected_dates = [datetime.date(2014, 1, 1),
                          datetime.date(2014, 2, 1),
                          datetime.date(2014, 3, 1),
                          datetime.date(2014, 3, 2)]

        entries, errors = documents.find_documents(
            self.root, '/tmp/input.beancount')
        self.assertFalse(errors)
        self.assertEqual(expected_dates, [entry.date for entry in entries])

    def test_find_documents__with_leaf_constraints(self):
        expected_dates = [datetime.date(2014, 3, 1), datetime.date(2014, 3, 2)]

        entries, errors = documents.find_documents(
            self.root, '/tmp/input.beancount', {'Assets:US:Bank:Checking',
                                                'Assets:US:Bank:Savings'}, True)
        self.assertEqual(2, len(errors))
        self.assertEqual(expected_dates, [entry.date for entry in entries])

        entries, errors = documents.find_documents(
            self.root, '/tmp/input.beancount', {'Assets:US:Bank:Checking',
                                                'Assets:US:Bank:Savings'}, False)
        self.assertEqual(0, len(errors))
        self.assertEqual(expected_dates, [entry.date for entry in entries])

    def test_find_documents__with_parent_constraints(self):
        expected_dates = [datetime.date(2014, 2, 1)]

        entries, errors = documents.find_documents(
            self.root, '/tmp/input.beancount', {'Assets:US:Bank'}, True)
        self.assertEqual(3, len(errors))
        self.assertEqual(expected_dates, [entry.date for entry in entries])

        entries, errors = documents.find_documents(
            self.root, '/tmp/input.beancount', {'Assets:US:Bank'}, False)
        self.assertEqual(0, len(errors))
        self.assertEqual(expected_dates, [entry.date for entry in entries])


if __name__ == '__main__':
    unittest.main()
