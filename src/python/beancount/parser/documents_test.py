"""
Tests for documents.
"""
import os
import re
import datetime
import shutil
import tempfile
import textwrap
from os import path

from beancount.utils import test_utils
from beancount.parser import documents
from beancount.parser import parser
from beancount.parser import printer


class TestDocuments(test_utils.TestCase):

    test_documents = [
        'root/Assets/US/Bank/Checking/other.txt',
        'root/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf',
        'root/Assets/US/Bank/Checking/otherdir/',
        'root/Assets/US/Bank/Checking/otherdir/another.txt',
        'root/Assets/US/Bank/Checking/otherdir/2014-06-08.bank-statement.pdf',
        'root/Assets/US/Bank/Savings/2014-07-01.savings.pdf',
        'root/Liabilities/US/Bank/',  # Empty directory.
    ]

    def setUp(self):
        self.tempdir = tempfile.mkdtemp(prefix="beancount-test-tmpdir.")
        self.root = path.join(self.tempdir, 'root')
        for filename in self.test_documents:
            abs_filename = path.join(self.tempdir, filename)
            if filename.endswith('/'):
                os.makedirs(abs_filename)
            else:
                parent_dir = path.dirname(abs_filename)
                if not path.exists(parent_dir):
                    os.makedirs(parent_dir)
                open(abs_filename, 'w')

    def tearDown(self):
        shutil.rmtree(self.tempdir, ignore_errors=True)

    def test_process_documents(self):

        # In this test we set the root to the directory root, but only the
        # checking account is declared, and so only that entry should get
        # auto-generated from the files (the '2014-07-01.savings.pdf' file
        # should be ignored).
        #
        # Moreover, we generate an error from a non-existing file and we assert
        # that the entry is still indeed present.

        entries, _, options_map = parser.parse_string(textwrap.dedent("""

          option "documents" "ROOT"

          2014-01-01 open Assets:US:Bank:Checking
          2014-01-01 open Liabilities:US:Bank

          2014-07-10 document Liabilities:US:Bank  "does-not-exist.pdf"

        """).replace('ROOT', self.root))

        entries, errors = documents.process_documents(entries, options_map,
                                                      '/tmp/input.beancount')

        # Check entrie
        expected_entries, _, __ = parser.parse_string(textwrap.dedent("""
          2014-06-08 document Assets:US:Bank:Checking "ROOT/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf"
          2014-07-10 document Liabilities:US:Bank "/tmp/does-not-exist.pdf"
        """).replace('ROOT', self.root))
        self.assertEqualEntries(expected_entries,
                                [entry
                                 for entry in entries
                                 if isinstance(entry, documents.Document)])

        self.assertEqual(1, len(errors))
        self.assertTrue(re.search(r'does-not-exist\.pdf', errors[0].message))

    def test_verify_document_entries(self):
        entries, _, __ = parser.parse_string(textwrap.dedent("""
          2014-06-08 document Assets:US:Bank:Checking "ROOT/Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf"
          2014-07-01 document Assets:US:Bank:Savings  "ROOT/Assets/US/Bank/Savings/2014-07-01.savings.pdf"
          2014-07-10 document Assets:US:Bank:Savings  "ROOT/Assets/US/Bank/Savings/2014-07-10.something-else.pdf"
        """).replace('ROOT', self.root))

        _, errors = documents.verify_document_entries(entries)
        self.assertEqual(1, len(errors))
        document_error = errors[0]
        self.assertTrue(document_error.entry.filename.endswith('2014-07-10.something-else.pdf'))

    def test_find_documents(self):
        # Test with an absolute directory name.
        entries1, errors1 = documents.find_documents(
            self.root, '/tmp/input.beancount')
        self.assertEqual(2, len(entries1))
        self.assertEqual([], errors1)

        entry = entries1[0]
        self.assertTrue(isinstance(entry, documents.Document))
        self.assertTrue(entry.filename.endswith(
            'Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf'))
        self.assertEqual('Assets:US:Bank:Checking', entry.account)
        self.assertEqual(datetime.date(2014, 6, 8), entry.date)

        entry = entries1[1]
        self.assertTrue(isinstance(entry, documents.Document))
        self.assertTrue(entry.filename.endswith(
            'Assets/US/Bank/Savings/2014-07-01.savings.pdf'))
        self.assertEqual('Assets:US:Bank:Savings', entry.account)
        self.assertEqual(datetime.date(2014, 7, 1), entry.date)

        # Test with a relative directory name, the entries should be the same,
        # as all the filenames attached to document directivesa are absolute
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

    def test_walk_accounts(self):
        actual_data = [
            (root[len(self.root):], account, dirs, files)
            for root, account, dirs, files in documents.walk_accounts(self.root)]

        self.assertEqual([
            ('/Assets/US', 'Assets:US',
             ['Bank'],
             []),
            ('/Assets/US/Bank', 'Assets:US:Bank',
             ['Checking', 'Savings'],
             []),
            ('/Assets/US/Bank/Checking', 'Assets:US:Bank:Checking',
             ['otherdir'],
             ['2014-06-08.bank-statement.pdf', 'other.txt']),

            ('/Assets/US/Bank/Savings', 'Assets:US:Bank:Savings',
             [],
             ['2014-07-01.savings.pdf']),

            ('/Liabilities/US', 'Liabilities:US',
             ['Bank'],
             []),
            ('/Liabilities/US/Bank', 'Liabilities:US:Bank',
             [],
             []),
            ], actual_data)
