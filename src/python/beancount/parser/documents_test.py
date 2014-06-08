"""
Tests for documents.
"""
import os
from os import path

from beancount.utils import test_utils
from beancount.parser import documents


class TestDocuments(test_utils.TestCase):

    def test_process_documents(self):
        pass #def process_documents(entries, filename, documents_dirs):

    def test_verify_document_entries(self):
        pass #def verify_document_entries(document_entries):

    def test_process_auto_documents(self):
        pass #def process_auto_documents(input_filename, document_dirs, accounts):

    def test_find_documents(self):
        pass #def find_documents(root_directory, location_filename, accounts):





    def test_walk_accounts(self):
        with test_utils.tempdir() as tempdir:
            for filename in [
                    'Assets/US/Bank/Checking/',
                    'Assets/US/Bank/Checking/other.txt',
                    'Assets/US/Bank/Checking/2014-06-08.bank-statement.pdf',
                    'Assets/US/Bank/Checking/otherdir/',
                    'Assets/US/Bank/Checking/otherdir/another.txt',
                    'Assets/US/Bank/Checking/otherdir/2014-06-08.bank-statement.pdf',
                    ]:
                abs_filename = path.join(tempdir, filename)
                if filename.endswith('/'):
                    os.makedirs(abs_filename)
                else:
                    open(abs_filename, 'w')

            actual_data = [(root[len(tempdir):], account, dirs, files)
                           for root, account, dirs, files in documents.walk_accounts(tempdir)]
            self.assertEqual([
                ('/Assets/US', 'Assets:US',
                 ['Bank'],
                 []),
                ('/Assets/US/Bank', 'Assets:US:Bank',
                 ['Checking'],
                 []),
                ('/Assets/US/Bank/Checking', 'Assets:US:Bank:Checking',
                 ['otherdir'],
                 ['2014-06-08.bank-statement.pdf', 'other.txt']),
                ], actual_data)


__incomplete__ = True
