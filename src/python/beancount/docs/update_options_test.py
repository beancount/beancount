__author__ = 'Martin Blais <blais@furius.ca>'

import unittest
from unittest import mock

from beancount.docs import update_options


class TestUpdateOptions(unittest.TestCase):

    def test_get_options_doc_id(self):
        docid = update_options.get_options_doc_id()
        self.assertTrue(docid)
        self.assertIsInstance(docid, str)

    @mock.patch('apiclient.discovery.build')
    def test_replace_gdocs_document(self, build):
        docid = update_options.get_options_doc_id()
        update_options.replace_gdocs_document(None, docid, "Title", "Contents")
