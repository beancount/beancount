__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import logging
import tempfile
import unittest
from unittest import mock

from beancount.docs import download_docs


class TestDownloadDocs(unittest.TestCase):

    def test_find_index_document(self):
        service = mock.MagicMock()
        x = service.files().list().execute
        x.return_value = {'items': [{'id': 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'},
                                    {'id': 'BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB'}]}
        docid = download_docs.find_index_document(service)
        self.assertEqual('AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', docid)
