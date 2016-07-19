__author__ = 'Martin Blais <blais@furius.ca>'

import unittest
from unittest import mock

# Skip the unittests if Google API client support is not installed.
try:
    import apiclient
except ImportError:
    apiclient = None
else:
    from beancount.docs import download_docs


@unittest.skipIf(apiclient is None, "google-api-python-client not installed")
class TestDownloadDocs(unittest.TestCase):

    def test_find_index_document(self):
        service = mock.MagicMock()
        execute = service.files().list().execute
        execute.return_value = {
            'files': [
                {'id': 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'}]}
        docid = download_docs.find_index_document(service)
        self.assertEqual('AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', docid)
