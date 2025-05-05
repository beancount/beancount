__copyright__ = "Copyright (C) 2015-2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
from unittest import mock

# Skip the unittests if Google API client support is not installed.
try:
    import apiclient
except ImportError:
    apiclient = None
else:
    import download_docs


@unittest.skipIf(apiclient is None, "google-api-python-client not installed")
class TestDownloadDocs(unittest.TestCase):
    def test_find_index_document(self):
        service = mock.MagicMock()
        execute = service.files().list().execute
        execute.return_value = {"files": [{"id": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}]}
        docid = download_docs.find_index_document(service)
        self.assertEqual("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", docid)


if __name__ == "__main__":
    unittest.main()
