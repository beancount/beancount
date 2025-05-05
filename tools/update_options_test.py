__copyright__ = "Copyright (C) 2015-2017, 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
from unittest import mock

# Skip the unittests if Google API client support is not installed.
try:
    import apiclient
except ImportError:
    apiclient = None
else:
    from tools import update_options


@unittest.skipIf(apiclient is None, "google-api-python-client not installed")
class TestUpdateOptions(unittest.TestCase):
    def test_get_options_doc_id(self):
        docid = update_options.get_options_docid()
        self.assertTrue(docid)
        self.assertIsInstance(docid, str)

    @mock.patch("apiclient.discovery.build")
    def test_replace_gdocs_document(self, build):
        docid = update_options.get_options_docid()
        update_options.replace_gdocs_document(None, docid, "Title", "Contents")


if __name__ == "__main__":
    unittest.main()
