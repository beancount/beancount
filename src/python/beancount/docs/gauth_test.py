__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import tempfile
import unittest
from unittest import mock

# Skip the unittests if oauth support is not installed.
try:
    import oauth2client
    import httplib2
except ImportError:
    oauth2client = None
else:
    from beancount.docs import gauth


@unittest.skipIf(oauth2client is None, "oauth2client and/or httplib2 not installed")
class TestGAuth(unittest.TestCase):

    @mock.patch('oauth2client.service_account.ServiceAccountCredentials.from_json_keyfile_name')
    def test_get_auth_via_service_account(self, mock_factory):
        mock_factory.return_value = mock.MagicMock()
        scopes = ['https://www.googleapis.com/auth/drive',
                  'https://www.googleapis.com/auth/drive.scripts']
        credentials, http = gauth.get_auth_via_service_account(scopes)
        self.assertEqual(mock_factory.return_value, credentials)
        self.assertIsInstance(http, httplib2.Http)
