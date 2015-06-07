__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import tempfile
import unittest
from unittest import mock

import httplib2

from beancount.docs import gauth


class TestGAuth(unittest.TestCase):

    def test_get_argparser(self):
        parser = gauth.get_argparser()
        self.assertIsInstance(parser, argparse.ArgumentParser)

    @mock.patch('oauth2client.tools.run_flow')
    def test_get_authenticated_http(self, run_flow):
        parser = gauth.get_argparser()
        args = parser.parse_args(['--storage', tempfile.NamedTemporaryFile().name])
        http = gauth.get_authenticated_http('https://www.googleapis.com/auth/drive', args)
        self.assertIsInstance(http, httplib2.Http)
