__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import http.client
import unittest
from unittest import mock

from beancount.utils import net_utils


class TestRetryingUrlopen(unittest.TestCase):

    def test_success_200(self):
        response = http.client.HTTPResponse(mock.MagicMock())
        response.status = 200
        with mock.patch('urllib.request.urlopen', return_value=response):
            self.assertIs(net_utils.retrying_urlopen('http://nowhere.com'), response)

    def test_success_other(self):
        response = http.client.HTTPResponse(mock.MagicMock())
        with mock.patch('urllib.request.urlopen', return_value=response):
            self.assertIsNone(net_utils.retrying_urlopen('http://nowhere.com'))

    def test_timeout_once(self):
        response = http.client.HTTPResponse(mock.MagicMock())
        response.status = 200
        with mock.patch('urllib.request.urlopen',
                        side_effect=[None, response]):
            self.assertIs(net_utils.retrying_urlopen('http://nowhere.com'), response)

    def test_max_retry(self):
        with mock.patch('urllib.request.urlopen',
                        side_effect=[None, None, None, None, None, None]):
            self.assertIsNone(net_utils.retrying_urlopen('http://nowhere.com'))
