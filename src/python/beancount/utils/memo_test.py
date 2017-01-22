__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest
import tempfile
import io
from os import path
from unittest import mock

from beancount.utils import memo


class TestMemoization(unittest.TestCase):

    def test_memoization_success(self):
        function = mock.MagicMock(return_value=io.BytesIO(b'Payload'))
        with tempfile.TemporaryDirectory() as tmp:
            mem_function = memo.memoize_recent_fileobj(function, path.join(tmp, 'cache.db'))
            mem_function()
            mem_function()
            mem_function()

            # Make sure to close before the test directory goes away.
            del mem_function

        self.assertEqual(1, function.call_count)

    def test_memoization_expired(self):
        now = datetime.datetime.now()
        function = mock.MagicMock(return_value=io.BytesIO(b'Payload'))
        expiration = datetime.timedelta(seconds=10*60)
        with tempfile.TemporaryDirectory() as tmp:
            mem_function = memo.memoize_recent_fileobj(function,
                                                       path.join(tmp, 'cache.db'),
                                                       expiration)
            with mock.patch('beancount.utils.memo.now', return_value=now):
                mem_function()
            within_time = now + datetime.timedelta(seconds=1)
            with mock.patch('beancount.utils.memo.now', return_value=within_time):
                mem_function()
            expired_time = now + expiration + datetime.timedelta(seconds=10)
            with mock.patch('beancount.utils.memo.now', return_value=expired_time):
                mem_function()
            self.assertEqual(2, function.call_count)

            # Make sure to close before the test directory goes away.
            del mem_function
