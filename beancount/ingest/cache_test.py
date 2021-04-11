__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import tempfile
import shutil
import sys
import unittest
from unittest import mock

from beancount.ingest import cache
from beancount.utils import file_type


class TestFileMemo(unittest.TestCase):

    def test_cache(self):
        with tempfile.NamedTemporaryFile() as tmpfile:
            shutil.copy(__file__, tmpfile.name)
            wrap = cache._FileMemo(tmpfile.name)

            # Check attributes.
            self.assertEqual(tmpfile.name, wrap.name)

            # Check that caching works.
            converter = mock.MagicMock(return_value='abc')
            self.assertEqual('abc', wrap.convert(converter))
            self.assertEqual('abc', wrap.convert(converter))
            self.assertEqual('abc', wrap.convert(converter))
            self.assertEqual(1, converter.call_count)

    @unittest.skipIf(not file_type.magic, 'python-magic is not installed')
    def test_cache_head_and_contents(self):
        with tempfile.NamedTemporaryFile() as tmpfile:
            shutil.copy(__file__, tmpfile.name)
            wrap = cache._FileMemo(tmpfile.name)

            contents = wrap.convert(cache.contents)
            self.assertIsInstance(contents, str)
            self.assertGreater(len(contents), 128)

            contents2 = wrap.contents()
            self.assertEqual(contents, contents2)

            head = wrap.convert(cache.head(128))
            self.assertIsInstance(head, str)
            self.assertEqual(128, len(head))

            mimetype = wrap.convert(cache.mimetype)
            self.assertRegex(mimetype, r'text/(x-(python|c\+\+)|plain)')

    def test_cache_head_obeys_explict_utf8_encoding_avoids_chardet_exception(self):
        data = b'asciiHeader1,\xf0\x9f\x8d\x8fHeader1,asciiHeader2'
        with mock.patch('builtins.open', mock.mock_open(read_data=data)):
            string = cache._FileMemo('filepath').head(encoding='utf-8')
            self.assertEqual(string, data.decode('utf8'))

    def test_cache_head_encoding(self):
        data = b'asciiHeader1,\xf0\x9f\x8d\x8fHeader1,asciiHeader2'
        # The 15th bytes is in the middle of the unicode character.
        num_bytes = 15
        if sys.version_info < (3, 7):
            # Reading the documentation, a partial read from longer
            # mocked file data should work just fine, however, this
            # does not seem the case in practice.
            data = data[:num_bytes]
        with mock.patch('builtins.open', mock.mock_open(read_data=data)):
            string = cache._FileMemo('filepath').head(num_bytes, encoding='utf-8')
            self.assertEqual(string, 'asciiHeader1,')

if __name__ == '__main__':
    unittest.main()
