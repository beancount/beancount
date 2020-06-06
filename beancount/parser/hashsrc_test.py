__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"


import unittest

from beancount.parser import hashsrc
from beancount.parser import _parser


class TestHashSource(unittest.TestCase):

    def test_hash_parser_source_files(self):
        source_hash = hashsrc.hash_parser_source_files()
        self.assertTrue(isinstance(source_hash, str))
        self.assertEqual(32, len(source_hash))

    def test_check_parser_source_files(self):
        self.assertTrue(len(_parser.SOURCE_HASH) >= 32)
        hashsrc.check_parser_source_files(_parser)


if __name__ == '__main__':
    unittest.main()
