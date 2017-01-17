__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"


import unittest

from beancount.parser import hashsrc


class TestHashSource(unittest.TestCase):

    def test_hash_parser_source_files(self):
        source_hash = hashsrc.hash_parser_source_files()
        self.assertTrue(isinstance(source_hash, str))
        self.assertEqual(32, len(source_hash))

    def test_check_parser_source_files(self):
        hashsrc.check_parser_source_files()
