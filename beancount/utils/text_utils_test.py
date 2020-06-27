"""
Tests for text_utils.
"""
__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

import tempfile
import unittest

from beancount.utils import text_utils


class TestTextUtils(unittest.TestCase):

    def test_replace_numbers(self):
        self.assertEqual(text_utils.replace_numbers(" 100.40 USD "), " XXX.XX USD ")
        self.assertEqual(text_utils.replace_numbers(" -10.40 CAD "), " -XX.XX CAD ")
        self.assertEqual(text_utils.replace_numbers("103,456.40 JPY"), "XXX,XXX.XX JPY")
        self.assertEqual(text_utils.replace_numbers(" 10.0em"), " 10.0em")
        self.assertEqual(text_utils.replace_numbers(" 10em"), " 10em")
        self.assertEqual(text_utils.replace_numbers(" 10em"), " 10em")
        self.assertEqual(text_utils.replace_numbers(" -10.40"), " -XX.XX")

    def test_fix_ampersand(self):
        with tempfile.NamedTemporaryFile('w') as input_file:
            input_file.write('S&P 500. Some other thing and &entity;')
            input_file.flush()
            tidy_file = text_utils.entitize_ampersand(input_file.name)
            with open(tidy_file.name) as f: contents = f.read()
            self.assertEqual('S&amp;P 500. Some other thing and &entity;', contents)


if __name__ == '__main__':
    unittest.main()
