"""
Tests for text_utils.
"""
from beancount.utils.text_utils import replace_numbers

import re
import unittest


class TestTree(unittest.TestCase):

    def test_replace_numbers(self):
        self.assertEqual(replace_numbers(" 100.40 USD "), " XXX.XX USD ")
        self.assertEqual(replace_numbers(" -10.40 CAD "), " -XX.XX CAD ")
        self.assertEqual(replace_numbers("103,456.40 JPY"), "XXX,XXX.XX JPY")
        self.assertEqual(replace_numbers(" 10.0em"), " 10.0em")
        self.assertEqual(replace_numbers(" 10em"), " 10em")
        self.assertEqual(replace_numbers(" 10em"), " 10em")
        self.assertEqual(replace_numbers(" -10.40"), " -XX.XX")
