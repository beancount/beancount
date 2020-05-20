# coding: utf-8
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import re
import unittest

from beancount.utils import regexp_utils


def match(regexp, string):
    regexp = regexp_utils.re_replace_unicode(regexp)
    return re.match("^{}$".format(regexp), string)


class TestRegexpUtils(unittest.TestCase):

    def test_replace_unicode(self):
        self.assertTrue(match(r"[\p{L}]+", "Assets"))
        self.assertFalse(match(r"[\p{L}]+", "Assets:Checking"))
        self.assertFalse(match(r"[\p{L}]+", "Checking0"))
        self.assertTrue(match(r"[\p{L} ]+", "Adrián Medraño"))

        self.assertFalse(match(r"[\p{Lu}]+", "Assets"))
        self.assertTrue(match(r"[\p{Lu}]+", "ASSETS"))

        self.assertFalse(match(r"[\p{Nd}]+", "Assets"))
        self.assertTrue(match(r"[\p{Nd}]+", "78654865347"))


if __name__ == '__main__':
    unittest.main()
