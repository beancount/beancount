"""Tests for file utilities.
"""
import unittest

from beancount.utils import file_utils


class TestFileUtils(unittest.TestCase):

    def test_guess_file_format(self):
        self.assertEqual('csv', file_utils.guess_file_format('/user/output.csv'))
        self.assertEqual('txt', file_utils.guess_file_format('/user/output.txt'))
        self.assertEqual('html', file_utils.guess_file_format('/user/output.html'))
        self.assertEqual('html', file_utils.guess_file_format('/user/output.xhtml'))
        self.assertEqual('txt', file_utils.guess_file_format('/user/output'))
