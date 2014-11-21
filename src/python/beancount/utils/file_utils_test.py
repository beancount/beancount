"""Tests for file utilities.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from beancount.utils import file_utils


class TestFileUtils(unittest.TestCase):

    def test_guess_file_format(self):
        self.assertEqual('csv', file_utils.guess_file_format('/user/output.csv'))
        self.assertEqual('text', file_utils.guess_file_format('/user/output.text'))
        self.assertEqual('text', file_utils.guess_file_format('/user/output.txt'))
        self.assertEqual('html', file_utils.guess_file_format('/user/output.html'))
        self.assertEqual('html', file_utils.guess_file_format('/user/output.xhtml'))
        self.assertEqual(None, file_utils.guess_file_format('/user/output'))

    def test_path_greedy_split(self):
        self.assertEqual(('/tmp/tmp.ju3h4h/blabla', None),
                         file_utils.path_greedy_split('/tmp/tmp.ju3h4h/blabla'))
        self.assertEqual(('/tmp/tmp.ju3h4h/bla', '.tgz'),
                         file_utils.path_greedy_split('/tmp/tmp.ju3h4h/bla.tgz'))
        self.assertEqual(('/tmp/tmp.ju3h4h/bla', '.tar.gz'),
                         file_utils.path_greedy_split('/tmp/tmp.ju3h4h/bla.tar.gz'))
