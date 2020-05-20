"""Tests for pager code.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.utils import pager
from beancount.utils import test_utils


class TestPager(unittest.TestCase):

    def test_pager_nothreshold(self):
        with test_utils.capture() as stdout:
            with pager.ConditionalPager('/bin/cat') as file:
                file.write('TEST')
        self.assertEqual('TEST', stdout.getvalue())

    def test_pager_threshold_below(self):
        with test_utils.capture() as stdout:
            with pager.ConditionalPager('/bin/cat', 20) as file:
                file.write('TEST')
        self.assertEqual('TEST', stdout.getvalue())

    def test_pager_threshold_above(self):
        with test_utils.capture() as stdout:
            with pager.ConditionalPager('/bin/cat', 20) as file:
                for _ in range(21):
                    file.write('TEST\n')
        self.assertRegex(stdout.getvalue(), 'TEST\nTEST\n')


if __name__ == '__main__':
    unittest.main()
