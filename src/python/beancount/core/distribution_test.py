"""
Tests for distribution.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from beancount.core import distribution


class TestDistribution(unittest.TestCase):

    def test_distribution(self):
        dist = distribution.Distribution()
        self.assertEqual(True, dist.empty())
        dist.update(1)
        dist.update(2)
        dist.update(2)
        dist.update(2)
        dist.update(3)
        dist.update(3)
        dist.update(4)
        self.assertEqual(2, dist.mode())
        self.assertEqual(1, dist.min())
        self.assertEqual(4, dist.max())
        self.assertEqual(False, dist.empty())
