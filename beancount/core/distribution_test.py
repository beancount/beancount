"""
Tests for distribution.
"""

__copyright__ = "Copyright (C) 2015-2017, 2019, 2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core import distribution


class TestDistributionBase(unittest.TestCase):
    def assertDist(self, dist, mode, min, max):
        self.assertEqual(mode, dist.mode())
        self.assertEqual(min, dist.min())
        self.assertEqual(max, dist.max())
        self.assertEqual(False, dist.empty())


class TestDistribution(TestDistributionBase):
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
        self.assertDist(dist, 2, 1, 4)


class TestDistributionUpdateFrom(TestDistributionBase):
    def test_update_from(self):
        dist1 = distribution.Distribution()
        dist1.update(1)
        dist1.update(2)
        dist1.update(2)
        dist1.update(3)
        self.assertDist(dist1, 2, 1, 3)

        dist2 = distribution.Distribution()
        dist2.update(2)
        dist2.update(3)
        dist2.update(3)
        dist2.update(3)
        dist2.update(4)
        self.assertDist(dist2, 3, 2, 4)

        dist1.update_from(dist2)
        self.assertDist(dist1, 3, 1, 4)


if __name__ == "__main__":
    unittest.main()
