"""
Unit tests for the Position class.
"""
import unittest

from beancount.core.data import Lot, ZERO
from beancount.core.position import Position


class TestPosition(unittest.TestCase):

    def test_compare_zero_to_none(self):
        pos1 = Position(Lot("CAD", None, None), ZERO)
        pos2 = None
        self.assertEqual(pos1, pos2)
        self.assertEqual(pos2, pos1)
