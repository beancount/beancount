"""
Unit tests for the Position class.
"""
import unittest

from beancount.core.amount import ZERO
from beancount.core.position import Lot, Position


class TestPosition(unittest.TestCase):

    def test_compare_zero_to_none(self):
        pos1 = Position(Lot("CAD", None, None), ZERO)
        pos2 = None
        self.assertEqual(pos1, pos2)
        self.assertEqual(pos2, pos1)
