"""
Unit tests for the inventory class.
"""
import unittest

from beancount2.data import Amount, Decimal
from beancount2.inventory import Inventory


class TestAmount(unittest.TestCase):

    def test_comparisons(self):
        amount1 = Amount(Decimal('100'), 'USD')
        amount2 = Amount(Decimal('100'), 'USD')
        self.assertEqual(amount1, amount2)


    def test_constructor(self):
        amount1 = Amount(Decimal('100'), 'USD')
        amount2 = Amount('100', 'USD')
        self.assertEqual(amount1, amount2)

        
