"""
Unit tests for the various basic data types.
"""
import unittest

from beancount.core.amount import Decimal, Amount


class TestAmount(unittest.TestCase):

    def test_comparisons(self):
        amount1 = Amount(Decimal('100'), 'USD')
        amount2 = Amount(Decimal('100'), 'USD')
        self.assertEqual(amount1, amount2)


    def test_constructor(self):
        amount1 = Amount(Decimal('100'), 'USD')
        amount2 = Amount('100', 'USD')
        self.assertEqual(amount1, amount2)

        
