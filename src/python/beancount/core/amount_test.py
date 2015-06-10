__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from beancount.core.number import D
from beancount.core.number import Decimal
from beancount.core.number import ZERO
from beancount.core.amount import Amount
from beancount.core import amount
from beancount.core import display_context


class TestAmount(unittest.TestCase):

    def test_constructor(self):
        amount1 = Amount(Decimal('100'), 'USD')
        amount2 = Amount('100', 'USD')
        self.assertEqual(amount1, amount2)

        amount = Amount('100,034.02', 'USD')
        self.assertEqual(amount.number, Decimal('100034.02'))

    def test_mutation(self):
        amount1 = Amount(Decimal('100'), 'USD')

        # Test how changing existing attributes should fail.
        with self.assertRaises(AttributeError) as ctx:
            amount1.currency = 'CAD'
        self.assertRegexpMatches("can't set attribute", str(ctx.exception))

        with self.assertRaises(AttributeError) as ctx:
            amount1.number = D('200')
        self.assertRegexpMatches("can't set attribute", str(ctx.exception))

        # Try setting a new attribute.
        with self.assertRaises(AttributeError):
            amount1.something = 42


    def test_fromstring(self):
        amount1 = Amount(Decimal('100'), 'USD')
        amount2 = Amount.from_string('100 USD')
        self.assertEqual(amount1, amount2)

        Amount.from_string('  100.00 USD  ')

        with self.assertRaises(ValueError):
            Amount.from_string('100')

        with self.assertRaises(ValueError):
            Amount.from_string('USD')

        with self.assertRaises(ValueError):
            Amount.from_string('100.00 U')

    def test_tostring(self):
        amount = Amount('100034.023', 'USD')

        self.assertEqual('100034.023 USD', str(amount))

        dcontext = display_context.DisplayContext()
        dformat = dcontext.build(commas=True)
        self.assertEqual('100,034.023 USD', amount.to_string(dformat))

    def test_comparisons(self):
        amount1 = Amount(Decimal('100'), 'USD')
        amount2 = Amount(Decimal('100'), 'USD')
        self.assertEqual(amount1, amount2)

        amount3 = Amount(Decimal('101'), 'USD')
        self.assertNotEqual(amount1, amount3)

    def test_hash(self):
        amount = Amount('100,034.027456', 'USD')
        self.assertTrue({amount: True})
        self.assertTrue({amount})

        amount2 = Amount('100,034.027456', 'CAD')
        self.assertEqual(2, len({amount: True, amount2: False}))

    def test_sort__explicit(self):
        # Check that we can sort currency-first.
        amounts = [
            Amount('1', 'USD'),
            Amount('201', 'EUR'),
            Amount('3', 'USD'),
            Amount('100', 'CAD'),
            Amount('2', 'USD'),
            Amount('200', 'EUR'),
        ]
        amounts = sorted(amounts, key=amount.amount_sortkey)
        self.assertEqual([
            Amount('100', 'CAD'),
            Amount('200', 'EUR'),
            Amount('201', 'EUR'),
            Amount('1', 'USD'),
            Amount('2', 'USD'),
            Amount('3', 'USD'),
        ], amounts)

    def test_sort__natural(self):
        # Check that we can sort currency-first.
        amounts = [
            Amount('1', 'USD'),
            Amount('201', 'EUR'),
            Amount('3', 'USD'),
            Amount('100', 'CAD'),
            Amount('2', 'USD'),
            Amount('200', 'EUR'),
        ]
        amounts = sorted(amounts)
        self.assertEqual([
            Amount('100', 'CAD'),
            Amount('200', 'EUR'),
            Amount('201', 'EUR'),
            Amount('1', 'USD'),
            Amount('2', 'USD'),
            Amount('3', 'USD'),
        ], amounts)

    def test_neg(self):
        amount_ = Amount('100', 'CAD')
        self.assertEqual(Amount('-100', 'CAD'), -amount_)

        amount_ = Amount('-100', 'CAD')
        self.assertEqual(Amount('100', 'CAD'), -amount_)

        amount_ = Amount('0', 'CAD')
        self.assertEqual(Amount('0', 'CAD'), -amount_)

    def test_mult(self):
        amount_ = Amount('100', 'CAD')
        self.assertEqual(Amount('102.1', 'CAD'),
                         amount.amount_mult(amount_, Decimal('1.021')))

    def test_div(self):
        amount_ = Amount('100', 'CAD')
        self.assertEqual(Amount('20', 'CAD'),
                         amount.amount_div(amount_, Decimal('5')))

    def test_sub(self):
        self.assertEqual(Amount('82.98', 'CAD'),
                         amount.amount_sub(Amount('100', 'CAD'),
                                           Amount('17.02', 'CAD')))
        with self.assertRaises(ValueError):
            amount.amount_sub(Amount('100', 'USD'),
                              Amount('17.02', 'CAD'))
