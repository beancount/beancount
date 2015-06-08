__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from . import number
from .number import D
from .number import Decimal
from .number import ZERO


class TestDecimalPrecision(unittest.TestCase):

    def test_formatting(self):
        # Verifying if we can auto-format everything using precision. No.
        #
        # "The significance of a new Decimal is determined solely by the number
        # of digits input. Context precision and rounding only come into play
        # during arithmetic operations."
        with number.decimal.localcontext() as context:
            context.prec = 2
            number = D('0.1122334455')
            self.assertEqual('0.1122334455', str(number))


class TestToDecimal(unittest.TestCase):

    def test_ZERO(self):
        self.assertEqual(ZERO, Decimal('0'))

    def test_D(self):
        dec = Decimal('10.345')
        self.assertEqual(dec, D(dec))
        self.assertEqual(dec, D('10.345'))
        self.assertEqual(Decimal('10034.45'), D('10,034.45'))
        self.assertEqual(Decimal('-83434309.10'), D('-83,434,309.10'))
        self.assertEqual(Decimal(), D(''))
        self.assertEqual(Decimal(), D(None))

    def test_round_to(self):
        self.assertEqual(D('135.12'), number.round_to(D('135.12345'), D('0.01')))
        self.assertEqual(D('135.12'), number.round_to(D('135.12987'), D('0.01')))
        self.assertEqual(D('-135.12'), number.round_to(D('-135.12345'), D('0.01')))
        self.assertEqual(D('-135.12'), number.round_to(D('-135.12987'), D('0.01')))

        self.assertEqual(D('130'), number.round_to(D('135.12345'), D('10')))
        self.assertEqual(D('130'), number.round_to(D('135.12987'), D('10')))
        self.assertEqual(D('-130'), number.round_to(D('-135.12345'), D('10')))
        self.assertEqual(D('-130'), number.round_to(D('-135.12987'), D('10')))
