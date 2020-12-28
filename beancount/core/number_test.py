__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import decimal
import random
import unittest

from beancount.core.number import D, ZERO, round_to, same_sign
from beancount.core import number


class TestDecimalPrecision(unittest.TestCase):

    def test_formatting(self):
        # Verifying if we can auto-format everything using precision. No.
        #
        # "The significance of a new Decimal is determined solely by the number
        # of digits input. Context precision and rounding only come into play
        # during arithmetic operations."
        with decimal.localcontext() as context:
            context.prec = 2
            number = D('0.1122334455')
            self.assertEqual('0.1122334455', str(number))


class TestToDecimal(unittest.TestCase):

    def test_ZERO(self):
        self.assertEqual(ZERO, decimal.Decimal('0'))

    def test_D(self):
        dec = decimal.Decimal('10.345')
        self.assertEqual(dec, D(dec))
        self.assertEqual(dec, D('10.345'))
        self.assertEqual(decimal.Decimal('10034.45'), D('10,034.45'))
        self.assertEqual(decimal.Decimal('-83434309.10'), D('-83,434,309.10'))
        self.assertEqual(decimal.Decimal('-122.34'), D('- 122.34'))
        self.assertEqual(decimal.Decimal(), D(''))
        self.assertEqual(decimal.Decimal(), D(None))

    def test_round_to(self):
        self.assertEqual(D('135.12'), round_to(D('135.12345'), D('0.01')))
        self.assertEqual(D('135.12'), round_to(D('135.12987'), D('0.01')))
        self.assertEqual(D('-135.12'), round_to(D('-135.12345'), D('0.01')))
        self.assertEqual(D('-135.12'), round_to(D('-135.12987'), D('0.01')))

        self.assertEqual(D('130'), round_to(D('135.12345'), D('10')))
        self.assertEqual(D('130'), round_to(D('135.12987'), D('10')))
        self.assertEqual(D('-130'), round_to(D('-135.12345'), D('10')))
        self.assertEqual(D('-130'), round_to(D('-135.12987'), D('10')))

    def test_same_sign(self):
        self.assertTrue(same_sign(D('135.12345'), D('234.20')))
        self.assertFalse(same_sign(D('135.12345'), D('-234.20')))
        self.assertFalse(same_sign(D('-135.12345'), D('234.20')))
        self.assertTrue(same_sign(D('-135.12345'), D('-234.20')))

        self.assertTrue(same_sign(D('135.12345'), ZERO))
        self.assertTrue(same_sign(ZERO, D('135.12345')))
        self.assertTrue(same_sign(ZERO, ZERO))

        self.assertFalse(same_sign(D('-135.12345'), ZERO))
        self.assertFalse(same_sign(ZERO, D('-135.12345')))


class TestInferQuantization(unittest.TestCase):

    def setUp(self):
        rn = [random.random() * 10000 for _ in range(100)]
        self.ir = list(map(int, rn))
        self.fr = [(x - ix - 0.5) for (x, ix) in zip(rn, self.ir)]

    def test_infer_quantization_none(self):
        qua = number.infer_quantization_from_numbers([
            0.8462362724718449, 0.7053497034927213, 0.18865925698056718,
            0.4231422803809822, 0.1454769973533604, 0.11586558849935513,
            0.04047872493132432, 0.09511048123106225, 0.4932086961083296,
            0.12377443905156471])
        self.assertIsNone(qua)

    def test_infer_quantization_one(self):
        xn = [ix + fx/1000 for ix, fx in zip(self.ir, self.fr)]
        qua = number.infer_quantization_from_numbers(xn)
        self.assertEqual(D("1"), qua)

    def test_infer_quantization_normal(self):
        for exp, expected_qua in [(2, D("0.01")),
                                  (3, D("0.001")),
                                  (5, D("0.00001"))]:
            xn = [(ix + fx/1000)/(10**exp) for ix, fx in zip(self.ir, self.fr)]
            qua = number.infer_quantization_from_numbers(xn)
            self.assertEqual(expected_qua, qua)


if __name__ == '__main__':
    unittest.main()
