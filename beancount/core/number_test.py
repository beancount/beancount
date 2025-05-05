__copyright__ = "Copyright (C) 2015-2017, 2019-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import decimal
import random
import unittest
from decimal import Decimal

from beancount.core import number
from beancount.core.number import ZERO
from beancount.core.number import D
from beancount.core.number import round_to
from beancount.core.number import same_sign


class TestDecimalPrecision(unittest.TestCase):
    def test_formatting(self):
        # Verifying if we can auto-format everything using precision. No.
        #
        # "The significance of a new Decimal is determined solely by the number
        # of digits input. Context precision and rounding only come into play
        # during arithmetic operations."
        with decimal.localcontext() as context:
            context.prec = 2
            number = D("0.1122334455")
            self.assertEqual("0.1122334455", str(number))


class TestToDecimal(unittest.TestCase):
    def test_ZERO(self):
        self.assertEqual(ZERO, decimal.Decimal("0"))

    def test_D(self):
        dec = decimal.Decimal("10.345")
        self.assertEqual(dec, D(dec))
        self.assertEqual(dec, D("10.345"))
        self.assertEqual(decimal.Decimal("10034.45"), D("10,034.45"))
        self.assertEqual(decimal.Decimal("-83434309.10"), D("-83,434,309.10"))
        self.assertEqual(decimal.Decimal("-122.34"), D("- 122.34"))
        self.assertEqual(decimal.Decimal(), D(""))
        self.assertEqual(decimal.Decimal(), D(None))

    def test_round_to(self):
        self.assertEqual(D("135.12"), round_to(D("135.12345"), D("0.01")))
        self.assertEqual(D("135.12"), round_to(D("135.12987"), D("0.01")))
        self.assertEqual(D("-135.12"), round_to(D("-135.12345"), D("0.01")))
        self.assertEqual(D("-135.12"), round_to(D("-135.12987"), D("0.01")))

        self.assertEqual(D("130"), round_to(D("135.12345"), D("10")))
        self.assertEqual(D("130"), round_to(D("135.12987"), D("10")))
        self.assertEqual(D("-130"), round_to(D("-135.12345"), D("10")))
        self.assertEqual(D("-130"), round_to(D("-135.12987"), D("10")))

    def test_same_sign(self):
        self.assertTrue(same_sign(D("135.12345"), D("234.20")))
        self.assertFalse(same_sign(D("135.12345"), D("-234.20")))
        self.assertFalse(same_sign(D("-135.12345"), D("234.20")))
        self.assertTrue(same_sign(D("-135.12345"), D("-234.20")))

        self.assertTrue(same_sign(D("135.12345"), ZERO))
        self.assertTrue(same_sign(ZERO, D("135.12345")))
        self.assertTrue(same_sign(ZERO, ZERO))

        self.assertFalse(same_sign(D("-135.12345"), ZERO))
        self.assertFalse(same_sign(ZERO, D("-135.12345")))


class TestInferQuantization(unittest.TestCase):
    def setUp(self):
        rn = [random.uniform(1, 10000) for _ in range(100)]
        self.ir = list(map(int, rn))
        self.fr = [(x - ix - 0.5) for (x, ix) in zip(rn, self.ir)]

    def test_infer_quantization_none(self):
        with decimal.localcontext() as ctx:
            ctx.prec = 20
            # Note: All the instances are actually floats, so larger prec.
            numbers = list(
                map(
                    Decimal,
                    [
                        0.8462362724718449,
                        0.7053497034927213,
                        0.18865925698056718,
                        0.4231422803809822,
                        0.1454769973533604,
                        0.11586558849935513,
                        0.04047872493132432,
                        0.09511048123106225,
                        0.4932086961083296,
                        0.12377443905156471,
                    ],
                )
            )
            exp = number.infer_quantum_from_list(numbers)
            self.assertEqual(-21, exp)

    def test_infer_quantization_one(self):
        xn = [Decimal(ix + fx / 1000) for ix, fx in zip(self.ir, self.fr)]
        qua = number.infer_quantum_from_list(xn)
        self.assertEqual(0, qua)

    def test_infer_quantization_normal(self):
        for exp, expected_qua in [(2, D("0.01")), (3, D("0.001")), (5, D("0.00001"))]:
            xn = [Decimal((ix + fx / 1000) / (10**exp)) for ix, fx in zip(self.ir, self.fr)]
            exp = number.infer_quantum_from_list(xn)
            self.assertEqual(expected_qua, number.TEN**exp)

    def test_infer_quantization_under(self):
        prices = list(
            map(
                Decimal,
                [
                    "0.033255736614566012637179913540",
                    "0.033232527998404838656076567740",
                    "0.033316674995835415625520573050",
                    "0.033940874995757390625530326170",
                    "0.034093621083495278033479935900",
                    "0.033997416196369075950227782690",
                    "0.034020548411240389195073824590",
                    "0.034090134315129201609054339670",
                    "0.034700534388229578735512526890",
                    "0.034191541012753444797757034910",
                    "0.034984606773019871256647075290",
                ],
            )
        )
        exp = number.infer_quantum_from_list(prices)
        self.assertEqual(Decimal("1e-29"), number.TEN**exp)

    def test_infer_quantization_under3(self):
        prices = [
            Decimal("0.7728273890026662544920591986"),
            Decimal("0.8199409642505739586749754018"),
            Decimal("0.7812805187702644634556037345"),
            Decimal("0.776343267939352063908577816800"),
            Decimal("0.782074844562624603847313364900"),
        ]
        _exp = number.infer_quantum_from_list(prices)

    def test_auto_quantize(self):
        self.assertEqual(
            Decimal("1135.11"), number.auto_quantize(Decimal("1135.109998"), 0.01)
        )
        self.assertEqual(
            Decimal("1135.11"), number.auto_quantize(Decimal("1135.109998"), 0.02)
        )
        self.assertEqual(
            Decimal("1135.1024"), number.auto_quantize(Decimal("1135.102399"), 0.01)
        )
        self.assertEqual(
            Decimal("1135.102399"), number.auto_quantize(Decimal("1135.102399"), 0.001)
        )
        self.assertEqual(
            Decimal("-1135.11"), number.auto_quantize(Decimal("-1135.109998"), 0.01)
        )


if __name__ == "__main__":
    unittest.main()
