"""Test for cross-language Decimal serialization."""

from decimal import Decimal
import unittest

from experiments.v3.decimal import decimal_func as mod


class DecimalFuncTests(unittest.TestCase):
    def test_accept_decimal(self):
        mod.accept_decimal(Decimal("0.01"))
        mod.accept_decimal_cast_ptr(Decimal("0.02"))
        mod.accept_decimal_cast_ref(Decimal("0.03"))
        print(mod.times_two(Decimal("0.08")))


if __name__ == "__main__":
    unittest.main()
