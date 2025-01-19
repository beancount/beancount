__copyright__ = "Copyright (C) 2013-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core import amount
from beancount.core import display_context
from beancount.core.amount import Amount
from beancount.core.number import D


class TestAmount(unittest.TestCase):
    def test_constructor(self):
        amount = Amount(D("100,034.02"), "USD")
        self.assertEqual(amount.number, D("100034.02"))

        # Ensure that it is possible to initialize the number to any object.
        # This is used when creating incomplete objects.
        class Dummy:
            pass

        amount = Amount(Dummy, Dummy)
        self.assertIs(amount.number, Dummy)
        self.assertIs(amount.currency, Dummy)

    def test_mutation(self):
        amount1 = Amount(D("100"), "USD")

        # Test how changing existing attributes should fail.
        with self.assertRaises(AttributeError) as ctx:
            amount1.currency = "CAD"
        self.assertRegex("can't set attribute", str(ctx.exception))

        with self.assertRaises(AttributeError) as ctx:
            amount1.number = D("200")
        self.assertRegex("can't set attribute", str(ctx.exception))

        # Try setting a new attribute.
        with self.assertRaises(AttributeError):
            amount1.something = 42

    def test_fromstring(self):
        amount1 = Amount(D("100"), "USD")
        amount2 = Amount.from_string("100 USD")
        self.assertEqual(amount1, amount2)

        amount3 = Amount(D("0.00000001"), "BTC")
        amount4 = Amount.from_string("0.00000001 BTC")
        self.assertEqual(amount3, amount4)

        Amount.from_string("  100.00 USD  ")

        with self.assertRaises(ValueError):
            Amount.from_string("100")

        with self.assertRaises(ValueError):
            Amount.from_string("USD")

        # Starting in v3 we will accept single character stock names.
        Amount.from_string("100.00 U")

    def test_tostring(self):
        amount1 = Amount(D("100034.023"), "USD")
        self.assertEqual("100034.023 USD", str(amount1))

        amount2 = Amount(D("0.00000001"), "BTC")
        self.assertEqual("0.00000001 BTC", str(amount2))

        dcontext = display_context.DisplayContext()
        dformat = dcontext.build(commas=True)
        self.assertEqual("100,034.023 USD", amount1.to_string(dformat))

    def test_comparisons(self):
        amount1 = Amount(D("100"), "USD")
        amount2 = Amount(D("100"), "USD")
        self.assertEqual(amount1, amount2)

        amount3 = Amount(D("101"), "USD")
        self.assertNotEqual(amount1, amount3)

    def test_hash(self):
        amount = Amount(D("100,034.027456"), "USD")
        self.assertTrue({amount: True})
        self.assertTrue({amount})

        amount2 = Amount(D("100,034.027456"), "CAD")
        self.assertEqual(2, len({amount: True, amount2: False}))

    def test_sort__explicit(self):
        # Check that we can sort currency-first.
        amounts = [
            Amount(D("1"), "USD"),
            Amount(D("201"), "EUR"),
            Amount(D("3"), "USD"),
            Amount(D("100"), "CAD"),
            Amount(D("2"), "USD"),
            Amount(D("200"), "EUR"),
        ]
        amounts = sorted(amounts, key=amount.sortkey)
        self.assertEqual(
            [
                Amount(D("100"), "CAD"),
                Amount(D("200"), "EUR"),
                Amount(D("201"), "EUR"),
                Amount(D("1"), "USD"),
                Amount(D("2"), "USD"),
                Amount(D("3"), "USD"),
            ],
            amounts,
        )

    def test_sort__natural(self):
        # Check that we can sort currency-first.
        amounts = [
            Amount(D("1"), "USD"),
            Amount(D("201"), "EUR"),
            Amount(D("3"), "USD"),
            Amount(D("100"), "CAD"),
            Amount(D("2"), "USD"),
            Amount(D("200"), "EUR"),
        ]
        amounts = sorted(amounts)
        self.assertEqual(
            [
                Amount(D("100"), "CAD"),
                Amount(D("200"), "EUR"),
                Amount(D("201"), "EUR"),
                Amount(D("1"), "USD"),
                Amount(D("2"), "USD"),
                Amount(D("3"), "USD"),
            ],
            amounts,
        )

    def test_neg(self):
        amount_ = Amount(D("100"), "CAD")
        self.assertEqual(Amount(D("-100"), "CAD"), -amount_)

        amount_ = Amount(D("-100"), "CAD")
        self.assertEqual(Amount(D("100"), "CAD"), -amount_)

        amount_ = Amount(D("0"), "CAD")
        self.assertEqual(Amount(D("0"), "CAD"), -amount_)

    def test_mult(self):
        amount_ = Amount(D("100"), "CAD")
        self.assertEqual(Amount(D("102.1"), "CAD"), amount.mul(amount_, D("1.021")))

    def test_div(self):
        amount_ = Amount(D("100"), "CAD")
        self.assertEqual(Amount(D("20"), "CAD"), amount.div(amount_, D("5")))

    def test_add(self):
        self.assertEqual(
            Amount(D("117.02"), "CAD"),
            amount.add(Amount(D("100"), "CAD"), Amount(D("17.02"), "CAD")),
        )
        with self.assertRaises(ValueError):
            amount.add(Amount(D("100"), "USD"), Amount(D("17.02"), "CAD"))

    def test_sub(self):
        self.assertEqual(
            Amount(D("82.98"), "CAD"),
            amount.sub(Amount(D("100"), "CAD"), Amount(D("17.02"), "CAD")),
        )
        with self.assertRaises(ValueError):
            amount.sub(Amount(D("100"), "USD"), Amount(D("17.02"), "CAD"))

    def test_abs(self):
        self.assertEqual(Amount(D("82.98"), "CAD"), amount.abs(Amount(D("82.98"), "CAD")))
        self.assertEqual(Amount(D("0"), "CAD"), amount.abs(Amount(D("0"), "CAD")))
        self.assertEqual(Amount(D("82.98"), "CAD"), amount.abs(Amount(D("-82.98"), "CAD")))


if __name__ == "__main__":
    unittest.main()
