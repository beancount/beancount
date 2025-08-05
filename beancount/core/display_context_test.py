__copyright__ = "Copyright (C) 2014-2017, 2019-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
from decimal import Decimal

from beancount.core import display_context
from beancount.core.display_context import Align
from beancount.core.display_context import Precision
from beancount.core.display_context import _CurrencyContext
from beancount.core.display_context import _FixedPrecisionContext


def decimalize(number_list):
    decimalized_list = []
    for element in number_list:
        if isinstance(element, str):
            decimalized_list.append(Decimal(element))
        else:
            decimalized_list.append((Decimal(element[0]),) + element[1:])
    return decimalized_list


class DisplayContextBaseTest(unittest.TestCase):
    alignment: Align

    def assertFormatNumbers(self, number_strings, expected_fmt_numbers, **build_args):
        dcontext = display_context.DisplayContext()
        numbers = decimalize(number_strings)

        if not build_args.pop("noinit", None):
            for number in numbers:
                if isinstance(number, Decimal):
                    dcontext.update(number)
                else:
                    number, currency = number
                    dcontext.update(number, currency)
        dformat = dcontext.build(alignment=self.alignment, **build_args)

        fmt_numbers = []
        for number in numbers:
            if isinstance(number, Decimal):
                fmt_numbers.append(dformat.format(number))
            else:
                number, currency = number
                fmt_numbers.append(dformat.format(number, currency))

        self.assertEqual(expected_fmt_numbers, fmt_numbers)


class TestDisplayContext(DisplayContextBaseTest):
    def test_dump(self):
        dcontext = display_context.DisplayContext()
        dcontext.update(Decimal("1.234"))
        dcontext.update(Decimal("1.23"), "USD")
        dcontext.update(Decimal("7"), "HOOL")
        self.assertRegex(str(dcontext), "sign=")

    def test_set_fixed_precision(self):
        dcontext = display_context.DisplayContext()

        # Set fixed precision for USD
        dcontext.set_fixed_precision("USD", 2)

        # Update with numbers for USD and other currencies.
        dcontext.update(Decimal("123.456"), "USD")
        dcontext.update(Decimal("-987.654321"), "USD")
        dcontext.update(Decimal("1000"), "USD")

        dcontext.update(Decimal("1.2345"), "EUR")
        dcontext.update(Decimal("764"), "EUR")
        dcontext.update(Decimal("0.00000125"), "EUR")

        dcontext.update(Decimal("5000.0"), "CAD")

        # Test USD formatting with fixed precision (MOST_COMMON).
        dformat_usd_common = dcontext.build(
            alignment=Align.NATURAL, precision=Precision.MOST_COMMON
        )
        self.assertEqual(dformat_usd_common.format(Decimal("123.456"), "USD"), "123.46")
        self.assertEqual(
            dformat_usd_common.format(Decimal("-987.654321"), "USD"), "-987.65"
        )
        self.assertEqual(dformat_usd_common.format(Decimal("1000"), "USD"), "1000.00")
        self.assertEqual(dformat_usd_common.format(Decimal("0.001"), "USD"), "0.00")

        # Test USD formatting with fixed precision (MAXIMUM) - should be the
        # same as MOST_COMMON
        dformat_usd_max = dcontext.build(
            alignment=Align.NATURAL, precision=Precision.MAXIMUM
        )
        self.assertEqual(dformat_usd_max.format(Decimal("123.456"), "USD"), "123.46")
        self.assertEqual(dformat_usd_max.format(Decimal("-987.654321"), "USD"), "-987.65")
        self.assertEqual(dformat_usd_max.format(Decimal("1000"), "USD"), "1000.00")
        self.assertEqual(dformat_usd_max.format(Decimal("0.001"), "USD"), "0.00")

        # Test EUR formatting (learned precision)
        dformat_eur = dcontext.build(alignment=Align.NATURAL)
        self.assertEqual(dformat_eur.format(Decimal("1.2345"), "EUR"), "1.23450000")
        self.assertEqual(dformat_eur.format(Decimal("0.00000125"), "EUR"), "0.00000125")

        # Test CAD formatting (learned precision)
        dformat_cad = dcontext.build(alignment=Align.NATURAL)
        self.assertEqual(dformat_cad.format(Decimal("5000.0"), "CAD"), "5000.0")

        # Test quantize with fixed precision
        self.assertEqual(dcontext.quantize(Decimal("123.456789"), "USD"), Decimal("123.46"))
        self.assertEqual(dcontext.quantize(Decimal("99.999"), "USD"), Decimal("100.00"))
        self.assertEqual(dcontext.quantize(Decimal("0.001"), "USD"), Decimal("0.00"))

        # Test quantize with learned precision for EUR (most common is 0)
        self.assertEqual(dcontext.quantize(Decimal("1.2345"), "EUR"), Decimal("1.2345000"))

        # Test that setting fixed precision for a currency replaces its context.
        dcontext_replace = display_context.DisplayContext()
        dcontext_replace.update(
            Decimal("1.23456"), "XYZ"
        )  # This would normally lead to 5 or 6 decimal places
        self.assertIsInstance(dcontext_replace.ccontexts["XYZ"], _CurrencyContext)
        dcontext_replace.set_fixed_precision("XYZ", 3)
        self.assertIsInstance(dcontext_replace.ccontexts["XYZ"], _FixedPrecisionContext)
        dformat_xyz = dcontext_replace.build(alignment=Align.NATURAL)
        self.assertEqual(dformat_xyz.format(Decimal("1.23456"), "XYZ"), "1.235")

    def test_set_fixed_precision_can_apply_to_update_from(self):
        dcontext = display_context.DisplayContext()
        dcontext2 = display_context.DisplayContext()

        dcontext2.set_fixed_precision("A", 1)
        dcontext.update_from(dcontext2)
        self.assertIsInstance(dcontext.ccontexts["A"], _FixedPrecisionContext)


class TestDisplayContextNatural(DisplayContextBaseTest):
    alignment = Align.NATURAL

    def test_natural_uninitialized(self):
        self.assertFormatNumbers(
            ["1.2345", "764", "-7409.01", "0.00000125"],
            ["1.2345", "764", "-7409.01", "0.00000125"],
            noinit=True,
        )

    def test_natural_no_clear_mode(self):
        self.assertFormatNumbers(
            ["1.2345", "764", "-7409.01", "0.00000125"],
            ["1.23450000", "764.00000000", "-7409.01000000", "0.00000125"],
        )

    def test_natural_clear_mode(self):
        self.assertFormatNumbers(
            ["1.2345", "1.23", "234.26", "38.019"], ["1.23", "1.23", "234.26", "38.02"]
        )

    def test_natural_maximum(self):
        self.assertFormatNumbers(
            ["1.2345", "1.23", "234.26", "38.019"],
            ["1.2345", "1.2300", "234.2600", "38.0190"],
            precision=Precision.MAXIMUM,
        )

    def test_natural_commas(self):
        self.assertFormatNumbers(
            ["0.2345", "1.23", "12234.26"], ["0.23", "1.23", "12,234.26"], commas=True
        )

    def test_natural_reserved(self):
        self.assertFormatNumbers(
            ["1.2345", "1.23", "234.26", "38.019"],
            ["1.23", "1.23", "234.26", "38.02"],
            reserved=10,
        )


class TestDisplayContextRight(DisplayContextBaseTest):
    alignment = Align.RIGHT

    def test_right_uninitialized(self):
        self.assertFormatNumbers(
            ["1.2345", "764", "-7409.01", "0.00000125"],
            ["1.2345", "764", "-7409.01", "0.00000125"],
            noinit=True,
        )

    def test_right_sign(self):
        self.assertFormatNumbers(["7409.01", "0.1"], [" 7409.01", "    0.10"])

        self.assertFormatNumbers(["-7409.01", "0.1"], ["-7409.01", "    0.10"])

    def test_right_integer(self):
        self.assertFormatNumbers(
            ["1", "20", "300", "4000", "50000"],
            ["     1", "    20", "   300", "  4000", " 50000"],
        )

        self.assertFormatNumbers(
            ["1", "20", "300", "4000", "50000", "0.001"],
            [
                "     1.000",
                "    20.000",
                "   300.000",
                "  4000.000",
                " 50000.000",
                "     0.001",
            ],
            precision=Precision.MAXIMUM,
        )

    def test_right_integer_commas(self):
        self.assertFormatNumbers(
            ["1", "20", "300", "4000", "50000"],
            ["      1", "     20", "    300", "  4,000", " 50,000"],
            commas=True,
        )

    def test_right_fractional(self):
        self.assertFormatNumbers(
            ["4000", "0.01", "0.02", "0.0002"],
            [" 4000.00", "    0.01", "    0.02", "    0.00"],
        )

    def test_right_fractional_commas(self):
        self.assertFormatNumbers(
            ["4000", "0.01", "0.02", "0.0002"],
            [" 4,000.00", "     0.01", "     0.02", "     0.00"],
            commas=True,
        )


class TestDisplayContextDot(DisplayContextBaseTest):
    alignment = Align.DOT

    def test_dot_uninitialized(self):
        self.assertFormatNumbers(
            ["1.2345", "764", "-7409.01", "0.00000125"],
            [" 1.23450000", " 764.00000000", "-7409.01000000", " 0.00000125"],
            noinit=True,
        )

    def test_dot_basic(self):
        self.assertFormatNumbers(
            ["1.2345", "764", "-7409.01", "0.00", "0.00000125"],
            ["    1.23", "  764.00", "-7409.01", "    0.00", "    0.00"],
        )

    def test_dot_basic_multi(self):
        self.assertFormatNumbers(
            [
                ("1.2345", "USD"),
                ("764", "CAD"),
                ("-7409.01", "EUR"),
                ("0.00", "XAU"),
                ("0.00000125", "RBFF"),
            ],
            [
                "    1.2345    ",
                "  764         ",
                "-7409.01      ",
                "    0.00      ",
                "    0.00000125",
            ],
        )

    def test_dot_sign(self):
        self.assertFormatNumbers([("7409.01", "USD"), "0.1"], [" 7409.01", "    0.1 "])
        self.assertFormatNumbers([("-7409.01", "USD"), "0.1"], ["-7409.01", "    0.1 "])

    def test_dot_integer(self):
        self.assertFormatNumbers(
            ["1", "20", "300", "4000", "50000"],
            ["     1", "    20", "   300", "  4000", " 50000"],
        )

        self.assertFormatNumbers(
            ["1", "20", "300", "4000", "50000", "0.001", ("0.1", "USD")],
            [
                "     1.000",
                "    20.000",
                "   300.000",
                "  4000.000",
                " 50000.000",
                "     0.001",
                "     0.1  ",
            ],
            precision=Precision.MAXIMUM,
        )

    def test_dot_integer_commas(self):
        self.assertFormatNumbers(
            ["1", "20", "300", "4000", "50000"],
            ["      1", "     20", "    300", "  4,000", " 50,000"],
            commas=True,
        )

    def test_dot_fractional(self):
        self.assertFormatNumbers(
            [("4000", "USD"), "0.01", "0.02", "0.0002"],
            [" 4000   ", "    0.01", "    0.02", "    0.00"],
        )

    def test_dot_fractional_commas(self):
        self.assertFormatNumbers(
            [("4000", "USD"), "0.01", "0.02", "0.0002"],
            [" 4,000   ", "     0.01", "     0.02", "     0.00"],
            commas=True,
        )


class TestDisplayContextQuantize(unittest.TestCase):
    def test_quantize_basic(self):
        dcontext = display_context.DisplayContext()
        dcontext.update(Decimal("1.23"), "USD")
        self.assertEqual(Decimal("3.23"), dcontext.quantize(Decimal("3.23253343"), "USD"))

        dcontext.update(Decimal("1.2301"), "USD")
        dcontext.update(Decimal("1.2302"), "USD")
        self.assertEqual(Decimal("3.2325"), dcontext.quantize(Decimal("3.23253343"), "USD"))


class TestFixedContext(unittest.TestCase):
    def test_init(self):
        # Test with a specific fractional_digits
        fcontext = _FixedPrecisionContext(2)
        self.assertTrue(fcontext.has_sign)  # Because unconditional
        self.assertEqual(fcontext.integer_max, 1)
        self.assertEqual(fcontext.fractional_digits, 2)

        fcontext = _FixedPrecisionContext(5)
        self.assertEqual(fcontext.fractional_digits, 5)

    def test_update(self):
        fcontext = _FixedPrecisionContext(2)

        # Test with positive number, integer_max update
        fcontext.update(Decimal("123.45"))
        self.assertTrue(fcontext.has_sign)  # Because unconditional
        self.assertEqual(fcontext.integer_max, 3)  # 123 has 3 integer digits
        self.assertEqual(fcontext.fractional_digits, 2)  # Should remain fixed

        # Test with negative number, has_sign update
        fcontext.update(Decimal("-9876.54"))
        self.assertTrue(fcontext.has_sign)
        self.assertEqual(fcontext.integer_max, 4)  # max(3, 4) = 4
        self.assertEqual(fcontext.fractional_digits, 2)

        # Test with larger integer part
        fcontext.update(Decimal("1234567.89"))
        self.assertTrue(fcontext.has_sign)  # Still true
        self.assertEqual(fcontext.integer_max, 7)  # max(4, 7) = 7
        self.assertEqual(fcontext.fractional_digits, 2)

        # Test with None
        fcontext.update(None)
        self.assertTrue(fcontext.has_sign)
        self.assertEqual(fcontext.integer_max, 7)
        self.assertEqual(fcontext.fractional_digits, 2)

    def test_update_from(self):
        fcontext = _FixedPrecisionContext(3)  # Fixed context with 3 fractional digits
        fcontext.update(Decimal("100.00"))  # Initial state: integer_max=3, has_sign=False

        cc_other = _CurrencyContext()
        cc_other.update(
            Decimal("-1234.5678")
        )  # Other state: integer_max=4, has_sign=True, fractional_dist updated

        fcontext.update_from(cc_other)

        self.assertTrue(fcontext.has_sign)  # has_sign should be True (from other)
        self.assertEqual(fcontext.integer_max, 4)  # integer_max should be max(3, 4) = 4
        self.assertEqual(
            fcontext.fractional_digits, 3
        )  # fractional_digits should remain fixed

        # Test with another _CurrencyContext that doesn't change anything
        cc_another = _CurrencyContext()
        cc_another.update(Decimal("50.0"))  # integer_max=2, has_sign=False

        fcontext.update_from(cc_another)
        self.assertTrue(fcontext.has_sign)
        self.assertEqual(fcontext.integer_max, 4)
        self.assertEqual(fcontext.fractional_digits, 3)

    def test_get_fractional(self):
        fcontext = _FixedPrecisionContext(4)

        # Should always return the fixed fractional_digits
        self.assertEqual(fcontext.get_fractional(Precision.MOST_COMMON), 4)
        self.assertEqual(fcontext.get_fractional(Precision.MAXIMUM), 4)

        # Update should not change fractional_digits
        fcontext.update(Decimal("123.456"))
        self.assertEqual(fcontext.get_fractional(Precision.MOST_COMMON), 4)
        self.assertEqual(fcontext.get_fractional(Precision.MAXIMUM), 4)

        # update_from should not change fractional_digits
        cc_other = _CurrencyContext()
        cc_other.update(Decimal("1.234567"))
        fcontext.update_from(cc_other)
        self.assertEqual(fcontext.get_fractional(Precision.MOST_COMMON), 4)
        self.assertEqual(fcontext.get_fractional(Precision.MAXIMUM), 4)

    def test_str(self):
        fcontext = _FixedPrecisionContext(2)
        self.assertRegex(
            str(fcontext),
            r"sign=1\s+integer_max=1\s+fractional_common=2\s+fractional_max=2\s+\"-0.00\"\s+\"-0.00\"",
        )

        fcontext.update(Decimal("-12345.67"))
        self.assertRegex(
            str(fcontext),
            r"sign=1\s+integer_max=5\s+fractional_common=2\s+fractional_max=2\s+\"-00000.00\"\s+\"-00000.00\"",
        )

        # Test with a different fixed precision
        fcontext_high_prec = _FixedPrecisionContext(8)
        fcontext_high_prec.update(Decimal("0.00000001"))
        self.assertRegex(
            str(fcontext_high_prec),
            r"sign=1\s+integer_max=1\s+fractional_common=8\s+fractional_max=8\s+\"-0.00000000\"\s+\"-0.00000000\"",
        )


if __name__ == "__main__":
    unittest.main()
