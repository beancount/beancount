__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core import display_context
from beancount.core.number import Decimal
from beancount.core.display_context import Precision
from beancount.core.display_context import Align


def decimalize(number_list):
    decimalized_list = []
    for element in number_list:
        if isinstance(element, str):
            decimalized_list.append(Decimal(element))
        else:
            decimalized_list.append((Decimal(element[0]),) + element[1:])
    return decimalized_list


class DisplayContextBaseTest(unittest.TestCase):

    alignment = None

    def assertFormatNumbers(self, number_strings, expected_fmt_numbers, **build_args):
        dcontext = display_context.DisplayContext()
        numbers = decimalize(number_strings)

        if not build_args.pop('noinit', None):
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
        dcontext.update(Decimal('1.234'))
        dcontext.update(Decimal('1.23'), 'USD')
        dcontext.update(Decimal('7'), 'HOOL')
        self.assertRegex(str(dcontext), 'sign=')


class TestDisplayContextNatural(DisplayContextBaseTest):

    alignment = Align.NATURAL

    def test_natural_uninitialized(self):
        self.assertFormatNumbers(['1.2345', '764', '-7409.01', '0.00000125'],
                                 ['1.2345', '764', '-7409.01', '0.00000125'],
                                 noinit=True)

    def test_natural_no_clear_mode(self):
        self.assertFormatNumbers(
            ['1.2345', '764', '-7409.01', '0.00000125'],
            ['1.23450000', '764.00000000', '-7409.01000000', '0.00000125'])

    def test_natural_clear_mode(self):
        self.assertFormatNumbers(['1.2345', '1.23', '234.26', '38.019'],
                                 ['1.23', '1.23', '234.26', '38.02'])

    def test_natural_maximum(self):
        self.assertFormatNumbers(['1.2345', '1.23', '234.26', '38.019'],
                                 ['1.2345', '1.2300', '234.2600', '38.0190'],
                                 precision=Precision.MAXIMUM)

    def test_natural_commas(self):
        self.assertFormatNumbers(['0.2345', '1.23', '12234.26'],
                                 ['0.23', '1.23', '12,234.26'],
                                 commas=True)

    def test_natural_reserved(self):
        self.assertFormatNumbers(['1.2345', '1.23', '234.26', '38.019'],
                                 ['1.23', '1.23', '234.26', '38.02'],
                                 reserved=10)


class TestDisplayContextRight(DisplayContextBaseTest):

    alignment = Align.RIGHT

    def test_right_uninitialized(self):
        self.assertFormatNumbers(
            ['1.2345', '764', '-7409.01', '0.00000125'],
            ['1.2345',
             '764',
             '-7409.01',
             '0.00000125'],
            noinit=True)

    def test_right_sign(self):
        self.assertFormatNumbers(
            ['7409.01', '0.1'],
            ['7409.01',
             '   0.10'])

        self.assertFormatNumbers(
            ['-7409.01', '0.1'],
            ['-7409.01',
             '    0.10'])

    def test_right_integer(self):
        self.assertFormatNumbers(
            ['1', '20', '300', '4000', '50000'],
            ['    1',
             '   20',
             '  300',
             ' 4000',
             '50000'])

        self.assertFormatNumbers(
            ['1', '20', '300', '4000', '50000', '0.001'],
            ['    1.000',
             '   20.000',
             '  300.000',
             ' 4000.000',
             '50000.000',
             '    0.001',], precision=Precision.MAXIMUM)

    def test_right_integer_commas(self):
        self.assertFormatNumbers(
            ['1', '20', '300', '4000', '50000'],
            ['     1',
             '    20',
             '   300',
             ' 4,000',
             '50,000'], commas=True)

    def test_right_fractional(self):
        self.assertFormatNumbers(
            ['4000', '0.01', '0.02', '0.0002'],
            ['4000.00',
             '   0.01',
             '   0.02',
             '   0.00'])

    def test_right_fractional_commas(self):
        self.assertFormatNumbers(
            ['4000', '0.01', '0.02', '0.0002'],
            ['4,000.00',
             '    0.01',
             '    0.02',
             '    0.00'], commas=True)


class TestDisplayContextDot(DisplayContextBaseTest):

    alignment = Align.DOT

    def test_dot_uninitialized(self):
        self.assertFormatNumbers(
            ['1.2345', '764', '-7409.01', '0.00000125'],
            ['1.23450000',
             '764.00000000',
             '-7409.01000000',
             '0.00000125'],
            noinit=True)

    def test_dot_basic(self):
        self.assertFormatNumbers(
            ['1.2345', '764', '-7409.01', '0.00', '0.00000125'],
            ['    1.23', '  764.00', '-7409.01', '    0.00', '    0.00'])

    def test_dot_basic_multi(self):
        self.assertFormatNumbers(
            [('1.2345', 'USD'),
             ('764', 'CAD'),
             ('-7409.01', 'EUR'),
             ('0.00', 'XAU'),
             ('0.00000125', 'RBFF')],
            ['    1.2345    ',
             '  764         ',
             '-7409.01      ',
             '    0.00      ',
             '    0.00000125'])

    def test_dot_sign(self):
        self.assertFormatNumbers(
            [('7409.01', 'USD'), '0.1'],
            ['7409.01',
             '   0.1 '])
        self.assertFormatNumbers(
            [('-7409.01', 'USD'), '0.1'],
            ['-7409.01',
             '    0.1 '])

    def test_dot_integer(self):
        self.assertFormatNumbers(
            ['1', '20', '300', '4000', '50000'],
            ['    1',
             '   20',
             '  300',
             ' 4000',
             '50000'])

        self.assertFormatNumbers(
            ['1', '20', '300', '4000', '50000', '0.001', ('0.1', 'USD')],
            ['    1.000',
             '   20.000',
             '  300.000',
             ' 4000.000',
             '50000.000',
             '    0.001',
             '    0.1  ',], precision=Precision.MAXIMUM)

    def test_dot_integer_commas(self):
        self.assertFormatNumbers(
            ['1', '20', '300', '4000', '50000'],
            ['     1',
             '    20',
             '   300',
             ' 4,000',
             '50,000'], commas=True)

    def test_dot_fractional(self):
        self.assertFormatNumbers(
            [('4000', 'USD'), '0.01', '0.02', '0.0002'],
            ['4000   ',
             '   0.01',
             '   0.02',
             '   0.00'])

    def test_dot_fractional_commas(self):
        self.assertFormatNumbers(
            [('4000', 'USD'), '0.01', '0.02', '0.0002'],
            ['4,000   ',
             '    0.01',
             '    0.02',
             '    0.00'], commas=True)


class TestDisplayContextQuantize(unittest.TestCase):

    def test_quantize_basic(self):
        dcontext = display_context.DisplayContext()
        dcontext.update(Decimal('1.23'), 'USD')
        self.assertEqual(Decimal('3.23'),
                         dcontext.quantize(Decimal('3.23253343'), 'USD'))

        dcontext.update(Decimal('1.2301'), 'USD')
        dcontext.update(Decimal('1.2302'), 'USD')
        self.assertEqual(Decimal('3.2325'),
                         dcontext.quantize(Decimal('3.23253343'), 'USD'))
