__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import io
from decimal import Decimal

from .display_context import DisplayContext
from .display_context import DisplayContextBuilder


class TestDisplayContext(unittest.TestCase):

    number = Decimal('-1764.9876543')

    def test_precision_empty_default(self):
        dcontext = DisplayContext()
        self.assertEqual('-1764.9876543', dcontext.format(self.number))
        self.assertEqual('-1764.9876543', dcontext(self.number))

    def test_precision_empty_default_integers(self):
        dcontext = DisplayContext()
        self.assertEqual('4343', dcontext(Decimal('4343')))
        self.assertEqual('-4343', dcontext(Decimal('-4343')))

    def test_precision_commas(self):
        dcontext = DisplayContext()
        dcontext.set_commas(True)
        self.assertEqual('-1,764.9876543', dcontext.format(self.number))
        dcontext.set_commas(False)
        self.assertEqual('-1764.9876543', dcontext.format(self.number))

    def test_precision_default(self):
        dcontext = DisplayContext()
        dcontext.set_precision(4)
        self.assertEqual('-1764.9877', dcontext.format(self.number))
        dcontext.set_precision(3)
        self.assertEqual('-1764.988', dcontext.format(self.number))

    def test_precision_currency(self):
        dcontext = DisplayContext()
        dcontext.set_precision(5, 'RBF1010')
        self.assertEqual('-1764.9876543', dcontext.format(self.number))
        self.assertEqual('-1764.98765', dcontext.format(self.number, 'RBF1010'))
        dcontext.set_precision(1)
        dcontext.set_precision(3, 'RBF1010')
        self.assertEqual('-1765.0', dcontext.format(self.number))
        self.assertEqual('-1764.988', dcontext.format(self.number, 'RBF1010'))

    def test_precision_integer(self):
        dcontext = DisplayContext()
        dcontext.set_precision(0)
        self.assertEqual('-1765', dcontext.format(self.number))

    def test_precision_max(self):
        dcontext = DisplayContext()
        dcontext.set_precision(2)
        dcontext.set_precision_max(4)
        self.assertEqual('-1764.99', dcontext.format(self.number))
        self.assertEqual('-1764.9877', dcontext.format_max(self.number))

    def test_precision_dump(self):
        dcontext = DisplayContext()
        dcontext.set_precision(2)
        dcontext.set_precision(2, 'CAD')
        dcontext.set_precision_max(8, 'CAD')
        dcontext.set_precision(4, 'RBF1010')
        oss = io.StringIO()
        dcontext.dump(oss)
        self.assertTrue(oss.getvalue())


class TestDisplayContextBuilder(unittest.TestCase):

    def test_build_empty(self):
        builder = DisplayContextBuilder()
        dcontext = builder.build()
        self.assertEqual('1.234567', dcontext.format(Decimal('1.234567'), 'CAD'))

    def test_build(self):
        builder = DisplayContextBuilder()
        builder.update(Decimal('123.45'), 'USD')
        builder.update(Decimal('123.46'), 'USD')
        builder.update(Decimal('123.47'), 'USD')
        builder.update(Decimal('123.4500'), 'USD')
        builder.update(Decimal('123.4500'))
        builder.update(Decimal('123.450'), 'RBF1005')
        dcontext = builder.build()
        self.assertEqual('1.23', dcontext.format(Decimal('1.234567'), 'USD'))
        self.assertEqual('1.2346', dcontext.format(Decimal('1.234567'), 'CAD'))
        self.assertEqual('1.235', dcontext.format(Decimal('1.234567'), 'RBF1005'))

    def test_integer_width(self):
        builder = DisplayContextBuilder()
        builder.update(Decimal('1.45'), 'USD')
        builder.update(Decimal('12.46'), 'USD')
        builder.update(Decimal('1234.47'), 'USD')
        builder.update(Decimal('123.4500'), 'USD')
        builder.update(Decimal('0.4500'), 'USD')
        builder.update(Decimal('0.008'), 'USD')
        dcontext = builder.build()
        ## FIXME: Complete this.
