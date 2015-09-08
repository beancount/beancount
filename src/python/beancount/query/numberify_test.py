__author__ = 'Martin Blais <blais@furius.ca>'

import datetime
import pprint
import unittest
import sys

from beancount.core.number import Decimal
from beancount.core.number import D
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import display_context
from beancount.query import numberify
from beancount.query import query_render
A = amount.from_string


def pr_table(types, rows):
    pprint.pprint(rows)
    query_render.render_text(types, rows, display_context.DEFAULT_DISPLAY_CONTEXT, sys.stdout)


class TestNumerifySimple(unittest.TestCase):

    input_amounts = ["24.17 CAD",
                     "-77.02 CAD",
                     "11.39 CAD",
                     "800.00 USD",
                     "41.17 CAD",
                     "950.00 USD",
                     "110 JPY",
                     "-947.00 USD"]

    expected_types = [('pos (CAD)', Decimal),
                      ('pos (USD)', Decimal),
                      ('pos (JPY)', Decimal)]

    expected_rows = [[D('24.17'), None, None],
                     [D('-77.02'), None, None],
                     [D('11.39'), None, None],
                     [None, D('800.00'), None],
                     [D('41.17'), None, None],
                     [None, D('950.00'), None],
                     [None, None, D('110')],
                     [None, D('-947.00'), None]]

    def test_amount(self):
        itypes = [('pos', amount.Amount)]
        irows = [(A(string),) for string in self.input_amounts]
        atypes, arows = numberify.numberify_results(itypes, irows)
        self.assertEqual(self.expected_types, atypes)
        self.assertEqual(self.expected_rows, arows)

    def test_position(self):
        itypes = [('pos', position.Position)]
        irows = [(position.from_string(string),) for string in self.input_amounts]
        atypes, arows = numberify.numberify_results(itypes, irows)
        self.assertEqual(self.expected_types, atypes)
        self.assertEqual(self.expected_rows, arows)

    def test_inventory(self):
        itypes = [('pos', inventory.Inventory)]
        irows = [(inventory.from_string(string),) for string in self.input_amounts]
        atypes, arows = numberify.numberify_results(itypes, irows)
        self.assertEqual(self.expected_types, atypes)
        self.assertEqual(self.expected_rows, arows)


class TestNumerifyIdentity(unittest.TestCase):

    def test_identity(self):
        itypes = [('date', datetime.date), ('name', str), ('count', int)]
        irows = [[datetime.date(2015, 9, 8), 'Testing', 3]]
        atypes, arows = numberify.numberify_results(itypes, irows)
        self.assertEqual(itypes, atypes)
        self.assertEqual(irows, arows)
