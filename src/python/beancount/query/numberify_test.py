__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount.core.number import Decimal
from beancount.core.number import D
from beancount.core.amount import A
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import display_context
from beancount.query import numberify


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


class TestNumerifyInventory(unittest.TestCase):

    def test_inventory(self):
        itypes = [('balance', inventory.Inventory)]
        irows = [[inventory.from_string('10 HOOL {23.00 USD}')],
                 [inventory.from_string('2.11 USD, 3.44 CAD')],
                 [inventory.from_string('-2 HOOL {24.00 USD}, 5.66 CAD')]]
        atypes, arows = numberify.numberify_results(itypes, irows)

        self.assertEqual([('balance (HOOL)', Decimal),
                          ('balance (CAD)', Decimal),
                          ('balance (USD)', Decimal)], atypes)

        self.assertEqual([[D('10'), None, None],
                          [None, D('3.44'), D('2.11')],
                          [D('-2'), D('5.66'), None]], arows)


class TestNumerifyPrecision(unittest.TestCase):

    def test_precision(self):
        # Some display context.
        dcontext = display_context.DisplayContext()
        dcontext.update(D('111'), 'JPY')
        dcontext.update(D('1.111'), 'RGAGX')
        dcontext.update(D('1.11'), 'USD')
        dformat = dcontext.build()

        # Input data.
        itypes = [('number', Decimal),
                  ('amount', amount.Amount),
                  ('position', position.Position),
                  ('inventory', inventory.Inventory)]
        irows = [[D(amt.split()[0]),
                  A(amt),
                  position.from_string(amt),
                  inventory.from_string(amt)]
                 for amt in ['123.45678909876 JPY',
                             '1.67321232123 RGAGX',
                             '5.67345434543 USD']]

        # First check with no explicit quantization.
        atypes, arows = numberify.numberify_results(itypes, irows)
        erows = [[D('123.45678909876'),
                  None, None, D('123.45678909876'),
                  None, None, D('123.45678909876'),
                  None, None, D('123.45678909876')],
                 [D('1.67321232123'),
                  None, D('1.67321232123'), None,
                  None, D('1.67321232123'), None,
                  None, D('1.67321232123'), None],
                 [D('5.67345434543'),
                  D('5.67345434543'), None, None,
                  D('5.67345434543'), None, None,
                  D('5.67345434543'), None, None]]
        self.assertEqual(erows, arows)

        # Then compare with quantization.
        atypes, arows = numberify.numberify_results(itypes, irows, dformat)

        erows = [[D('123.45678909876'),
                  None, None, D('123'),
                  None, None, D('123'),
                  None, None, D('123')],
                 [D('1.67321232123'),
                  None, D('1.673'), None, None,
                  D('1.673'), None, None,
                  D('1.673'), None],
                 [D('5.67345434543'),
                  D('5.67'), None, None,
                  D('5.67'), None, None,
                  D('5.67'), None, None]]
        self.assertEqual(erows, arows)
