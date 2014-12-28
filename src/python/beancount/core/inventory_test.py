"""
Unit tests for the Inventory class.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import copy
from datetime import date
import types

from .amount import D
from .position import Position
from .position import Lot
from .inventory import Inventory
from . import amount
from . import position
from . import inventory


A = amount.from_string


def invariant_check(method, prefun, postfun):
    """Decorate a method with the pre/post invariant checkers.

    Args:
      method: An unbound method to instrument.
      prefun: A function that checks invariants pre-call.
      postfun: A function that checks invariants pre-call.
    Returns:
      An unbound method, decorated.
    """
    reentrant = []
    def new_method(self, *args, **kw):
        if not reentrant:
            reentrant.append(None)
            prefun(self)
        result = method(self, *args, **kw)
        if not reentrant:
            postfun(self)
            reentrant.pop()
        return result
    return new_method

def instrument_invariants(klass, prefun, postfun):
    """Instrument the class 'klass' with pre/post invariant
    checker functions.

    Args:
      klass: A class object, whose methods to be instrumented.
      prefun: A function that checks invariants pre-call.
      postfun: A function that checks invariants pre-call.
    """
    instrumented = {}
    for attrname, object_ in klass.__dict__.items():
        if attrname.startswith('_'):
            continue
        if not isinstance(object_, types.FunctionType):
            continue
        instrumented[attrname] = object_
        setattr(klass, attrname,
                invariant_check(object_, prefun, postfun))
    klass.__instrumented = instrumented

def uninstrument_invariants(klass):
    """Undo the instrumentation for invariants.

    Args:
      klass: A class object, whose methods to be uninstrumented.
    """
    instrumented = getattr(klass, '__instrumented', None)
    if instrumented:
        for attrname, object_ in instrumented.items():
            setattr(klass, attrname, object_)
    del klass.__instrumented


def setUp(module):
    instrument_invariants(Inventory,
                          inventory.check_invariants,
                          inventory.check_invariants)

def tearDown(module):
    uninstrument_invariants(Inventory)


class TestInventory(unittest.TestCase):

    def checkAmount(self, inventory, number, currency):
        amount_ = amount.Amount(number, currency)
        inv_amount = inventory.get_units(amount_.currency)
        self.assertEqual(inv_amount, amount_)

    def test_from_string(self):
        inv = inventory.from_string('')
        self.assertEqual(Inventory(), inv)

        inv = inventory.from_string('10 USD')
        self.assertEqual(
            Inventory([Position(Lot("USD", None, None), D('10'))]),
            inv)

        inv = inventory.from_string(' 10.00  USD ')
        self.assertEqual(
            Inventory([Position(Lot("USD", None, None), D('10'))]),
            inv)

        inv = inventory.from_string('1 USD, 2 CAD')
        self.assertEqual(
            Inventory([Position(Lot("USD", None, None), D('1')),
                       Position(Lot("CAD", None, None), D('2'))]),
            inv)

        inv = inventory.from_string('2.2 GOOG {532.43 USD}, 3.413 EUR')
        self.assertEqual(
            Inventory([Position(Lot("GOOG", A('532.43 USD'), None),
                                D('2.2')),
                       Position(Lot("EUR", None, None), D('3.413'))]),
            inv)

    def test_ctor_empty_len(self):
        # Test regular constructor.
        inv = Inventory()
        self.assertTrue(inv.is_empty())
        self.assertEqual(0, len(inv))

        inv = Inventory([position.from_string('100.00 USD'),
                         position.from_string('101.00 USD')])
        self.assertFalse(inv.is_empty())
        self.assertEqual(1, len(inv))

        inv = Inventory([position.from_string('100.00 USD'),
                         position.from_string('100.00 CAD')])
        self.assertFalse(inv.is_empty())
        self.assertEqual(2, len(inv))

        inv = Inventory()
        self.assertEqual(0, len(inv))
        inv.add_amount(A('100 USD'))
        self.assertEqual(1, len(inv))
        inv.add_amount(A('100 CAD'))
        self.assertEqual(2, len(inv))

    def test_str(self):
        inv = Inventory.from_string('100.00 USD, 101.00 CAD')
        self.assertEqual('(100.00 USD, 101.00 CAD)', str(inv))

    def test_copy(self):
        inv = Inventory()
        inv.add_amount(A('100.00 USD'))
        self.checkAmount(inv, '100', 'USD')

        # Test copying.
        inv2 = copy.copy(inv)
        inv2.add_amount(A('50.00 USD'))
        self.checkAmount(inv2, '150', 'USD')

        # Check that the original object is not modified.
        self.checkAmount(inv, '100', 'USD')

    def test_op_eq(self):
        inv1 = Inventory.from_string('100 USD, 100 CAD')
        inv2 = Inventory.from_string('100 CAD, 100 USD')
        self.assertEqual(inv1, inv2)
        self.assertEqual(inv2, inv1)

        inv3 = Inventory.from_string('200 USD, 100 CAD')
        self.assertNotEqual(inv1, inv3)
        self.assertNotEqual(inv3, inv1)

        inv4 = Inventory.from_string('100 USD, 100 JPY')
        self.assertNotEqual(inv1, inv4)
        self.assertNotEqual(inv4, inv1)

        inv5 = Inventory.from_string('100 JPY, 100 USD')
        self.assertEqual(inv4, inv5)

    def test_is_small(self):
        inv = Inventory.from_string('1.50 JPY, 1.51 USD, 1.52 CAD')
        self.assertFalse(inv.is_small(D('1.49')))
        self.assertFalse(inv.is_small(D('1.50')))
        self.assertTrue(inv.is_small(D('1.53')))
        self.assertTrue(inv.is_small(D('1.52')))

        ninv = -inv
        self.assertFalse(ninv.is_small(D('1.49')))
        self.assertFalse(ninv.is_small(D('1.50')))
        self.assertTrue(ninv.is_small(D('1.53')))
        self.assertTrue(ninv.is_small(D('1.52')))

    def test_op_neg(self):
        inv = Inventory()
        inv.add_amount(A('10 USD'))
        ninv = -inv
        self.checkAmount(ninv, '-10', 'USD')

        pinv = Inventory.from_string('1.50 JPY, 1.51 USD, 1.52 CAD')
        ninv = Inventory.from_string('-1.50 JPY, -1.51 USD, -1.52 CAD')
        self.assertEqual(pinv, -ninv)

    def test_get_units(self):
        inv = Inventory.from_string('40.50 JPY, 40.51 USD {1.01 CAD}, 40.52 CAD')
        self.assertEqual(inv.get_units('JPY'), A('40.50 JPY'))
        self.assertEqual(inv.get_units('USD'), A('40.51 USD'))
        self.assertEqual(inv.get_units('CAD'), A('40.52 CAD'))
        self.assertEqual(inv.get_units('AUD'), A('0 AUD'))
        self.assertEqual(inv.get_units('NZD'), A('0 NZD'))

    def test_units(self):
        inv = Inventory()
        self.assertEqual(inv.units(), Inventory.from_string(''))

        inv = Inventory.from_string('40.50 JPY, 40.51 USD {1.01 CAD}, 40.52 CAD')
        self.assertEqual(inv.units(),
                         Inventory.from_string('40.50 JPY, 40.51 USD, 40.52 CAD'))

        # Check that the same units coalesce.
        inv = Inventory.from_string('2 GOOG {400 USD}, 3 GOOG {410 USD}')
        self.assertEqual(inv.units(), Inventory.from_string('5 GOOG'))

        inv = Inventory.from_string('2 GOOG {400 USD}, -3 GOOG {410 USD}')
        self.assertEqual(inv.units(), Inventory.from_string('-1 GOOG'))

    POSITIONS_ALL_KINDS = [
        position.from_string('40.50 USD'),
        position.from_string('40.50 USD {1.10 CAD}'),
        position.from_string('40.50 USD {1.10 CAD, 2012-01-01}')]

    def test_cost(self):
        inv = Inventory(self.POSITIONS_ALL_KINDS +
                        [position.from_string('50.00 CAD')])
        inv_cost = inv.cost()
        self.assertEqual(Inventory.from_string('40.50 USD, 139.10 CAD'), inv_cost)

    def test_average(self):
        # Identity, no aggregation.
        inv = Inventory.from_string('40.50 JPY, 40.51 USD {1.01 CAD}, 40.52 CAD')
        self.assertEqual(inv.average(), inv)

        # Identity, no aggregation, with a mix of lots at cost and without cost.
        inv = Inventory.from_string('40 USD {1.01 CAD}, 40 USD')
        self.assertEqual(inv.average(), inv)

        # Aggregation.
        inv = Inventory.from_string('40 USD {1.01 CAD}, 40 USD {1.02 CAD}')
        self.assertEqual(inv.average(), Inventory.from_string('80.00 USD {1.015 CAD}'))

        # Aggregation, more units.
        inv = Inventory.from_string('2 GOOG {500 USD}, 3 GOOG {520 USD}, 4 GOOG {530 USD}')
        self.assertEqual(inv.average(), Inventory.from_string('9 GOOG {520 USD}'))

    def test_currency_pairs(self):
        inv = Inventory()
        self.assertEqual(set(), inv.currency_pairs())

        inv = Inventory.from_string('40 USD {1.01 CAD}, 40 USD')
        self.assertEqual(set([('USD', 'CAD'), ('USD', None)]), inv.currency_pairs())

        inv = Inventory.from_string('40 AAPL {1.01 USD}, 10 GOOG {2.02 USD}')
        self.assertEqual(set([('AAPL', 'USD'), ('GOOG', 'USD')]), inv.currency_pairs())

    def test_get_position(self):
        inv = Inventory(self.POSITIONS_ALL_KINDS)
        self.assertEqual(
            position.from_string('40.50 USD'),
            inv.get_position(Lot('USD', None, None)))
        self.assertEqual(
            position.from_string('40.50 USD {1.10 CAD}'),
            inv.get_position(Lot('USD', A('1.10 CAD'), None)))
        self.assertEqual(
            position.from_string('40.50 USD {1.10 CAD, 2012-01-01}'),
            inv.get_position(Lot('USD', A('1.10 CAD'), date(2012, 1, 1))))

    def test_add(self):
        inv = Inventory()
        inv.add_amount(A('100.00 USD'))
        self.checkAmount(inv, '100', 'USD')

        # Add some amount
        inv.add_amount(A('25.01 USD'))
        self.checkAmount(inv, '125.01', 'USD')

        # Subtract some amount.
        inv.add_amount(A('-12.73 USD'))
        self.checkAmount(inv, '112.28', 'USD')

        # Subtract some to be negative (should be allowed if no lot).
        inv.add_amount(A('-120 USD'))
        self.checkAmount(inv, '-7.72', 'USD')

        # Subtract some more.
        inv.add_amount(A('-1 USD'))
        self.checkAmount(inv, '-8.72', 'USD')

        # Add to above zero again
        inv.add_amount(A('18.72 USD'))
        self.checkAmount(inv, '10', 'USD')

    def test_add_multi_currency(self):
        inv = Inventory()
        inv.add_amount(A('100 USD'))
        inv.add_amount(A('100 CAD'))
        self.checkAmount(inv, '100', 'USD')
        self.checkAmount(inv, '100', 'CAD')

        inv.add_amount(A('25 USD'))
        self.checkAmount(inv, '125', 'USD')
        self.checkAmount(inv, '100', 'CAD')

    def test_add_withlots(self):
        # Testing the strict case where everything matches, with only a cost.
        inv = Inventory()
        inv.add_amount(A('50 GOOG'), A('700 USD'))
        self.checkAmount(inv, '50', 'GOOG')

        inv.add_amount(A('-40 GOOG'), A('700 USD'))
        self.checkAmount(inv, '10', 'GOOG')

        position_, _ = inv.add_amount(A('-12 GOOG'),
                                      A('700 USD'))
        self.assertTrue(position_.is_negative_at_cost())

        # Testing the strict case where everything matches, a cost and a lot-date.
        inv = Inventory()
        inv.add_amount(A('50 GOOG'), A('700 USD'), date(2000, 1, 1))
        self.checkAmount(inv, '50', 'GOOG')

        inv.add_amount(A('-40 GOOG'), A('700 USD'), date(2000, 1, 1))
        self.checkAmount(inv, '10', 'GOOG')

        position_, _ = inv.add_amount(A('-12 GOOG'), A('700 USD'),
                                      date(2000, 1, 1))
        self.assertTrue(position_.is_negative_at_cost())

    def test_add_allow_negative(self):

        def check_allow_negative(inv):
            position_, _ = inv.add_amount(A('-11 USD'))
            self.assertFalse(position_.is_negative_at_cost())
            position_, _ = inv.add_amount(A('-11 USD'), A('1.10 CAD'))
            self.assertTrue(position_.is_negative_at_cost())
            position_, _ = inv.add_amount(A('-11 USD'), None, date(2012, 1, 1))
            self.assertTrue(position_.is_negative_at_cost())
            inv.add_amount(A('-11 USD'), A('1.10 CAD'))
            inv.add_amount(A('-11 USD'), None, date(2012, 1, 1))

        # Test adding to a position that does not exist.
        inv = Inventory()
        check_allow_negative(inv)

        # Test adding to a position that does exist.
        inv = Inventory.from_string(
            '10 USD, 10 USD {1.10 CAD}, 10 USD {1.10 CAD, 2012-01-01}')
        check_allow_negative(inv)

    def test_add_position(self):
        inv = Inventory()
        for position in self.POSITIONS_ALL_KINDS:
            inv.add_position(position)
        self.assertEqual(Inventory(self.POSITIONS_ALL_KINDS), inv)

    def test_op_add(self):
        inv1 = Inventory.from_string('17.00 USD')
        orig_inv1 = Inventory.from_string('17.00 USD')
        inv2 = Inventory.from_string('21.00 CAD')
        inv3 = inv1 + inv2
        self.assertEqual(Inventory.from_string('17.00 USD, 21.00 CAD'), inv3)
        self.assertEqual(orig_inv1, inv1)

    def test_update(self):
        inv1 = Inventory.from_string('11 USD')
        inv2 = Inventory.from_string('12 CAD')
        inv_updated = inv1.add_inventory(inv2)
        expect_updated = Inventory.from_string('11 USD, 12 CAD')
        self.assertEqual(expect_updated, inv_updated)
        self.assertEqual(expect_updated, inv1)

    def test_sum_inventories(self):
        inv1 = Inventory()
        inv1.add_amount(A('10 USD'))

        inv2 = Inventory()
        inv2.add_amount(A('20 CAD'))
        inv2.add_amount(A('55 GOOG'))

        inv = inv1 + inv2
