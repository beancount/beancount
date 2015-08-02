"""
Unit tests for the Position class.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import unittest
import copy
import random
from datetime import date

from beancount.core.number import ZERO
from beancount.core.number import D
from beancount.core.amount import Amount
from beancount.core.position import Lot
from beancount.core.position import Position
from beancount.core.position import from_string
from beancount.core.position import from_amounts
from beancount.core.position import lot_currency_pair


class TestPosition(unittest.TestCase):

    def test_from_string__empty(self):
        with self.assertRaises(ValueError):
            from_string('')

    def test_from_string__simple(self):
        pos = from_string('10 USD')
        self.assertEqual(Position(Lot("USD", None, None), D('10')), pos)

    def test_from_string__with_spaces(self):
        pos = from_string(' - 111.2934  CAD ')
        self.assertEqual(Position(Lot("CAD", None, None), D('-111.2934')), pos)

    def test_from_string__with_cost(self):
        pos = from_string('2.2 GOOG {532.43 USD}')
        cost = Amount(D('532.43'), 'USD')
        self.assertEqual(Position(Lot("GOOG", cost, None), D('2.2')), pos)

    def test_from_string__with_cost_and_date(self):
        pos = from_string('2.2 GOOG {532.43 USD, 2014-06-15}')
        cost = Amount(D('532.43'), 'USD')
        lot_date = datetime.date(2014, 6, 15)
        self.assertEqual(Position(Lot("GOOG", cost, lot_date), D('2.2')), pos)

    def test_from_string__with_label(self):
        pos = from_string('2.2 GOOG {"78c3f7f1315b"}')
        self.assertEqual(Position(Lot("GOOG", None, None), D('2.2')), pos)

    def test_from_string__with_compound_cost(self):
        pos = from_string('1.1 GOOG {500.00 # 11.00 USD}')
        self.assertEqual(
            Position(
                Lot("GOOG", Amount(D('510.00'), 'USD'), None),
                D('1.1')),
            pos)

    def test_from_string__with_merge_cost_spec(self):
        pos = from_string('1.1 GOOG {*}')
        self.assertEqual(Position(Lot("GOOG", None, None), D('1.1')), pos)

    def test_from_string__with_everything(self):
        pos = from_string('20 GOOG {*, 532.43 # 20.00 USD, "e4dc1a361022", 2014-06-15}')
        cost = Amount(D('533.43'), 'USD')
        lot_date = datetime.date(2014, 6, 15)
        self.assertEqual(Position(Lot("GOOG", cost, lot_date), D('20')), pos)

    def test_from_string__missing_currency(self):
        with self.assertRaises(ValueError):
            from_string('2.2 GOOG {532.43}')

    def test_str(self):
        pos = from_string('2.2 GOOG {532.43 USD, 2014-06-15}')
        self.assertEqual(('2.2 GOOG {532.43 USD, 2014-06-15}'), str(pos))

    def test_to_string(self):
        pos = from_string('2.2 GOOG {532.43 USD, 2014-06-15}')
        self.assertEqual(('2.2 GOOG {532.43 USD, 2014-06-15}'), pos.to_string())

    def test_to_string_no_detail(self):
        pos = from_string('2.2 GOOG {532.43 USD, 2014-06-15}')
        self.assertEqual(('2.2 GOOG {532.43 USD}'), pos.to_string(detail=False))

    def test_from_amounts(self):
        pos = from_amounts(Amount(D('10.00'), 'USD'))
        self.assertEqual(Position(Lot("USD", None, None), D('10')), pos)

        pos = from_amounts(Amount(D('10'), 'GOOG'),
                           Amount(D('510.00'), 'USD'))
        self.assertEqual(
            Position(Lot("GOOG", Amount(D('510'), 'USD'), None), D('10')), pos)

    def test_constructors(self):
        Position(Lot('USD', None, None), D('123.45'))
        Position(Lot('USD', Amount('74.00', 'CAD'), None), D('123.45'))
        Position(Lot('USD', Amount('74.00', 'CAD'), date(2013, 2, 3)), D('123.45'))
        with self.assertRaises(Exception):
            Position(None, D('123.45'))
        Position(Lot('USD', None, None), None)

    def test_compare_zero_to_none(self):
        pos1 = Position(Lot("CAD", None, None), ZERO)
        pos_none = None
        self.assertEqual(pos1, pos_none)
        self.assertEqual(pos_none, pos1)

        pos2 = Position(Lot("USD", None, None), ZERO)
        self.assertNotEqual(pos1, pos2)

    def test_neg(self):
        pos = Position(Lot("CAD", None, None), D('7'))
        npos = -pos
        self.assertEqual(D('-7'), npos.number)
        self.assertEqual(pos.lot, npos.lot)

    def test_eq_and_sortkey(self):
        pos1 = Position(Lot("USD", None, None), D('200'))
        pos2 = Position(Lot("USD", None, None), D('201'))
        pos3 = Position(Lot("CAD", None, None), D('100'))
        pos4 = Position(Lot("CAD", None, None), D('101'))
        pos5 = Position(Lot("ZZZ", None, None), D('50'))
        positions = [pos5, pos4, pos3, pos2, pos1]
        positions.sort()

        self.assertTrue(pos1 < pos2)
        self.assertTrue(pos2 < pos3)
        self.assertTrue(pos3 < pos4)
        self.assertTrue(pos4 < pos5)

        self.assertTrue(positions[0] is pos1)
        self.assertTrue(positions[1] is pos2)
        self.assertTrue(positions[2] is pos3)
        self.assertTrue(positions[3] is pos4)
        self.assertTrue(positions[4] is pos5)

    def test_eq_and_sortkey__bycost(self):
        pos1 = Position(Lot("USD", None, None), D('1'))
        pos2 = Position(Lot("USD", Amount.from_string('10 USD'), None), D('1'))
        pos3 = Position(Lot("USD", Amount.from_string('11 USD'), None), D('1'))
        pos4 = Position(Lot("USD", Amount.from_string('12 USD'), None), D('1'))

        positions = [pos3, pos2, pos1, pos4]
        self.assertEqual([pos1, pos2, pos3, pos4], sorted(positions))

        for _ in range(64):
            random.shuffle(positions)
            self.assertEqual([pos1, pos2, pos3, pos4], sorted(positions))

    def test_copy(self):
        # Ensure that the lot instances are shared.
        pos1 = Position(Lot("USD", None, None), D('200'))
        pos2 = copy.copy(pos1)
        self.assertTrue(pos1.lot is pos2.lot)

    def test_quantities(self):
        A = Amount.from_string  # pylint: disable=invalid-name

        pos = Position(Lot("USD", None, None), D('10'))
        self.assertEqual(A('10 USD'), pos.get_units())
        self.assertEqual(A('10 USD'), pos.get_cost())
        self.assertEqual(A('10 USD'), pos.get_weight())
        self.assertEqual(A('16 AUD'), pos.get_weight(A('1.6 AUD')))

        pos = Position(Lot("USD", A('1.5 AUD'), None), D('10'))
        self.assertEqual(A('10 USD'), pos.get_units())
        self.assertEqual(A('15 AUD'), pos.get_cost())
        self.assertEqual(A('15 AUD'), pos.get_weight())
        self.assertEqual(A('15 AUD'), pos.get_weight(A('1.6 AUD')))

        cost_pos = pos.cost()
        self.assertEqual(A('15 AUD'), cost_pos.get_units())
        self.assertEqual(A('15 AUD'), cost_pos.get_cost())
        self.assertEqual(A('15 AUD'), cost_pos.get_weight())
        with self.assertRaises(AssertionError):
            self.assertEqual(A('15 AUD'), cost_pos.get_weight(A('1.6 AUD')))

    def test_add(self):
        pos = Position(Lot("USD", Amount('10', 'AUD'), None), D('28372'))
        pos.add(D('337'))
        self.assertEqual(Amount('28709', 'USD'), pos.get_units())
        self.assertEqual(Amount('287090', 'AUD'), pos.get_cost())

    def test_negative(self):
        pos = Position(Lot("USD", Amount('10', 'AUD'), None), D('28372'))
        negpos = pos.get_negative()
        self.assertEqual(Amount('-28372', 'USD'), negpos.get_units())
        self.assertEqual(Amount('-283720', 'AUD'), negpos.get_cost())

    def test_is_negative_at_cost(self):
        pos1 = Position(Lot("USD", Amount('10', 'AUD'), None), D('1'))
        pos2 = Position(Lot("USD", Amount('10', 'AUD'), None), D('-1'))
        self.assertFalse(pos1.is_negative_at_cost())
        self.assertTrue(pos2.is_negative_at_cost())

    def test_lot_currency_pair(self):
        self.assertEqual(("USD", None),
                         lot_currency_pair(Lot("USD", None, None)))
        self.assertEqual(("AAPL", "USD"),
                         lot_currency_pair(Lot("AAPL", Amount('43.23', 'USD'), None)))
