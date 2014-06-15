"""
Unit tests for the Position class.
"""
import datetime
import unittest
import copy
from datetime import date

from .amount import ZERO, to_decimal, Amount
from .position import Lot, Position, create_position, from_string


class TestPosition(unittest.TestCase):

    def test_constructors(self):
        position = Position(Lot('USD', None, None),
                            to_decimal('123.45'))
        position = Position(Lot('USD', Amount('74.00', 'CAD'), None),
                            to_decimal('123.45'))
        position = Position(Lot('USD', Amount('74.00', 'CAD'), date(2013, 2, 3)),
                            to_decimal('123.45'))
        with self.assertRaises(Exception):
            Position(None, to_decimal('123.45'))
        with self.assertRaises(Exception):
            Position(Lot('USD', None, None), None)

    def test_compare_zero_to_none(self):
        pos1 = Position(Lot("CAD", None, None), ZERO)
        posNone = None
        self.assertEqual(pos1, posNone)
        self.assertEqual(posNone, pos1)

        pos2 = Position(Lot("USD", None, None), ZERO)
        self.assertNotEqual(pos1, pos2)

    def test_eq_and_sortkey(self):
        pos1 = Position(Lot("USD", None, None), to_decimal('200'))
        pos2 = Position(Lot("USD", None, None), to_decimal('201'))
        pos3 = Position(Lot("CAD", None, None), to_decimal('100'))
        pos4 = Position(Lot("CAD", None, None), to_decimal('101'))
        pos5 = Position(Lot("ZZZ", None, None), to_decimal('50'))
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

    def test_copy(self):
        # Ensure that the lot instances are shared.
        pos1 = Position(Lot("USD", None, None), to_decimal('200'))
        pos2 = copy.copy(pos1)
        self.assertTrue(pos1.lot is pos2.lot)

    def test_getamount(self):
        pos = Position(Lot("USD", Amount('10', 'AUD'), None), to_decimal('28372'))
        self.assertEqual(Amount('28372', 'USD'), pos.get_amount())
        self.assertEqual(Amount('283720', 'AUD'), pos.get_cost())

        cpos = pos.get_cost_position()
        self.assertTrue(isinstance(cpos, Position))
        self.assertEqual(Amount('283720', 'AUD'), cpos.get_amount())
        self.assertEqual(Amount('283720', 'AUD'), cpos.get_cost())

    def test_add(self):
        pos = Position(Lot("USD", Amount('10', 'AUD'), None), to_decimal('28372'))
        pos.add(to_decimal('337'))
        self.assertEqual(Amount('28709', 'USD'), pos.get_amount())
        self.assertEqual(Amount('287090', 'AUD'), pos.get_cost())

    def test_negative(self):
        pos = Position(Lot("USD", Amount('10', 'AUD'), None), to_decimal('28372'))
        negpos = pos.get_negative()
        self.assertEqual(Amount('-28372', 'USD'), negpos.get_amount())
        self.assertEqual(Amount('-283720', 'AUD'), negpos.get_cost())



class TestPositionFromString(unittest.TestCase):

    def test_from_string(self):
        with self.assertRaises(ValueError):
            pos = from_string('')

        pos = from_string('10 USD')
        self.assertEqual(Position(Lot("USD", None, None), to_decimal('10')), pos)

        pos = from_string(' 111.2934  CAD ')
        self.assertEqual(Position(Lot("CAD", None, None), to_decimal('111.2934')), pos)

        pos = from_string('2.2 GOOG {532.43 USD}')
        cost = Amount(to_decimal('532.43'), 'USD')
        self.assertEqual(Position(Lot("GOOG", cost, None), to_decimal('2.2')), pos)

        pos = from_string('2.2 GOOG {532.43 USD / 2014-06-15}')
        cost = Amount(to_decimal('532.43'), 'USD')
        lot_date = datetime.date(2014, 6, 15)
        self.assertEqual(Position(Lot("GOOG", cost, lot_date), to_decimal('2.2')), pos)

    # FIXME: Remvoe this eventually.
    def test_create_position(self):
        pos1 = Position(Lot("USD", Amount('10', 'AUD'), None), to_decimal('28372'))
        pos2 = create_position('28372', 'USD', Amount('10', 'AUD'))
        self.assertEqual(pos1, pos2)

        pos1 = Position(Lot("USD", None, None), to_decimal('28372'))
        pos2 = create_position('28372', 'USD')
        self.assertEqual(pos1, pos2)
