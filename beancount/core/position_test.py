"""
Unit tests for the Position class.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest
import copy
import random
from datetime import date

from beancount.core import position
from beancount.core import convert
from beancount.core.position import Cost
from beancount.core.position import Position
from beancount.core.position import from_string
from beancount.core.position import from_amounts

from beancount.core.number import ZERO
from beancount.core.number import D
from beancount.core.amount import A
from beancount.core.amount import Amount
from beancount.core import display_context


class TestCost(unittest.TestCase):

    dformat = display_context.DisplayContext().build()

    def test_cost_to_str__detail(self):
        cost = position.Cost(D('101.23'), 'USD', datetime.date(2015, 9, 6), "f4412439c31b")
        self.assertEqual('101.23 USD, 2015-09-06, "f4412439c31b"',
                         position.cost_to_str(cost, self.dformat))

        cost = position.Cost(D('101.23'), 'USD', datetime.date(2015, 9, 6), None)
        self.assertEqual('101.23 USD, 2015-09-06',
                         position.cost_to_str(cost, self.dformat))

        cost = position.Cost(D('101.23'), 'USD', None, None)
        self.assertEqual('101.23 USD',
                         position.cost_to_str(cost, self.dformat))

        cost = position.Cost(D('101.23'), 'USD', None, "f4412439c31b")
        self.assertEqual('101.23 USD, "f4412439c31b"',
                         position.cost_to_str(cost, self.dformat))

        cost = position.Cost(None, None, None, "f4412439c31b")
        self.assertEqual('"f4412439c31b"',
                         position.cost_to_str(cost, self.dformat))

        cost = position.Cost(None, 'USD', None, "f4412439c31b")
        self.assertEqual('"f4412439c31b"',
                         position.cost_to_str(cost, self.dformat))

    def test_cost_to_str__simple(self):
        cost = position.Cost(D('101.23'), 'USD', datetime.date(2015, 9, 6), "f4412439c31b")
        self.assertEqual('101.23 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.Cost(D('101.23'), 'USD', datetime.date(2015, 9, 6), None)
        self.assertEqual('101.23 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.Cost(D('101.23'), 'USD', None, None)
        self.assertEqual('101.23 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.Cost(D('101.23'), 'USD', None, "f4412439c31b")
        self.assertEqual('101.23 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.Cost(None, None, None, "f4412439c31b")
        self.assertEqual('',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.Cost(None, 'USD', None, "f4412439c31b")
        self.assertEqual('',
                         position.cost_to_str(cost, self.dformat, False))


class TestCostSpec(unittest.TestCase):

    dformat = display_context.DisplayContext().build()

    def test_cost_to_str__detail(self):
        cost = position.CostSpec(
            D('101.23'), D('202.46'), 'USD', datetime.date(2015, 9, 6), "f4412439c31b",
            True)
        self.assertEqual('101.23 # 202.46 USD, 2015-09-06, "f4412439c31b", *',
                         position.cost_to_str(cost, self.dformat))

        cost = position.CostSpec(
            D('101.23'), D('202.46'), 'USD', datetime.date(2015, 9, 6), "f4412439c31b",
            False)
        self.assertEqual('101.23 # 202.46 USD, 2015-09-06, "f4412439c31b"',
                         position.cost_to_str(cost, self.dformat))

        cost = position.CostSpec(D('101.23'), None, 'USD', datetime.date(2015, 9, 6),
                                 None, True)
        self.assertEqual('101.23 USD, 2015-09-06, *',
                         position.cost_to_str(cost, self.dformat))

        cost = position.CostSpec(D('101.23'), D('202.46'), 'USD', None, None, False)
        self.assertEqual('101.23 # 202.46 USD',
                         position.cost_to_str(cost, self.dformat))

        cost = position.CostSpec(None, D('202.46'), 'USD', None, None, False)
        self.assertEqual('# 202.46 USD',
                         position.cost_to_str(cost, self.dformat))

        cost = position.CostSpec(D('101.23'), None, 'USD', None, "f4412439c31b", True)
        self.assertEqual('101.23 USD, "f4412439c31b", *',
                         position.cost_to_str(cost, self.dformat))

        cost = position.CostSpec(None, None, None, None, "f4412439c31b", False)
        self.assertEqual('"f4412439c31b"',
                         position.cost_to_str(cost, self.dformat))

        cost = position.CostSpec(None, None, 'USD', None, "f4412439c31b", True)
        self.assertEqual('"f4412439c31b", *',
                         position.cost_to_str(cost, self.dformat))

    def test_cost_to_str__simple(self):
        cost = position.CostSpec(
            D('101.23'), D('202.46'), 'USD', datetime.date(2015, 9, 6), "f4412439c31b",
            True)
        self.assertEqual('101.23 # 202.46 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.CostSpec(
            D('101.23'), D('202.46'), 'USD', datetime.date(2015, 9, 6), "f4412439c31b",
            False)
        self.assertEqual('101.23 # 202.46 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.CostSpec(D('101.23'), None, 'USD', datetime.date(2015, 9, 6), None,
                                 True)
        self.assertEqual('101.23 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.CostSpec(D('101.23'), D('202.46'), 'USD', None, None, False)
        self.assertEqual('101.23 # 202.46 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.CostSpec(None, D('202.46'), 'USD', None, None, False)
        self.assertEqual('# 202.46 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.CostSpec(D('101.23'), None, 'USD', None, "f4412439c31b", True)
        self.assertEqual('101.23 USD',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.CostSpec(None, None, None, None, "f4412439c31b", False)
        self.assertEqual('',
                         position.cost_to_str(cost, self.dformat, False))

        cost = position.CostSpec(None, None, 'USD', None, "f4412439c31b", True)
        self.assertEqual('',
                         position.cost_to_str(cost, self.dformat, False))


class TestPosition(unittest.TestCase):

    def test_from_string__empty(self):
        with self.assertRaises(ValueError):
            from_string('')

    def test_from_string__simple(self):
        pos = from_string('10 USD')
        self.assertEqual(Position(A("10 USD")), pos)

    def test_from_string__with_spaces(self):
        pos = from_string(' - 111.2934  CAD ')
        self.assertEqual(Position(A("-111.2934 CAD")), pos)

    def test_from_string__with_cost(self):
        pos = from_string('2.2 HOOL {532.43 USD}')
        cost = Cost(D('532.43'), 'USD', None, None)
        self.assertEqual(Position(A("2.2 HOOL"), cost), pos)

    def test_from_string__with_cost_and_date(self):
        pos = from_string('2.2 HOOL {532.43 USD, 2014-06-15}')
        cost = Cost(D('532.43'), 'USD', datetime.date(2014, 6, 15), None)
        self.assertEqual(Position(A("2.2 HOOL"), cost), pos)

    def test_from_string__with_label(self):
        pos = from_string('2.2 HOOL {"78c3f7f1315b"}')
        self.assertEqual(
            Position(A("2.2 HOOL"), Cost(None, None, None, "78c3f7f1315b")), pos)

    def test_from_string__with_compound_cost(self):
        pos = from_string('1.1 HOOL {500.00 # 11.00 USD}')
        self.assertEqual(
            Position(A("1.1 HOOL"), Cost(D('510.00'), 'USD', None, None)),
            pos)

    # Not supported in string constructor.
    # def test_from_string__with_merge_cost_spec(self):
    #     pos = from_string('1.1 HOOL {*}')

    def test_from_string__with_everything(self):
        pos = from_string('20 HOOL {532.43 # 20.00 USD, "e4dc1a361022", 2014-06-15}')
        cost = Cost(D('533.43'), 'USD', datetime.date(2014, 6, 15), "e4dc1a361022")
        self.assertEqual(Position(A("20 HOOL"), cost), pos)

    def test_from_string__missing_currency(self):
        with self.assertRaises(ValueError):
            from_string('2.2 HOOL {532.43}')

    def test_str(self):
        pos = from_string('2.2 HOOL {532.43 USD, 2014-06-15}')
        self.assertEqual(('2.2 HOOL {532.43 USD, 2014-06-15}'), str(pos))

    def test_to_string(self):
        pos = from_string('2.2 HOOL {532.43 USD, 2014-06-15}')
        self.assertEqual(('2.2 HOOL {532.43 USD, 2014-06-15}'), pos.to_string())

    def test_to_string_no_detail(self):
        pos = from_string('2.2 HOOL {532.43 USD, 2014-06-15}')
        self.assertEqual(('2.2 HOOL {532.43 USD}'), pos.to_string(detail=False))

    def test_from_amounts(self):
        pos = from_amounts(A('10.00 USD'))
        self.assertEqual(Position(A("10 USD")), pos)

        pos = from_amounts(A('10 HOOL'),
                           A('510.00 USD'))
        self.assertEqual(
            Position(A("10 HOOL"), Cost(D('510'), 'USD', None, None)), pos)

    def test_constructors(self):
        Position(A('123.45 USD'), None)
        Position(A('123.45 USD'), Cost('74.00', 'CAD', None, None))
        Position(A('123.45 USD'), Cost('74.00', 'CAD', date(2013, 2, 3), None))
        Position(Amount(D('123.45'), None), None)
        Position(Amount(None, 'USD'), None)

    def test_compare_zero_to_none(self):
        pos1 = Position(Amount(ZERO, "CAD"), None)
        pos_none = None
        self.assertEqual(pos1, pos_none)
        self.assertEqual(pos_none, pos1)

        pos2 = Position(Amount(ZERO, "USD"), None)
        self.assertNotEqual(pos1, pos2)

    def test_neg(self):
        pos = Position(A("7 CAD"), None)
        npos = -pos
        self.assertEqual(D('-7'), npos.units.number)
        self.assertEqual('CAD', npos.units.currency)
        self.assertEqual(pos.cost, npos.cost)

    def test_abs(self):
        pos = Position(A("7 CAD"), None)
        self.assertEqual(D('7'), abs(pos).units.number)
        pos = Position(A("-7 CAD"), None)
        self.assertEqual(D('7'), abs(pos).units.number)

    def test_mul(self):
        pos = position.from_string("2 HOOL {100.00 USD}")
        pos2 = pos * D('3')
        self.assertEqual(position.from_string("6 HOOL {100.00 USD}"),
                         pos2)

    def test_eq_and_sortkey(self):
        pos1 = Position(A("200 USD"), None)
        pos2 = Position(A("201 USD"), None)
        pos3 = Position(A("100 CAD"), None)
        pos4 = Position(A("101 CAD"), None)
        pos5 = Position(A("50 ZZZ"), None)
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
        pos1 = Position(A("1 USD"), None)
        pos2 = Position(A("1 USD"), Cost(D('10'), 'USD', None, None))
        pos3 = Position(A("1 USD"), Cost(D('11'), 'USD', None, None))
        pos4 = Position(A("1 USD"), Cost(D('12'), 'USD', None, None))

        positions = [pos3, pos2, pos1, pos4]
        self.assertEqual([pos1, pos2, pos3, pos4], sorted(positions))

        for _ in range(64):
            random.shuffle(positions)
            self.assertEqual([pos1, pos2, pos3, pos4], sorted(positions))

    def test_copy(self):
        # Ensure that the lot instances are shared.
        pos1 = Position(A("200 USD"), None)
        pos2 = copy.copy(pos1)
        self.assertEqual(pos1.units, pos2.units)
        self.assertEqual(pos1.cost, pos2.cost)

    def test_quantities(self):
        pos = Position(A("10 USD"), None)
        self.assertEqual(A('10 USD'), pos.units)
        self.assertEqual(A('10 USD'), convert.get_cost(pos))

        pos = Position(A("10 USD"), Cost(D('1.5'), 'AUD', None, None))
        self.assertEqual(A('10 USD'), pos.units)
        self.assertEqual(A('15 AUD'), convert.get_cost(pos))

    def test_negative(self):
        pos = Position(A("28372 USD"), Cost(D('10'), 'AUD', None, None))
        negpos = pos.get_negative()
        self.assertEqual(A('-28372 USD'), negpos.units)
        self.assertEqual(A('-283720 AUD'), convert.get_cost(negpos))

    def test_is_negative_at_cost(self):
        pos1 = Position(A("1 USD"), Cost('10', 'AUD', None, None))
        pos2 = Position(A("-1 USD"), Cost('10', 'AUD', None, None))
        self.assertFalse(pos1.is_negative_at_cost())
        self.assertTrue(pos2.is_negative_at_cost())

    def test_currency_pair(self):
        self.assertEqual(("USD", None),
                         Position(A('100 USD'), None).currency_pair())
        self.assertEqual(("AAPL", "USD"),
                         Position(A('100 AAPL'),
                                  Cost('43.23', 'USD', None, None)).currency_pair())
