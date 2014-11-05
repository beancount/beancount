import datetime
import re
import unittest

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.core import position
from beancount.core import amount
from beancount.query import query_parser as q
from beancount.query import query_compile as c
from beancount.query import query_env as cc


class TestCompileDataTypes(unittest.TestCase):

    def test_compile_EvalLength(self):
        with self.assertRaises(c.CompilationError):
            cc.Length([c.EvalConstant(17)])
        c_length = cc.Length([c.EvalConstant('testing')])
        self.assertEqual(int, c_length.dtype)

    def test_compile_EvalYear(self):
        with self.assertRaises(c.CompilationError):
            cc.Year([c.EvalConstant(17)])
        c_year = cc.Year([c.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_year.dtype)

    def test_compile_EvalMonth(self):
        with self.assertRaises(c.CompilationError):
            cc.Month([c.EvalConstant(17)])
        c_month = cc.Month([c.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_month.dtype)

    def test_compile_EvalDay(self):
        with self.assertRaises(c.CompilationError):
            cc.Day([c.EvalConstant(17)])
        c_day = cc.Day([c.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_day.dtype)

    def test_compile_EvalUnits(self):
        with self.assertRaises(c.CompilationError):
            cc.UnitsPosition([c.EvalConstant(17)])
        with self.assertRaises(c.CompilationError):
            cc.UnitsPosition([c.EvalConstant(inventory.Inventory())])
        c_units = cc.UnitsPosition([c.EvalConstant(position.Position.from_string('100 USD'))])
        self.assertEqual(amount.Amount, c_units.dtype)

    def test_compile_EvalCost(self):
        with self.assertRaises(c.CompilationError):
            cc.CostPosition([c.EvalConstant(17)])
        with self.assertRaises(c.CompilationError):
            cc.CostPosition([c.EvalConstant(inventory.Inventory())])
        c_cost = cc.CostPosition([c.EvalConstant(position.Position.from_string('100 USD'))])
        self.assertEqual(amount.Amount, c_cost.dtype)

    def test_compile_EvalSum(self):
        with self.assertRaises(c.CompilationError):
            cc.Sum([c.EvalConstant('testing')])
        c_sum = cc.Sum([c.EvalConstant(17)])
        self.assertEqual(int, c_sum.dtype)
        c_sum = cc.Sum([c.EvalConstant(D('17.'))])
        self.assertEqual(Decimal, c_sum.dtype)

    def test_compile_EvalCount(self):
        c_count = cc.Count([c.EvalConstant(17)])
        self.assertEqual(int, c_count.dtype)

    def test_compile_EvalFirst(self):
        c_first = cc.First([c.EvalConstant(17.)])
        self.assertEqual(float, c_first.dtype)

    def test_compile_EvalLast(self):
        c_last = cc.Last([c.EvalConstant(17.)])
        self.assertEqual(float, c_last.dtype)

    def test_compile_columns(self):
        class_types = [
            # Postings accessors.
            (cc.TypeColumn, str),
            (cc.FilenameColumn, str),
            (cc.LineNoColumn, int),
            (cc.DateColumn, datetime.date),
            (cc.FlagColumn, str),
            (cc.PayeeColumn, str),
            (cc.NarrationColumn, str),
            (cc.TagsColumn, set),
            (cc.LinksColumn, set),
            (cc.AccountColumn, str),
            (cc.NumberColumn, Decimal),
            (cc.CurrencyColumn, str),
            (cc.ChangeColumn, position.Position),
            # Entries accessors.
            (cc.TypeEntryColumn, str),
            (cc.FilenameEntryColumn, str),
            (cc.LineNoEntryColumn, int),
            (cc.DateEntryColumn, datetime.date),
            (cc.FlagEntryColumn, str),
            (cc.PayeeEntryColumn, str),
            (cc.NarrationEntryColumn, str),
            (cc.TagsEntryColumn, set),
            (cc.LinksEntryColumn, set),
            ]
        for cls, dtype in class_types:
            instance = cls()
            self.assertEqual(dtype, instance.dtype)
