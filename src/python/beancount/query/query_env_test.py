__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import unittest

from beancount.core.number import D
from beancount.core.number import Decimal
from beancount.core import inventory
from beancount.core import position
from beancount.core import amount
from beancount.query import query_compile as qc
from beancount.query import query_env as qe


class TestCompileDataTypes(unittest.TestCase):

    def test_compile_EvalLength(self):
        with self.assertRaises(qc.CompilationError):
            qe.Length([qc.EvalConstant(17)])
        c_length = qe.Length([qc.EvalConstant('testing')])
        self.assertEqual(int, c_length.dtype)

    def test_compile_EvalYear(self):
        with self.assertRaises(qc.CompilationError):
            qe.Year([qc.EvalConstant(17)])
        c_year = qe.Year([qc.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_year.dtype)

    def test_compile_EvalMonth(self):
        with self.assertRaises(qc.CompilationError):
            qe.Month([qc.EvalConstant(17)])
        c_month = qe.Month([qc.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_month.dtype)

    def test_compile_EvalDay(self):
        with self.assertRaises(qc.CompilationError):
            qe.Day([qc.EvalConstant(17)])
        c_day = qe.Day([qc.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_day.dtype)

    def test_compile_EvalUnits(self):
        with self.assertRaises(qc.CompilationError):
            qe.UnitsPosition([qc.EvalConstant(17)])
        with self.assertRaises(qc.CompilationError):
            qe.UnitsPosition([qc.EvalConstant(inventory.Inventory())])
        c_units = qe.UnitsPosition(
            [qc.EvalConstant(position.Position.from_string('100 USD'))])
        self.assertEqual(amount.Amount, c_units.dtype)

    def test_compile_EvalCost(self):
        with self.assertRaises(qc.CompilationError):
            qe.CostPosition([qc.EvalConstant(17)])
        with self.assertRaises(qc.CompilationError):
            qe.CostPosition([qc.EvalConstant(inventory.Inventory())])
        c_cost = qe.CostPosition([qc.EvalConstant(
            position.Position.from_string('100 USD'))])
        self.assertEqual(amount.Amount, c_cost.dtype)

    def test_compile_EvalSum(self):
        with self.assertRaises(qc.CompilationError):
            qe.Sum([qc.EvalConstant('testing')])
        c_sum = qe.Sum([qc.EvalConstant(17)])
        self.assertEqual(int, c_sum.dtype)
        c_sum = qe.Sum([qc.EvalConstant(D('17.'))])
        self.assertEqual(Decimal, c_sum.dtype)

    def test_compile_EvalCount(self):
        c_count = qe.Count([qc.EvalConstant(17)])
        self.assertEqual(int, c_count.dtype)

    def test_compile_EvalFirst(self):
        c_first = qe.First([qc.EvalConstant(17.)])
        self.assertEqual(float, c_first.dtype)

    def test_compile_EvalLast(self):
        c_last = qe.Last([qc.EvalConstant(17.)])
        self.assertEqual(float, c_last.dtype)

    def test_compile_columns(self):
        class_types = [
            # Postings accessors.
            (qe.TypeColumn, str),
            (qe.FilenameColumn, str),
            (qe.LineNoColumn, int),
            (qe.DateColumn, datetime.date),
            (qe.FlagColumn, str),
            (qe.PayeeColumn, str),
            (qe.NarrationColumn, str),
            (qe.TagsColumn, set),
            (qe.LinksColumn, set),
            (qe.AccountColumn, str),
            (qe.NumberColumn, Decimal),
            (qe.CurrencyColumn, str),
            (qe.PositionColumn, position.Position),
            # Entries accessors.
            (qe.TypeEntryColumn, str),
            (qe.FilenameEntryColumn, str),
            (qe.LineNoEntryColumn, int),
            (qe.DateEntryColumn, datetime.date),
            (qe.FlagEntryColumn, str),
            (qe.PayeeEntryColumn, str),
            (qe.NarrationEntryColumn, str),
            (qe.TagsEntryColumn, set),
            (qe.LinksEntryColumn, set),
            ]
        for cls, dtype in class_types:
            instance = cls()
            self.assertEqual(dtype, instance.dtype)
