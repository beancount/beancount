import datetime
import unittest

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.core import position
from beancount.query import query_parser as q
from beancount.query import query_compile as c


class QueryCompilerTestBase(unittest.TestCase):

    maxDiff = 8192

    def setUp(self):
        self.parser = q.Parser()

    def compile(self, query):
        """Parse one query and compile it.

        Args:
          query: An SQL query to be parsed.
        Returns:
          The AST.
        """
        statement = self.parser.parse(query.strip())
        return c.compile_select(statement)

    def assertCompile(self, expected, query, debug=False):
        """Assert parsed and compiled contents from 'query' is 'expected'.

        Args:
          expected: An expected AST to compare against the parsed value.
          query: An SQL query to be parsed.
          debug: A boolean, if true, print extra debugging information on the console.
        Raises:
          AssertionError: If the actual AST does not match the expected one.
        """
        actual = self.compile(query)
        if debug:
            print()
            print()
            print(actual)
            print()
        try:
            self.assertEqual(expected, actual)
            return actual
        except AssertionError:
            raise


class TestCompileExpression(unittest.TestCase):

    def test_expr_invalid(self):
        with self.assertRaises(c.CompilationError):
            c.compile_expression(q.Column('invalid'), c.TargetsContext())

    def test_expr_column(self):
        self.assertEqual(c.FilenameColumn(),
                         c.compile_expression(q.Column('filename'), c.TargetsContext()))

    def test_expr_function(self):
        self.assertEqual(c.Sum([c.ChangeColumn()]),
                         c.compile_expression(q.Function('sum', [q.Column('change')]),
                                              c.TargetsContext()))

    def test_expr_unaryop(self):
        self.assertEqual(c.EvalNot(c.AccountColumn()),
                         c.compile_expression(q.Not(q.Column('account')),
                                              c.TargetsContext()))

    def test_expr_binaryop(self):
        self.assertEqual(c.EvalEqual(c.DateColumn(),
                                     c.EvalConstant(datetime.date(2014, 1, 1))),
                         c.compile_expression(
                             q.Equal(q.Column('date'),
                                     q.Constant(datetime.date(2014, 1, 1))),
                             c.TargetsContext()))

    def test_expr_constant(self):
        self.assertEqual(c.EvalConstant(D(17)),
                         c.compile_expression(q.Constant(D(17)), c.TargetsContext()))


class TestCompileExpressionDataTypes(unittest.TestCase):

    def test_expr_function_arity(self):
        # Compile with the correct number of arguments.
        c.compile_expression(q.Function('sum', [q.Column('number')]),
                             c.TargetsContext())

        # Compile with an incorrect number of arguments.
        with self.assertRaises(c.CompilationError):
            c.compile_expression(q.Function('sum', [q.Column('date'),
                                                    q.Column('account')]),
                                 c.TargetsContext())


class TestCompileAggregateChecks(unittest.TestCase):

    def test_is_aggregrate_simple(self):
        self.assertFalse(c.is_aggregate(
            c.ChangeColumn()))
        self.assertFalse(c.is_aggregate(
            c.Length([c.AccountColumn()])))
        self.assertTrue(c.is_aggregate(
            c.Sum([c.ChangeColumn()])))

    def test_is_aggregrate_derived(self):
        self.assertFalse(c.is_aggregate(
            c.EvalAnd(
                c.EvalEqual(c.ChangeColumn(), c.EvalConstant(42)),
                c.EvalOr(
                    c.EvalNot(c.EvalEqual(c.DateColumn(),
                                          c.EvalConstant(datetime.date(2014, 1, 1)))),
                    c.EvalConstant(False)))))

        self.assertTrue(c.is_aggregate(
            c.EvalAnd(
                c.EvalEqual(c.ChangeColumn(), c.EvalConstant(42)),
                c.EvalOr(
                    c.EvalNot(c.EvalEqual(c.DateColumn(),
                                          c.EvalConstant(datetime.date(2014, 1, 1)))),
                    # Aggregation node deep in the tree.
                    c.Sum([c.EvalConstant(1)])))))

    def test_has_nested_aggregrates(self):
        self.assertFalse(c.has_nested_aggregates(
            c.ChangeColumn()))

        self.assertFalse(c.has_nested_aggregates(
            c.Sum([c.ChangeColumn()])))

        self.assertFalse(c.has_nested_aggregates(
            c.EvalEqual(c.Sum([c.ChangeColumn()]), c.EvalConstant(1))))

        self.assertTrue(c.has_nested_aggregates(
            c.First([c.Sum([c.ChangeColumn()])])))

        self.assertTrue(c.has_nested_aggregates(
            c.First([c.EvalEqual(c.Sum([c.ChangeColumn()]), c.EvalConstant(1))])))

        self.assertTrue(c.has_nested_aggregates(
            c.EvalOr(c.First([c.EvalEqual(c.Sum([c.ChangeColumn()]), c.EvalConstant(1))]),
                     c.EvalConstant(False))))



class TestCompileDataTypes(unittest.TestCase):

    def test_compile_EvalConstant(self):
        c_int = c.EvalConstant(17)
        self.assertEqual(int, c_int.dtype)

        c_decimal = c.EvalConstant(D('7364.35'))
        self.assertEqual(Decimal, c_decimal.dtype)

        c_str = c.EvalConstant("Assets:Checking")
        self.assertEqual(str, c_str.dtype)

    def test_compile_EvalNot(self):
        c_not = c.EvalNot(c.EvalConstant(17))
        self.assertEqual(bool, c_not.dtype)

    def test_compile_EvalEqual(self):
        c_equal = c.EvalEqual(c.EvalConstant(17), c.EvalConstant(18))
        self.assertEqual(bool, c_equal.dtype)

    def test_compile_EvalMatch(self):
        with self.assertRaises(c.CompilationError):
            c.EvalMatch(c.EvalConstant('testing'), c.EvalConstant(18))
        c_equal = c.EvalMatch(c.EvalConstant('testing'), c.EvalConstant('test.*'))
        self.assertEqual(bool, c_equal.dtype)

    def test_compile_EvalAnd(self):
        c_and = c.EvalAnd(c.EvalConstant(17), c.EvalConstant(18))
        self.assertEqual(bool, c_and.dtype)

    def test_compile_EvalOr(self):
        c_or = c.EvalOr(c.EvalConstant(17), c.EvalConstant(18))
        self.assertEqual(bool, c_or.dtype)

    def test_compile_EvalLength(self):
        with self.assertRaises(c.CompilationError):
            c.Length([c.EvalConstant(17)])
        c_length = c.Length([c.EvalConstant('testing')])
        self.assertEqual(int, c_length.dtype)

    def test_compile_EvalYear(self):
        with self.assertRaises(c.CompilationError):
            c.Year([c.EvalConstant(17)])
        c_year = c.Year([c.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_year.dtype)

    def test_compile_EvalMonth(self):
        with self.assertRaises(c.CompilationError):
            c.Month([c.EvalConstant(17)])
        c_month = c.Month([c.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_month.dtype)

    def test_compile_EvalDay(self):
        with self.assertRaises(c.CompilationError):
            c.Day([c.EvalConstant(17)])
        c_day = c.Day([c.EvalConstant(datetime.date.today())])
        self.assertEqual(int, c_day.dtype)

    def test_compile_EvalUnits(self):
        with self.assertRaises(c.CompilationError):
            c.Units([c.EvalConstant(17)])
        c_units = c.Units([c.EvalConstant(inventory.Inventory())])
        self.assertEqual(inventory.Inventory, c_units.dtype)
        c_units = c.Units([c.EvalConstant(position.Position.from_string('100 USD'))])
        self.assertEqual(inventory.Inventory, c_units.dtype)

    def test_compile_EvalCost(self):
        with self.assertRaises(c.CompilationError):
            c.Cost([c.EvalConstant(17)])
        c_cost = c.Cost([c.EvalConstant(inventory.Inventory())])
        self.assertEqual(inventory.Inventory, c_cost.dtype)
        c_cost = c.Cost([c.EvalConstant(position.Position.from_string('100 USD'))])
        self.assertEqual(inventory.Inventory, c_cost.dtype)

    def test_compile_EvalSum(self):
        with self.assertRaises(c.CompilationError):
            c.Sum([c.EvalConstant('testing')])
        c_sum = c.Sum([c.EvalConstant(17)])
        self.assertEqual(int, c_sum.dtype)
        c_sum = c.Sum([c.EvalConstant(D('17.'))])
        self.assertEqual(Decimal, c_sum.dtype)

    def test_compile_EvalCount(self):
        c_count = c.Count([c.EvalConstant(17)])
        self.assertEqual(int, c_count.dtype)

    def test_compile_EvalFirst(self):
        c_first = c.First([c.EvalConstant(17.)])
        self.assertEqual(float, c_first.dtype)

    def test_compile_EvalLast(self):
        c_last = c.Last([c.EvalConstant(17.)])
        self.assertEqual(float, c_last.dtype)

    def test_compile_columns(self):
        class_types = [
            # Postings accessors.
            (c.TypeColumn, str),
            (c.FilenameColumn, str),
            (c.LineNoColumn, int),
            (c.DateColumn, datetime.date),
            (c.FlagColumn, str),
            (c.PayeeColumn, str),
            (c.NarrationColumn, str),
            (c.TagsColumn, set),
            (c.LinksColumn, set),
            (c.AccountColumn, str),
            (c.NumberColumn, Decimal),
            (c.CurrencyColumn, str),
            (c.ChangeColumn, position.Position),
            # Entries accessors.
            (c.TypeEntryColumn, str),
            (c.FilenameEntryColumn, str),
            (c.LineNoEntryColumn, int),
            (c.DateEntryColumn, datetime.date),
            (c.FlagEntryColumn, str),
            (c.PayeeEntryColumn, str),
            (c.NarrationEntryColumn, str),
            (c.TagsEntryColumn, set),
            (c.LinksEntryColumn, set),
            ]
        for cls, dtype in class_types:
            instance = cls()
            self.assertEqual(dtype, instance.dtype)
