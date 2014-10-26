import datetime
import re
import unittest

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.core import position
from beancount.query import query_parser as q
from beancount.query import query_compile as c
from beancount.query import query_env as cc
from beancount.utils.misc_utils import box


class TestCompileExpression(unittest.TestCase):

    def test_expr_invalid(self):
        with self.assertRaises(c.CompilationError):
            c.compile_expression(q.Column('invalid'), cc.TargetsEnvironment())

    def test_expr_column(self):
        self.assertEqual(cc.FilenameColumn(),
                         c.compile_expression(q.Column('filename'), cc.TargetsEnvironment()))

    def test_expr_function(self):
        self.assertEqual(cc.SumPosition([cc.ChangeColumn()]),
                         c.compile_expression(q.Function('sum', [q.Column('change')]),
                                              cc.TargetsEnvironment()))

    def test_expr_unaryop(self):
        self.assertEqual(c.EvalNot(cc.AccountColumn()),
                         c.compile_expression(q.Not(q.Column('account')),
                                              cc.TargetsEnvironment()))

    def test_expr_binaryop(self):
        self.assertEqual(c.EvalEqual(cc.DateColumn(),
                                     c.EvalConstant(datetime.date(2014, 1, 1))),
                         c.compile_expression(
                             q.Equal(q.Column('date'),
                                     q.Constant(datetime.date(2014, 1, 1))),
                             cc.TargetsEnvironment()))

    def test_expr_constant(self):
        self.assertEqual(c.EvalConstant(D(17)),
                         c.compile_expression(q.Constant(D(17)), cc.TargetsEnvironment()))


class TestCompileExpressionDataTypes(unittest.TestCase):

    def test_expr_function_arity(self):
        # Compile with the correct number of arguments.
        c.compile_expression(q.Function('sum', [q.Column('number')]),
                             cc.TargetsEnvironment())

        # Compile with an incorrect number of arguments.
        with self.assertRaises(c.CompilationError):
            c.compile_expression(q.Function('sum', [q.Column('date'),
                                                    q.Column('account')]),
                                 cc.TargetsEnvironment())


class TestCompileAggregateChecks(unittest.TestCase):

    def test_is_aggregrate_derived(self):
        columns, aggregates = c.get_columns_and_aggregates(
            c.EvalAnd(
                c.EvalEqual(cc.ChangeColumn(), c.EvalConstant(42)),
                c.EvalOr(
                    c.EvalNot(c.EvalEqual(cc.DateColumn(),
                                          c.EvalConstant(datetime.date(2014, 1, 1)))),
                    c.EvalConstant(False))))
        self.assertEqual((2, 0), (len(columns), len(aggregates)))

        columns, aggregates = c.get_columns_and_aggregates(
            c.EvalAnd(
                c.EvalEqual(cc.ChangeColumn(), c.EvalConstant(42)),
                c.EvalOr(
                    c.EvalNot(c.EvalEqual(cc.DateColumn(),
                                          c.EvalConstant(datetime.date(2014, 1, 1)))),
                    # Aggregation node deep in the tree.
                    cc.Sum([c.EvalConstant(1)]))))
        self.assertEqual((2, 1), (len(columns), len(aggregates)))

    def test_get_columns_and_aggregates(self):
        # Simple column.
        c_query = cc.ChangeColumn()
        columns, aggregates = c.get_columns_and_aggregates(c_query)
        self.assertEqual((1, 0), (len(columns), len(aggregates)))
        self.assertFalse(c.is_aggregate(c_query))

        # Multiple columns.
        c_query = c.EvalAnd(cc.ChangeColumn(), cc.DateColumn())
        columns, aggregates = c.get_columns_and_aggregates(c_query)
        self.assertEqual((2, 0), (len(columns), len(aggregates)))
        self.assertFalse(c.is_aggregate(c_query))

        # Simple aggregate.
        c_query = cc.SumPosition([cc.ChangeColumn()])
        columns, aggregates = c.get_columns_and_aggregates(c_query)
        self.assertEqual((0, 1), (len(columns), len(aggregates)))
        self.assertTrue(c.is_aggregate(c_query))

        # Multiple aggregates.
        c_query = c.EvalAnd(cc.First([cc.AccountColumn()]), cc.Last([cc.AccountColumn()]))
        columns, aggregates = c.get_columns_and_aggregates(c_query)
        self.assertEqual((0, 2), (len(columns), len(aggregates)))
        self.assertTrue(c.is_aggregate(c_query))

        # Simple non-aggregate function.
        c_query = cc.Length([cc.AccountColumn()])
        columns, aggregates = c.get_columns_and_aggregates(c_query)
        self.assertEqual((1, 0), (len(columns), len(aggregates)))
        self.assertFalse(c.is_aggregate(c_query))

        # Mix of column and aggregates (this is used to detect this illegal case).
        c_query = c.EvalAnd(cc.Length([cc.AccountColumn()]),
                            cc.SumPosition([cc.ChangeColumn()]))
        columns, aggregates = c.get_columns_and_aggregates(c_query)
        self.assertEqual((1, 1), (len(columns), len(aggregates)))
        self.assertTrue(c.is_aggregate(c_query))


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

    def test_compile_EvalGreater(self):
        c_gt = c.EvalGreater(c.EvalConstant(17), c.EvalConstant(18))
        self.assertEqual(bool, c_gt.dtype)

    def test_compile_EvalGreaterEq(self):
        c_ge = c.EvalGreaterEq(c.EvalConstant(17), c.EvalConstant(18))
        self.assertEqual(bool, c_ge.dtype)

    def test_compile_EvalLess(self):
        c_lt = c.EvalLess(c.EvalConstant(17), c.EvalConstant(18))
        self.assertEqual(bool, c_lt.dtype)

    def test_compile_EvalLessEq(self):
        c_le = c.EvalLessEq(c.EvalConstant(17), c.EvalConstant(18))
        self.assertEqual(bool, c_le.dtype)

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


class TestCompileMisc(unittest.TestCase):

    def test_find_unique_names(self):
        self.assertEqual('date', c.find_unique_name('date', {}))
        self.assertEqual('date', c.find_unique_name('date', {'account', 'number'}))
        self.assertEqual('date_1', c.find_unique_name('date', {'date', 'number'}))
        self.assertEqual('date_2', c.find_unique_name('date', {'date', 'date_1', 'date_3'}))


class CompileSelectBase(unittest.TestCase):

    maxDiff = 8192

    # Default execution contexts.
    xcontext_entries = cc.FilterEntriesEnvironment()
    xcontext_targets = cc.TargetsEnvironment()
    xcontext_postings = cc.FilterPostingsEnvironment()

    def setUp(self):
        self.parser = q.Parser()

    def compile(self, query):
        """Parse one query and compile it.

        Args:
          query: An SQL query to be parsed.
        Returns:
          The AST.
        """
        select = self.parser.parse(query.strip())
        query = c.compile_select(select,
                                 self.xcontext_targets,
                                 self.xcontext_postings,
                                 self.xcontext_entries)
        self.assertInvariants(query)
        return query

    def assertInvariants(self, query):
        """Assert the invariants on the query.

        Args:
          query: An instance of EvalQuery, a compiled query statement.
        Raises:
          AssertionError: if the check fails.
        """
        # Check that the group references cover all the simple indexes.
        if query.group_indexes is not None:
            non_aggregate_indexes = [index
                                     for index, c_target in enumerate(query.c_targets)
                                     if not c.is_aggregate(c_target.c_expr)]

            self.assertEqual(set(non_aggregate_indexes), set(query.group_indexes),
                             "Invalid indexes: {}".format(query))

    def assertIndexes(self,
                      query,
                      expected_simple_indexes,
                      expected_aggregate_indexes,
                      expected_group_indexes,
                      expected_order_indexes):
        """Check the four lists of indexes for comparison.

        Args:
          query: An instance of EvalQuery, a compiled query statement.
          expected_simple_indexes: The expected visible non-aggregate indexes.
          expected_aggregate_indexes: The expected visible aggregate indexes.
          expected_group_indexes: The expected group_indexes.
          expected_order_indexes: The expected order_indexes.
        Raises:
          AssertionError: if the check fails.
        """
        # Compute the list of _visible_ aggregates and non-aggregates.
        simple_indexes = [index
                          for index, c_target in enumerate(query.c_targets)
                          if c_target.name and not c.is_aggregate(c_target.expression)]
        aggregate_indexes = [index
                             for index, c_target in enumerate(query.c_targets)
                             if c_target.name and c.is_aggregate(c_target.expression)]

        self.assertEqual(set(expected_simple_indexes), set(simple_indexes))

        self.assertEqual(set(expected_aggregate_indexes), set(aggregate_indexes))

        self.assertEqual(
            set(expected_group_indexes) if expected_group_indexes is not None else None,
            set(query.group_indexes) if query.group_indexes is not None else None)

        self.assertEqual(
            set(expected_order_indexes) if expected_order_indexes is not None else None,
            set(query.order_indexes) if query.order_indexes is not None else None)

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


class TestCompileSelect(CompileSelectBase):

    def test_compile_from(self):
        # Test the compilation of from.
        query = self.compile("SELECT account;")
        self.assertEqual(None, query.c_from)

        query = self.compile("SELECT account FROM CLOSE;")
        self.assertEqual(c.EvalFrom(None, None, True, None), query.c_from)

        query = self.compile("SELECT account FROM length(payee) != 0;")
        self.assertTrue(isinstance(query.c_from, c.EvalFrom))
        self.assertTrue(isinstance(query.c_from.c_expr, c.EvalNode))

        with self.assertRaises(c.CompilationError):
            query = self.compile("SELECT account FROM sum(payee) != 0;")

    def test_compile_from_invalid_dates(self):
        query = self.compile("""
          SELECT account FROM  OPEN ON 2014-03-01  CLOSE ON 2014-03-02;
        """)

        query = self.compile("""
          SELECT account FROM  OPEN ON 2014-03-02  CLOSE ON 2014-03-02;
        """)

        with self.assertRaises(c.CompilationError):
            query = self.compile("""
              SELECT account FROM  OPEN ON 2014-03-03  CLOSE ON 2014-03-02;
            """)

    def test_compile_targets_wildcard(self):
        # Test the wildcard expandion.
        query = self.compile("SELECT *;")
        self.assertTrue(list, type(query.c_targets))
        self.assertGreater(len(query.c_targets), 3)
        self.assertTrue(all(isinstance(target.c_expr, c.EvalColumn)
                            for target in query.c_targets))

    def test_compile_targets_named(self):
        # Test the wildcard expandion.
        query = self.compile("SELECT length(account), account as a, date;")
        self.assertEqual(
            [c.EvalTarget(cc.Length([cc.AccountColumn()]), 'length_account', False),
             c.EvalTarget(cc.AccountColumn(), 'a', False),
             c.EvalTarget(cc.DateColumn(), 'date', False)],
            query.c_targets)

    def test_compile_mixed_aggregates(self):
        # Check mixed aggregates and non-aggregates in a target.
        with self.assertRaises(c.CompilationError) as assertion:
            self.compile("""
              SELECT length(account) and sum(length(account));
            """)
        self.assertTrue(re.search('Mixed aggregates and non-aggregates',
                                  str(assertion.exception)))

    def test_compile_aggregates_of_aggregates(self):
        # Check mixed aggregates and non-aggregates in a target.
        with self.assertRaises(c.CompilationError) as assertion:
            self.compile("""
              SELECT sum(sum(length(account)));
            """)
        self.assertTrue(re.search('Aggregates of aggregates',
                                  str(assertion.exception)))

    def test_compile_having(self):
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT account, sum(number) GROUP BY account HAVING sum(number) > 0;
            """)


class TestCompileSelectGroupBy(CompileSelectBase):

    def test_compile_group_by_non_aggregates(self):
        self.compile("""
          SELECT payee GROUP BY payee, length(account);
        """)

        with self.assertRaises(c.CompilationError) as assertion:
            self.compile("""
              SELECT payee GROUP BY payee, last(account);
            """)
        self.assertTrue(re.search('may not be aggregates',
                                  str(assertion.exception)))

    def test_compile_group_by_reference_by_name(self):
        # Valid references to target names.
        self.compile("""
          SELECT payee, last(account) GROUP BY payee;
        """)
        self.compile("""
          SELECT payee as a, last(account) as len GROUP BY a;
        """)

        # References to non-targets have to be valid.
        self.compile("""
          SELECT payee, last(account) as len GROUP BY payee, date;
        """)

        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT payee, last(account) as len GROUP BY something;
            """)

    def test_compile_group_by_reference_by_number(self):
        self.compile("""
          SELECT date, payee, narration GROUP BY 1, 2, 3;
        """)

        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT date, payee, narration GROUP BY 1, 2, 3, 4;
            """)

    def test_compile_group_by_reference_an_aggregate(self):
        # By name.
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT payee, last(account) as last GROUP BY last;
            """)
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT account, sum(number) as sum_num GROUP BY account, sum_num;
            """)

        # By number.
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT payee, last(account) as last GROUP BY 2;
            """)

        # Explicit aggregate in group-by clause.
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT account, sum(number) GROUP BY account, sum(number);
            """)

    def test_compile_group_by_implicit(self):
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT payee, last(account);
            """)

        self.compile("""
          SELECT first(account), last(account);
        """)

    def test_compile_group_by_coverage(self):
        # Non-aggregates.
        query = self.compile("SELECT account, length(account);")
        self.assertEqual(None, query.group_indexes)
        self.assertEqual(None, query.order_indexes)

        # Aggregates only.
        query = self.compile("SELECT first(account), last(account);")
        self.assertEqual([], query.group_indexes)

        # Mixed with non-aggregates in group-by clause.
        query = self.compile("SELECT account, sum(number) GROUP BY account;")
        self.assertEqual([0], query.group_indexes)

        # Mixed with non-aggregates in group-by clause with non-aggregates a
        # strict subset of the group-by columns. 'account' is a subset of
        # {'account', 'flag'}.
        query = self.compile("""
          SELECT account, sum(number) GROUP BY account, flag;
        """)
        self.assertEqual([0, 2], query.group_indexes)

        # Non-aggregates not covered by group-by clause.
        with self.assertRaises(c.CompilationError) as a:
            self.compile("""
              SELECT account, date, sum(number) GROUP BY account;
            """)
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT payee, last(account) as len GROUP BY date;
            """)

        # Non-aggregates not covered by group-by clause, and no aggregates in
        # the list of targets.
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT date, flag, account, number GROUP BY date, flag;
            """)

        # All non-aggregates and matching list of aggregates (this is a
        # pointless list of aggregates, essentially).
        query = self.compile("""
          SELECT date, flag, account GROUP BY date, flag, account;
        """)
        self.assertEqual([0, 1, 2], query.group_indexes)

    def test_compile_group_by_reconcile(self):
        # Check that no invisible column is created if redundant.
        query = self.compile("""
          SELECT account, length(account), sum(number)
          GROUP BY account, length(account);
        """)
        self.assertEqual([0, 1], query.group_indexes)


class TestCompileSelectOrderBy(CompileSelectBase):

    def test_compile_order_by_simple(self):
        query = self.compile("""
          SELECT account, sum(number) GROUP BY account ORDER BY account;
        """)
        self.assertEqual([0], query.group_indexes)
        self.assertEqual([0], query.order_indexes)

    def test_compile_order_by_simple(self):
        query = self.compile("""
          SELECT account, length(narration) GROUP BY account, 2 ORDER BY 1, 2;
        """)
        self.assertEqual([0, 1], query.group_indexes)
        self.assertEqual([0, 1], query.order_indexes)

        query = self.compile("""
          SELECT account, length(narration) as l GROUP BY account, l ORDER BY l;
        """)
        self.assertEqual([0, 1], query.group_indexes)
        self.assertEqual([1], query.order_indexes)

    def test_compile_order_by_create_non_agg(self):
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT account, last(narration) GROUP BY account ORDER BY year(date);
            """)

        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT account GROUP BY account ORDER BY year(date);
            """)

        query = self.compile("""
          SELECT account, year(date) GROUP BY 1, 2 ORDER BY 2;
        """)
        self.assertEqual([0, 1], query.group_indexes)
        self.assertEqual([1], query.order_indexes)

        # We don't detect similarity between order-by and targets yet.
        # This could eventually be improved.
        with self.assertRaises(c.CompilationError):
            self.compile("""
              SELECT account, year(date) GROUP BY 1, 2 ORDER BY year(date);
            """)

    def test_compile_order_by_reconcile(self):
        # Check that no invisible column is created if redundant.
        query = self.compile("""
          SELECT account, length(account)
          ORDER BY length(account);
        """)
        self.assertEqual([1], query.order_indexes)

    def test_compile_order_by_reference_invisible(self):
        # So this is an interesting case: the grouping expression is an
        # invisible non-aggregate (length(account)) and the ordering expression
        # refers to the same non-aggregate expression. If they are reconciled to
        # the same invisible expression, the condition that the grouping
        # expressions cover all the non-aggregates is fulfilled. Otherwise, it
        # would fail. In order to support the compilation of this, we must
        # reconcile the grouping and ordering columns by comparing their values.
        query = self.compile("""
          SELECT count(account) as num, first(account) as first
          GROUP BY length(account)
          ORDER BY length(account);
        """)
        self.assertEqual([2], query.group_indexes)
        self.assertEqual([2], query.order_indexes)

    def test_compile_order_by_aggregate(self):
        query = self.compile("""
          SELECT account, first(narration) GROUP BY account ORDER BY 2;
        """)
        self.assertEqual([0], query.group_indexes)
        self.assertEqual([1], query.order_indexes)

        query = self.compile("""
          SELECT account, first(narration) as f GROUP BY account ORDER BY f;
        """)
        self.assertEqual([0], query.group_indexes)
        self.assertEqual([1], query.order_indexes)

        query = self.compile("""
          SELECT account, first(narration) GROUP BY account ORDER BY sum(number);
        """)
        self.assertEqual([0], query.group_indexes)
        self.assertEqual([2], query.order_indexes)

        query = self.compile("""
          SELECT account GROUP BY account ORDER BY sum(number);
        """)
        self.assertEqual([0], query.group_indexes)
        self.assertEqual([1], query.order_indexes)
