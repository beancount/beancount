__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest
from decimal import Decimal

from beancount.core.number import D
from beancount.query import query_parser as qp
from beancount.query import query_compile as qc
from beancount.query import query_env as qe


class TestCompileExpression(unittest.TestCase):

    def test_expr_invalid(self):
        with self.assertRaises(qc.CompilationError):
            qc.compile_expression(qp.Column('invalid'), qe.TargetsEnvironment())

    def test_expr_column(self):
        self.assertEqual(qe.FilenameColumn(),
                         qc.compile_expression(qp.Column('filename'),
                                               qe.TargetsEnvironment()))

    def test_expr_function(self):
        self.assertEqual(qe.SumPosition([qe.PositionColumn()]),
                         qc.compile_expression(qp.Function('sum', [qp.Column('position')]),
                                               qe.TargetsEnvironment()))

    def test_expr_unaryop(self):
        self.assertEqual(qc.EvalNot(qe.AccountColumn()),
                         qc.compile_expression(qp.Not(qp.Column('account')),
                                               qe.TargetsEnvironment()))

    def test_expr_binaryop(self):
        self.assertEqual(qc.EvalEqual(qe.DateColumn(),
                                      qc.EvalConstant(datetime.date(2014, 1, 1))),
                         qc.compile_expression(
                             qp.Equal(qp.Column('date'),
                                      qp.Constant(datetime.date(2014, 1, 1))),
                             qe.TargetsEnvironment()))

    def test_expr_constant(self):
        self.assertEqual(qc.EvalConstant(D(17)),
                         qc.compile_expression(qp.Constant(D(17)), qe.TargetsEnvironment()))


class TestCompileExpressionDataTypes(unittest.TestCase):

    def test_expr_function_arity(self):
        # Compile with the correct number of arguments.
        qc.compile_expression(qp.Function('sum', [qp.Column('number')]),
                              qe.TargetsEnvironment())

        # Compile with an incorrect number of arguments.
        with self.assertRaises(qc.CompilationError):
            qc.compile_expression(qp.Function('sum', [qp.Column('date'),
                                                      qp.Column('account')]),
                                  qe.TargetsEnvironment())


class TestCompileAggregateChecks(unittest.TestCase):

    def test_is_aggregate_derived(self):
        columns, aggregates = qc.get_columns_and_aggregates(
            qc.EvalAnd(
                qc.EvalEqual(qe.PositionColumn(), qc.EvalConstant(42)),
                qc.EvalOr(
                    qc.EvalNot(qc.EvalEqual(qe.DateColumn(),
                                            qc.EvalConstant(datetime.date(2014, 1, 1)))),
                    qc.EvalConstant(False))))
        self.assertEqual((2, 0), (len(columns), len(aggregates)))

        columns, aggregates = qc.get_columns_and_aggregates(
            qc.EvalAnd(
                qc.EvalEqual(qe.PositionColumn(), qc.EvalConstant(42)),
                qc.EvalOr(
                    qc.EvalNot(qc.EvalEqual(qe.DateColumn(),
                                            qc.EvalConstant(datetime.date(2014, 1, 1)))),
                    # Aggregation node deep in the tree.
                    qe.Sum([qc.EvalConstant(1)]))))
        self.assertEqual((2, 1), (len(columns), len(aggregates)))

    def test_get_columns_and_aggregates(self):
        # Simple column.
        c_query = qe.PositionColumn()
        columns, aggregates = qc.get_columns_and_aggregates(c_query)
        self.assertEqual((1, 0), (len(columns), len(aggregates)))
        self.assertFalse(qc.is_aggregate(c_query))

        # Multiple columns.
        c_query = qc.EvalAnd(qe.PositionColumn(), qe.DateColumn())
        columns, aggregates = qc.get_columns_and_aggregates(c_query)
        self.assertEqual((2, 0), (len(columns), len(aggregates)))
        self.assertFalse(qc.is_aggregate(c_query))

        # Simple aggregate.
        c_query = qe.SumPosition([qe.PositionColumn()])
        columns, aggregates = qc.get_columns_and_aggregates(c_query)
        self.assertEqual((0, 1), (len(columns), len(aggregates)))
        self.assertTrue(qc.is_aggregate(c_query))

        # Multiple aggregates.
        c_query = qc.EvalAnd(qe.First([qe.AccountColumn()]), qe.Last([qe.AccountColumn()]))
        columns, aggregates = qc.get_columns_and_aggregates(c_query)
        self.assertEqual((0, 2), (len(columns), len(aggregates)))
        self.assertTrue(qc.is_aggregate(c_query))

        # Simple non-aggregate function.
        c_query = qe.Length([qe.AccountColumn()])
        columns, aggregates = qc.get_columns_and_aggregates(c_query)
        self.assertEqual((1, 0), (len(columns), len(aggregates)))
        self.assertFalse(qc.is_aggregate(c_query))

        # Mix of column and aggregates (this is used to detect this illegal case).
        c_query = qc.EvalAnd(qe.Length([qe.AccountColumn()]),
                             qe.SumPosition([qe.PositionColumn()]))
        columns, aggregates = qc.get_columns_and_aggregates(c_query)
        self.assertEqual((1, 1), (len(columns), len(aggregates)))
        self.assertTrue(qc.is_aggregate(c_query))


class TestCompileDataTypes(unittest.TestCase):

    def test_compile_EvalConstant(self):
        c_int = qc.EvalConstant(17)
        self.assertEqual(int, c_int.dtype)

        c_decimal = qc.EvalConstant(D('7364.35'))
        self.assertEqual(Decimal, c_decimal.dtype)

        c_str = qc.EvalConstant("Assets:Checking")
        self.assertEqual(str, c_str.dtype)

    def test_compile_EvalNot(self):
        c_not = qc.EvalNot(qc.EvalConstant(17))
        self.assertEqual(bool, c_not.dtype)

    def test_compile_EvalEqual(self):
        c_equal = qc.EvalEqual(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(bool, c_equal.dtype)

    def test_compile_EvalGreater(self):
        c_gt = qc.EvalGreater(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(bool, c_gt.dtype)

    def test_compile_EvalGreaterEq(self):
        c_ge = qc.EvalGreaterEq(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(bool, c_ge.dtype)

    def test_compile_EvalLess(self):
        c_lt = qc.EvalLess(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(bool, c_lt.dtype)

    def test_compile_EvalLessEq(self):
        c_le = qc.EvalLessEq(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(bool, c_le.dtype)

    def test_compile_EvalMatch(self):
        with self.assertRaises(qc.CompilationError):
            qc.EvalMatch(qc.EvalConstant('testing'), qc.EvalConstant(18))
        c_equal = qc.EvalMatch(qc.EvalConstant('testing'), qc.EvalConstant('test.*'))
        self.assertEqual(bool, c_equal.dtype)

    def test_compile_EvalAnd(self):
        c_and = qc.EvalAnd(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(bool, c_and.dtype)

    def test_compile_EvalOr(self):
        c_or = qc.EvalOr(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(bool, c_or.dtype)

    def test_compile_EvalMul(self):
        c_plus = qc.EvalMul(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(Decimal, c_plus.dtype)

    def test_compile_EvalDiv(self):
        c_plus = qc.EvalDiv(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(Decimal, c_plus.dtype)

    def test_compile_EvalAdd(self):
        c_plus = qc.EvalAdd(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(Decimal, c_plus.dtype)

    def test_compile_EvalSub(self):
        c_plus = qc.EvalSub(qc.EvalConstant(17), qc.EvalConstant(18))
        self.assertEqual(Decimal, c_plus.dtype)


class TestCompileMisc(unittest.TestCase):

    def test_find_unique_names(self):
        self.assertEqual('date', qc.find_unique_name('date', {}))
        self.assertEqual('date', qc.find_unique_name('date', {'account', 'number'}))
        self.assertEqual('date_1', qc.find_unique_name('date', {'date', 'number'}))
        self.assertEqual('date_2',
                         qc.find_unique_name('date', {'date', 'date_1', 'date_3'}))


class CompileSelectBase(unittest.TestCase):

    maxDiff = 8192

    # Default execution contexts.
    xcontext_entries = qe.FilterEntriesEnvironment()
    xcontext_targets = qe.TargetsEnvironment()
    xcontext_postings = qe.FilterPostingsEnvironment()

    def setUp(self):
        self.parser = qp.Parser()

    def parse(self, query):
        return self.parser.parse(query.strip())

    def compile(self, query):
        """Parse one query and compile it.

        Args:
          query: An SQL query to be parsed.
        Returns:
          The AST.
        """
        statement = self.parse(query)
        c_query = qc.compile(statement,
                             self.xcontext_targets,
                             self.xcontext_postings,
                             self.xcontext_entries)
        if isinstance(c_query, qp.Select):
            self.assertSelectInvariants(c_query)
        return c_query

    def assertSelectInvariants(self, query):
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
                                     if not qc.is_aggregate(c_target.c_expr)]

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
                          if c_target.name and not qc.is_aggregate(c_target.expression)]
        aggregate_indexes = [index
                             for index, c_target in enumerate(query.c_targets)
                             if c_target.name and qc.is_aggregate(c_target.expression)]

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
            print()
            print("Expected: {}".format(expected))
            print("Actual  : {}".format(actual))
            raise


class TestCompileSelect(CompileSelectBase):

    def test_compile_from(self):
        # Test the compilation of from.
        query = self.compile("SELECT account;")
        self.assertEqual(None, query.c_from)

        query = self.compile("SELECT account FROM CLOSE;")
        self.assertEqual(qc.EvalFrom(None, None, True, None), query.c_from)

        query = self.compile("SELECT account FROM length(payee) != 0;")
        self.assertTrue(isinstance(query.c_from, qc.EvalFrom))
        self.assertTrue(isinstance(query.c_from.c_expr, qc.EvalNode))

        with self.assertRaises(qc.CompilationError):
            query = self.compile("SELECT account FROM sum(payee) != 0;")

    def test_compile_from_invalid_dates(self):
        self.compile("""
          SELECT account FROM  OPEN ON 2014-03-01  CLOSE ON 2014-03-02;
        """)

        self.compile("""
          SELECT account FROM  OPEN ON 2014-03-02  CLOSE ON 2014-03-02;
        """)

        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT account FROM  OPEN ON 2014-03-03  CLOSE ON 2014-03-02;
            """)

    def test_compile_targets_wildcard(self):
        # Test the wildcard expansion.
        query = self.compile("SELECT *;")
        self.assertTrue(list, type(query.c_targets))
        self.assertGreater(len(query.c_targets), 3)
        self.assertTrue(all(isinstance(target.c_expr, qc.EvalColumn)
                            for target in query.c_targets))

    def test_compile_targets_named(self):
        # Test the wildcard expansion.
        query = self.compile("SELECT length(account), account as a, date;")
        self.assertEqual(
            [qc.EvalTarget(qe.Length([qe.AccountColumn()]), 'length_account', False),
             qc.EvalTarget(qe.AccountColumn(), 'a', False),
             qc.EvalTarget(qe.DateColumn(), 'date', False)],
            query.c_targets)

    def test_compile_mixed_aggregates(self):
        # Check mixed aggregates and non-aggregates in a target.
        with self.assertRaises(qc.CompilationError) as assertion:
            self.compile("""
              SELECT length(account) and sum(length(account));
            """)
        self.assertRegex(str(assertion.exception), 'Mixed aggregates and non-aggregates')

    def test_compile_aggregates_of_aggregates(self):
        # Check mixed aggregates and non-aggregates in a target.
        with self.assertRaises(qc.CompilationError) as assertion:
            self.compile("""
              SELECT sum(sum(length(account)));
            """)
        self.assertRegex(str(assertion.exception), 'Aggregates of aggregates')

    def test_compile_having(self):
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT account, sum(number) GROUP BY account HAVING sum(number) > 0;
            """)

    def test_compile_group_by_inventory(self):
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT sum(number), balance GROUP BY balance;
            """)


class TestCompileSelectGroupBy(CompileSelectBase):

    def test_compile_group_by_non_aggregates(self):
        self.compile("""
          SELECT payee GROUP BY payee, length(account);
        """)

        with self.assertRaises(qc.CompilationError) as assertion:
            self.compile("""
              SELECT payee GROUP BY payee, last(account);
            """)
        self.assertRegex(str(assertion.exception), 'may not be aggregates')

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

        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT payee, last(account) as len GROUP BY something;
            """)

    def test_compile_group_by_reference_by_number(self):
        self.compile("""
          SELECT date, payee, narration GROUP BY 1, 2, 3;
        """)

        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT date, payee, narration GROUP BY 1, 2, 3, 4;
            """)

    def test_compile_group_by_reference_an_aggregate(self):
        # By name.
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT payee, last(account) as last GROUP BY last;
            """)
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT account, sum(number) as sum_num GROUP BY account, sum_num;
            """)

        # By number.
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT payee, last(account) as last GROUP BY 2;
            """)

        # Explicit aggregate in group-by clause.
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT account, sum(number) GROUP BY account, sum(number);
            """)

    def test_compile_group_by_implicit(self):
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
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT account, date, sum(number) GROUP BY account;
            """)
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT payee, last(account) as len GROUP BY date;
            """)

        # Non-aggregates not covered by group-by clause, and no aggregates in
        # the list of targets.
        with self.assertRaises(qc.CompilationError):
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

    def test_compile_order_by_simple_2(self):
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
        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT account, last(narration) GROUP BY account ORDER BY year(date);
            """)

        with self.assertRaises(qc.CompilationError):
            self.compile("""
              SELECT account GROUP BY account ORDER BY year(date);
            """)

        query = self.compile("""
          SELECT account, year(date) GROUP BY 1, 2 ORDER BY 2;
        """)
        self.assertEqual([0, 1], query.group_indexes)
        self.assertEqual([1], query.order_indexes)

        # We detect similarity between order-by and targets yet.
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


class TestTranslationJournal(CompileSelectBase):

    maxDiff = 4096

    def test_journal(self):
        journal = self.parse("JOURNAL;")
        select = qc.transform_journal(journal)
        self.assertEqual(
            qp.Select([
                qp.Target(qp.Column('date'), None),
                qp.Target(qp.Column('flag'), None),
                qp.Target(qp.Function('maxwidth', [qp.Column('payee'),
                                                   qp.Constant(48)]), None),
                qp.Target(qp.Function('maxwidth', [qp.Column('narration'),
                                                   qp.Constant(80)]), None),
                qp.Target(qp.Column('account'), None),
                qp.Target(qp.Column('position'), None),
                qp.Target(qp.Column('balance'), None),
            ], None, None, None, None, None, None, None, None),
            select)

    def test_journal_with_account(self):
        journal = self.parse("JOURNAL 'liabilities';")
        select = qc.transform_journal(journal)
        self.assertEqual(
            qp.Select([
                qp.Target(qp.Column('date'), None),
                qp.Target(qp.Column('flag'), None),
                qp.Target(qp.Function('maxwidth', [qp.Column('payee'),
                                                   qp.Constant(48)]), None),
                qp.Target(qp.Function('maxwidth', [qp.Column('narration'),
                                                   qp.Constant(80)]), None),
                qp.Target(qp.Column('account'), None),
                qp.Target(qp.Column('position'), None),
                qp.Target(qp.Column('balance'), None),
            ],
                      None,
                      qp.Match(qp.Column('account'), qp.Constant('liabilities')),
                      None, None, None, None, None, None),
            select)

    def test_journal_with_account_and_from(self):
        journal = self.parse("JOURNAL 'liabilities' FROM year = 2014;")
        select = qc.transform_journal(journal)
        self.assertEqual(
            qp.Select([
                qp.Target(qp.Column('date'), None),
                qp.Target(qp.Column('flag'), None),
                qp.Target(qp.Function('maxwidth', [qp.Column('payee'),
                                                   qp.Constant(48)]), None),
                qp.Target(qp.Function('maxwidth', [qp.Column('narration'),
                                                   qp.Constant(80)]), None),
                qp.Target(qp.Column('account'), None),
                qp.Target(qp.Column('position'), None),
                qp.Target(qp.Column('balance'), None),
            ],
                      qp.From(qp.Equal(qp.Column('year'), qp.Constant(2014)),
                              None, None, None),
                      qp.Match(qp.Column('account'), qp.Constant('liabilities')),
                      None, None, None, None, None, None),
            select)

    def test_journal_with_account_func_and_from(self):
        journal = self.parse("JOURNAL 'liabilities' AT cost FROM year = 2014;")
        select = qc.transform_journal(journal)
        self.assertEqual(
            qp.Select([
                qp.Target(qp.Column('date'), None),
                qp.Target(qp.Column('flag'), None),
                qp.Target(qp.Function('maxwidth', [qp.Column('payee'),
                                                   qp.Constant(48)]), None),
                qp.Target(qp.Function('maxwidth', [qp.Column('narration'),
                                                   qp.Constant(80)]), None),
                qp.Target(qp.Column('account'), None),
                qp.Target(qp.Function('cost', [qp.Column('position')]), None),
                qp.Target(qp.Function('cost', [qp.Column('balance')]), None),
            ],
                      qp.From(qp.Equal(qp.Column('year'), qp.Constant(2014)),
                              None, None, None),
                      qp.Match(qp.Column('account'), qp.Constant('liabilities')),
                      None, None, None, None, None, None),
            select)


class TestTranslationBalance(CompileSelectBase):

    group_by = qp.GroupBy([qp.Column('account'),
                           qp.Function('account_sortkey', [qp.Column(name='account')])],
                          None)

    order_by = qp.OrderBy([qp.Function('account_sortkey', [qp.Column('account')])], None)

    def test_balance(self):
        balance = self.parse("BALANCES;")
        select = qc.transform_balances(balance)
        self.assertEqual(
            qp.Select([
                qp.Target(qp.Column('account'), None),
                qp.Target(qp.Function('sum', [qp.Column('position')]), None),
                ], None, None, self.group_by, self.order_by,
                      None, None, None, None),
            select)

    def test_balance_with_units(self):
        balance = self.parse("BALANCES AT cost;")
        select = qc.transform_balances(balance)
        self.assertEqual(
            qp.Select([
                qp.Target(qp.Column('account'), None),
                qp.Target(qp.Function('sum',
                                      [qp.Function('cost',
                                                   [qp.Column('position')])]), None)],
                      None, None, self.group_by, self.order_by,
                      None, None, None, None),
            select)

    def test_balance_with_units_and_from(self):
        balance = self.parse("BALANCES AT cost FROM year = 2014;")
        select = qc.transform_balances(balance)
        self.assertEqual(
            qp.Select([
                qp.Target(qp.Column('account'), None),
                qp.Target(qp.Function('sum', [qp.Function('cost',
                                                          [qp.Column('position')])]), None),
                ],
                      qp.From(qp.Equal(qp.Column('year'), qp.Constant(2014)),
                              None, None, None),
                      None,
                      self.group_by,
                      self.order_by,
                      None, None, None, None),
            select)


class TestCompilePrint(CompileSelectBase):

    def test_print(self):
        self.assertCompile(qc.EvalPrint(None), "PRINT;")

    def test_print_from(self):
        self.assertCompile(qc.EvalPrint(
            qc.EvalFrom(qc.EvalEqual(qe.YearEntryColumn(), qc.EvalConstant(2014)),
                        None, None, None)
            ), "PRINT FROM year = 2014;")


if __name__ == '__main__':
    unittest.main()
