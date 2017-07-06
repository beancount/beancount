__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount.core.number import D
from beancount.query import query_parser as qp


def qSelect(target_spec=None,
            from_clause=None, where_clause=None,
            group_by=None, order_by=None, pivot_by=None,
            limit=None, distinct=None, flatten=None):
    "A convenience constructor for writing tests without having to provide all arguments."
    return qp.Select(target_spec,
                     from_clause, where_clause,
                     group_by, order_by, pivot_by,
                     limit, distinct, flatten)


class QueryParserTestBase(unittest.TestCase):

    maxDiff = 8192

    def setUp(self):
        self.parser = qp.Parser()

    def parse(self, query):
        """Parse one query.

        Args:
          query: An SQL query to be parsed.
        Returns:
          The AST.
        """
        return self.parser.parse(query.strip())

    def assertParse(self, expected, query, debug=False):
        """Assert parsed contents from 'query' is 'expected'.

        Args:
          expected: An expected AST to compare against the parsed value.
          query: An SQL query to be parsed.
          debug: A boolean, if true, print extra debugging information on the console.
        Raises:
          AssertionError: If the actual AST does not match the expected one.
        """
        actual = self.parse(query)
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


class TestSelectTarget(QueryParserTestBase):

    def test_unterminated__empty(self):
        with self.assertRaises(qp.ParseError):
            self.parse("SELECT")

    def test_unterminated__non_empty(self):
        # Unterminated statements are now accepted.
        self.parse("SELECT date, account")

    def test_empty(self):
        with self.assertRaises(qp.ParseError):
            self.parse("SELECT ; ")

    def test_target_wildcard(self):
        self.assertParse(qSelect(qp.Wildcard()),
                         "SELECT *;")

    def test_target_one(self):
        self.assertParse(qSelect([qp.Target(qp.Column('date'), None)]),
                         "SELECT date;")

    def test_target_one_as(self):
        self.assertParse(qSelect([qp.Target(qp.Column('date'), 'xdate')]),
                         "SELECT date as xdate;")

    def test_target_multiple(self):
        self.assertParse(qSelect([qp.Target(qp.Column('date'), None),
                                  qp.Target(qp.Column('account'), None),
                                  qp.Target(qp.Column('position'), None)]),
                         "SELECT date, account, position;")

    def test_target_multiple_as(self):
        self.assertParse(qSelect([qp.Target(qp.Column('date'), 'xdate'),
                                  qp.Target(qp.Column('account'), None),
                                  qp.Target(qp.Column('position'), 'xposition')]),
                         "SELECT date as xdate, account, position as xposition;")


class TestSelectExpression(QueryParserTestBase):

    def test_expr_constant_null(self):
        self.assertParse(qSelect([qp.Target(qp.Constant(None), None)]),
                         "SELECT NULL;")

    def test_expr_constant_boolean(self):
        self.assertParse(qSelect([qp.Target(qp.Constant(True), None)]),
                         "SELECT TRUE;")

        self.assertParse(qSelect([qp.Target(qp.Constant(False), None)]),
                         "SELECT FALSE;")

    def test_expr_constant_integer(self):
        self.assertParse(qSelect([qp.Target(qp.Constant(17), None)]),
                         "SELECT 17;")

    def test_expr_constant_decimal(self):
        self.assertParse(qSelect([qp.Target(qp.Constant(D('17.345')), None)]),
                         "SELECT 17.345;")

        self.assertParse(qSelect([qp.Target(qp.Constant(D('.345')), None)]),
                         "SELECT .345;")

        self.assertParse(qSelect([qp.Target(qp.Constant(D('17.')), None)]),
                         "SELECT 17.;")

    def test_expr_constant_string(self):
        self.assertParse(qSelect([qp.Target(qp.Constant('rainy-day'), None)]),
                         "SELECT 'rainy-day';")

    def test_expr_constant_date(self):
        self.assertParse(
            qSelect([qp.Target(qp.Constant(datetime.date(1972, 5, 28)), None)]),
            "SELECT 1972-05-28;")

        self.assertParse(
            qSelect([qp.Target(qp.Constant(datetime.date(1972, 5, 28)), None)]),
            "SELECT #'May 28, 1972';")

    def test_expr_column(self):
        self.assertParse(
            qSelect([qp.Target(qp.Column('date'), None)]),
            "SELECT date;")

    def test_expr_eq(self):
        self.assertParse(
            qSelect([qp.Target(qp.Equal(qp.Column('a'), qp.Constant(42)), None)]),
            "SELECT a = 42;")

    def test_expr_ne(self):
        self.assertParse(
            qSelect([
                qp.Target(qp.Not(qp.Equal(qp.Column('a'), qp.Constant(42))),
                         None)]),
            "SELECT a != 42;")

    def test_expr_gt(self):
        self.assertParse(
            qSelect([qp.Target(qp.Greater(qp.Column('a'), qp.Constant(42)), None)]),
            "SELECT a > 42;")

    def test_expr_gte(self):
        self.assertParse(
            qSelect([qp.Target(qp.GreaterEq(qp.Column('a'), qp.Constant(42)), None)]),
            "SELECT a >= 42;")

    def test_expr_lt(self):
        self.assertParse(
            qSelect([qp.Target(qp.Less(qp.Column('a'), qp.Constant(42)), None)]),
            "SELECT a < 42;")

    def test_expr_lte(self):
        self.assertParse(
            qSelect([qp.Target(qp.LessEq(qp.Column('a'), qp.Constant(42)), None)]),
            "SELECT a <= 42;")

    def test_expr_match(self):
        self.assertParse(
            qSelect([qp.Target(
                qp.Match(qp.Column('a'), qp.Constant('Assets:.*:Checking')),
                None)]),
            "SELECT a ~ 'Assets:.*:Checking';")

    def test_expr_paren_single(self):
        self.assertParse(
            qSelect([
                qp.Target(qp.Not(qp.Equal(qp.Column('a'), qp.Constant(42))),
                          None)]),
            "SELECT a != (42);")

    def test_expr_paren_multi(self):
        self.assertParse(
            qSelect([
                qp.Target(qp.Not(qp.Equal(qp.Column('a'), qp.Constant(42))),
                          None)]),
            "SELECT not (a = 42);")

    def test_expr_and(self):
        self.assertParse(
            qSelect([qp.Target(qp.And(qp.Column('a'), qp.Column('b')), None)]),
            "SELECT a AND b;")

    def test_expr_or(self):
        self.assertParse(
            qSelect([qp.Target(qp.Or(qp.Column('a'), qp.Column('b')), None)]),
            "SELECT a OR b;")

    def test_expr_not(self):
        self.assertParse(
            qSelect([qp.Target(qp.Not(qp.Column('a')), None)]),
            "SELECT NOT a;")

    def test_expr_paren_multi2(self):
        self.assertParse(
            qSelect([qp.Target(
                qp.Not(qp.Equal(
                    qp.Column('a'),
                    qp.Not(qp.Equal(
                        qp.Column('b'),
                        qp.And(qp.Constant(42),
                              qp.Constant(17)))))),
                None)]),
            "SELECT a != (b != (42 AND 17));")

    def test_expr_function__zero_args(self):
        self.assertParse(
            qSelect([qp.Target(qp.Function('random', []), None)]),
            "SELECT random();")

    def test_expr_function__one_args(self):
        self.assertParse(
            qSelect([qp.Target(qp.Function('min', [qp.Column('a')]), None)]),
            "SELECT min(a);")

    def test_expr_function__two_args(self):
        self.assertParse(
            qSelect([qp.Target(qp.Function('min', [qp.Column('a'),
                                                   qp.Column('b')]), None)]),
            "SELECT min(a, b);")

    def test_expr_function__five_args(self):
        self.assertParse(
            qSelect([qp.Target(qp.Function('min', [qp.Column('a'),
                                                   qp.Column('b'),
                                                   qp.Column('c'),
                                                   qp.Column('d'),
                                                   qp.Column('e')]), None)]),
            "SELECT min(a, b, c, d, e);")

    def test_expr_mul(self):
        self.assertParse(
            qSelect([qp.Target(qp.Mul(qp.Column('a'), qp.Column('b')), None)]),
            "SELECT a * b;")

    def test_expr_div(self):
        self.assertParse(
            qSelect([qp.Target(qp.Div(qp.Column('a'), qp.Column('b')), None)]),
            r"SELECT a / b;")

    def test_expr_add(self):
        expected = qSelect([qp.Target(qp.Add(qp.Column('a'), qp.Column('b')), None)])
        self.assertParse(expected, "SELECT a + b;")
        self.assertParse(expected, "SELECT a+b;")


    def test_expr_sub(self):
        expected = qSelect([qp.Target(qp.Sub(qp.Column('a'), qp.Column('b')), None)])
        self.assertParse(expected, "SELECT a - b;")
        self.assertParse(expected, "SELECT a-b;")

    def test_expr_numerical(self):
        expected = qSelect([qp.Target(qp.Add(qp.Constant(2), qp.Constant(3)), None)])
        self.assertParse(expected, "SELECT 2+(3);")

        expected = qSelect([qp.Target(qp.Sub(qp.Constant(2), qp.Constant(3)), None)])
        self.assertParse(expected, "SELECT 2-(3);")

        # Note: The parser should be modified to remove signs from the DECIMAL
        # and INTEGER tokens such that this is possible:
        #self.assertParse(expected, "SELECT 2+3;")
        #self.assertParse(expected, "SELECT 2-3;")


class TestSelectPrecedence(QueryParserTestBase):

    def test_expr_function__and_or(self):
        self.assertParse(
            qSelect(qp.Wildcard(),
                    where_clause=qp.Or(qp.And(qp.Column('a'), qp.Column('b')),
                                       qp.And(qp.Column('c'), qp.Column('d')))),
            "SELECT * WHERE a AND b OR c AND d;")

    def test_expr_function__and_eq(self):
        self.assertParse(
            qSelect(qp.Wildcard(),
                    where_clause=qp.And(
                        qp.Equal(qp.Column('a'), qp.Constant(2)),
                        qp.Not(qp.Equal(qp.Column('b'), qp.Constant(3))))),
            "SELECT * WHERE a = 2 AND b != 3;")

    def test_expr_function__and_not(self):
        self.assertParse(
            qSelect(qp.Wildcard(),
                    where_clause=qp.And(
                        qp.Not(qp.Column('a')), qp.Column('b'))),
            "SELECT * WHERE not a AND b;")

    def test_expr_function__and_plus_minus(self):
        self.assertParse(
            qSelect(qp.Wildcard(),
                    where_clause=qp.And(
                        qp.Add(qp.Column('a'), qp.Column('b')),
                        qp.Sub(qp.Column('c'), qp.Column('d')))),
            "SELECT * WHERE a + b AND c - d;")

    def test_expr_function__mul_div_plus_minus(self):
        self.assertParse(
            qSelect(qp.Wildcard(),
                    where_clause=qp.Sub(qp.Add(qp.Mul(qp.Column(name='a'),
                                                      qp.Column(name='b')),
                                               qp.Div(qp.Column(name='c'),
                                                      qp.Column(name='d'))),
                                        qp.Constant(value=3))),
            "SELECT * WHERE a * b + c / d - 3;")

    def test_expr_function__membership_precedence(self):
        self.assertParse(
            qSelect(qp.Wildcard(),
                    where_clause=qp.And(qp.Contains(qp.Constant('orange'),
                                                    qp.Column('tags')),
                                        qp.Contains(qp.Constant('bananas'),
                                                    qp.Column('tags')))),
            "SELECT * WHERE 'orange' IN tags AND 'bananas' IN tags;")


class TestSelectFromBase(QueryParserTestBase):

    def setUp(self):
        super().setUp()
        self.targets = [qp.Target(qp.Column('a'), None),
                        qp.Target(qp.Column('b'), None)]
        self.expr = qp.Equal(qp.Column('d'),
                             qp.And(
                                 qp.Function('max', [qp.Column('e')]),
                                 qp.Constant(17)))

class TestSelectFrom(TestSelectFromBase):

    def test_from_empty(self):
        with self.assertRaises(qp.ParseError):
            self.parse("SELECT a, b FROM;")

    def test_from(self):
        self.assertParse(qSelect(self.targets, qp.From(self.expr, None, None, None)),
                         "SELECT a, b FROM d = (max(e) and 17);")

    def test_from_open_default(self):
        self.assertParse(qSelect(self.targets, qp.From(self.expr,
                                                      datetime.date(2014, 1, 1),
                                                      None, None)),
                         "SELECT a, b FROM d = (max(e) and 17) OPEN ON 2014-01-01;")

    def test_from_close_default(self):
        self.assertParse(qSelect(self.targets, qp.From(self.expr, None, True, None)),
                         "SELECT a, b FROM d = (max(e) and 17) CLOSE;")

    def test_from_close_dated(self):
        self.assertParse(qSelect(self.targets,
                                 qp.From(self.expr, None, datetime.date(2014, 10, 18),
                                        None)),
                         "SELECT a, b FROM d = (max(e) and 17) CLOSE ON 2014-10-18;")

    def test_from_close_no_expr(self):
        self.assertParse(qSelect(self.targets, qp.From(None, None, True, None)),
                         "SELECT a, b FROM CLOSE;")

    def test_from_close_no_expr_dated(self):
        self.assertParse(qSelect(self.targets,
                                 qp.From(None, None, datetime.date(2014, 10, 18), None)),
                         "SELECT a, b FROM CLOSE ON 2014-10-18;")

    def test_from_clear_default(self):
        self.assertParse(qSelect(self.targets, qp.From(self.expr, None, None, True)),
                         "SELECT a, b FROM d = (max(e) and 17) CLEAR;")

    def test_from_open_close_clear(self):
        self.assertParse(qSelect(self.targets, qp.From(
            self.expr, datetime.date(2013, 10, 25), datetime.date(2014, 10, 25), True)), """
          SELECT a, b
          FROM d = (max(e) and 17) OPEN ON 2013-10-25 CLOSE ON 2014-10-25 CLEAR;
        """)


class TestSelectWhere(TestSelectFromBase):

    def test_where_empty(self):
        with self.assertRaises(qp.ParseError):
            self.parse("SELECT a, b WHERE;")

    def test_where(self):
        self.assertParse(qSelect(self.targets, None, self.expr),
                         "SELECT a, b WHERE d = (max(e) and 17);")


class TestSelectFromAndWhere(TestSelectFromBase):

    def test_both(self):
        self.assertParse(qSelect(self.targets,
                                 qp.From(self.expr, None, None, None),
                                 self.expr), """
          SELECT a, b
          FROM d = (max(e) and 17)
          WHERE d = (max(e) and 17);
        """)


class TestSelectFromSelect(QueryParserTestBase):

    def test_from_select(self):
        subselect = qSelect(
            qp.Wildcard(),
            qp.From(qp.Equal(qp.Column('date'),
                             qp.Constant(datetime.date(2014, 5, 2))),
                   None, None, None))

        expected = qSelect([qp.Target(qp.Column('a'), None),
                            qp.Target(qp.Column('b'), None)],
                           subselect,
                           qp.Equal(qp.Column('c'), qp.Constant(5)),
                           limit=100)

        self.assertParse(expected, """
           SELECT a, b FROM (
              SELECT * FROM date = 2014-05-02
           ) WHERE c = 5 LIMIT 100;
        """)


class TestSelectGroupBy(QueryParserTestBase):

    def test_groupby_empty(self):
        with self.assertRaises(qp.ParseError):
            self.parse("SELECT * GROUP BY;")

    def test_groupby_one(self):
        self.assertParse(qSelect(qp.Wildcard(),
                                 group_by=qp.GroupBy([qp.Column('a')], None)),
                         "SELECT * GROUP BY a;")

    def test_groupby_many(self):
        self.assertParse(qSelect(qp.Wildcard(),
                                 group_by=qp.GroupBy([qp.Column('a'),
                                                      qp.Column('b'),
                                                      qp.Column('c')], None)),
                         "SELECT * GROUP BY a, b, c;")

    def test_groupby_expr(self):
        self.assertParse(
            qSelect(qp.Wildcard(),
                    group_by=qp.GroupBy([
                        qp.Greater(qp.Function('length', [qp.Column('a')]), qp.Constant(0)),
                        qp.Column('b')], None)),
            "SELECT * GROUP BY length(a) > 0, b;")

    def test_groupby_having(self):
        self.assertParse(
            qSelect(qp.Wildcard(),
                    group_by=qp.GroupBy([qp.Column('a')],
                                        qp.Equal(
                                            qp.Function('sum', [qp.Column('position')]),
                                            qp.Constant(0)))),
            "SELECT * GROUP BY a HAVING sum(position) = 0;")

    def test_groupby_numbers(self):
        self.assertParse(qSelect(qp.Wildcard(), group_by=qp.GroupBy([1], None)),
                         "SELECT * GROUP BY 1;")

        self.assertParse(qSelect(qp.Wildcard(), group_by=qp.GroupBy([2, 4, 5], None)),
                         "SELECT * GROUP BY 2, 4, 5;")


class TestSelectOrderBy(QueryParserTestBase):

    def test_orderby_empty(self):
        with self.assertRaises(qp.ParseError):
            self.parse("SELECT * ORDER BY;")

    def test_orderby_one(self):
        self.assertParse(qSelect(qp.Wildcard(),
                                 order_by=qp.OrderBy([qp.Column('a')], None)),
                         "SELECT * ORDER BY a;")

    def test_orderby_many(self):
        self.assertParse(qSelect(qp.Wildcard(),
                                 order_by=qp.OrderBy([qp.Column('a'),
                                                      qp.Column('b'),
                                                      qp.Column('c')], None)),
                         "SELECT * ORDER BY a, b, c;")

    def test_orderby_asc(self):
        self.assertParse(qSelect(qp.Wildcard(),
                                 order_by=qp.OrderBy([qp.Column('a')], 'ASC')),
                         "SELECT * ORDER BY a ASC;")

    def test_orderby_desc(self):
        self.assertParse(qSelect(qp.Wildcard(),
                                 order_by=qp.OrderBy([qp.Column('a')], 'DESC')),
                         "SELECT * ORDER BY a DESC;")


class TestSelectPivotBy(QueryParserTestBase):

    def test_pivotby_empty(self):
        with self.assertRaises(qp.ParseError):
            self.parse("SELECT * PIVOT BY;")

    def test_pivotby_one(self):
        self.assertParse(qSelect(qp.Wildcard(), pivot_by=qp.PivotBy([qp.Column('a')])),
                         "SELECT * PIVOT BY a;")

    def test_pivotby_many(self):
        self.assertParse(qSelect(qp.Wildcard(), pivot_by=qp.PivotBy([qp.Column('a'),
                                                                     qp.Column('b'),
                                                                     qp.Column('c')])),
                         "SELECT * PIVOT BY a, b , c;")


class TestSelectOptions(QueryParserTestBase):

    def test_distinct(self):
        self.assertParse(qSelect([qp.Target(qp.Column('account'), None)], distinct=True),
                         "SELECT DISTINCT account;")

    def test_limit_empty(self):
        with self.assertRaises(qp.ParseError):
            self.parse("SELECT * LIMIT;")

    def test_limit_present(self):
        self.assertParse(qSelect(qp.Wildcard(), limit=45),
                         "SELECT * LIMIT 45;")

    def test_flatten(self):
        self.assertParse(qSelect(qp.Wildcard(), flatten=True),
                         "SELECT * FLATTEN;")

    def test_limit_and_flatten(self):
        self.assertParse(qSelect(qp.Wildcard(), limit=100, flatten=True),
                         "SELECT * LIMIT 100 FLATTEN;")


class TestBalances(QueryParserTestBase):

    def test_balances_empty(self):
        self.assertParse(qp.Balances(None, None, None),
                         "BALANCES;")

    def test_balances_from(self):
        self.assertParse(
            qp.Balances(None,
                        qp.From(qp.Equal(qp.Column('date'),
                                         qp.Constant(datetime.date(2014, 1, 1))),
                                None, True, None),
                        None),
            "BALANCES FROM date = 2014-01-01 CLOSE;")

    def test_balances_from_with_transformer(self):
        self.assertParse(
            qp.Balances('units',
                        qp.From(qp.Equal(qp.Column('date'),
                                         qp.Constant(datetime.date(2014, 1, 1))),
                                None, True, None),
                        None),
            "BALANCES AT units FROM date = 2014-01-01 CLOSE;")

    def test_balances_from_with_transformer_simple(self):
        self.assertParse(
            qp.Balances('units',
                        None,
                        qp.Equal(qp.Column('date'),
                                 qp.Constant(datetime.date(2014, 1, 1)))),
            "BALANCES AT units WHERE date = 2014-01-01;")


class TestJournal(QueryParserTestBase):

    def test_journal_empty(self):
        self.assertParse(qp.Journal(None, None, None),
                         "JOURNAL;")

    def test_journal_account(self):
        self.assertParse(qp.Journal('Assets:Checking', None, None),
                         "JOURNAL 'Assets:Checking';")

    def test_journal_summary(self):
        self.assertParse(qp.Journal(None, 'cost', None),
                         "JOURNAL AT cost;")

    def test_journal_account_and_summary(self):
        self.assertParse(qp.Journal('Assets:Checking', 'cost', None),
                         "JOURNAL 'Assets:Checking' AT cost;")

    def test_journal_from(self):
        self.assertParse(
            qp.Journal(None, None, qp.From(qp.Equal(qp.Column('date'),
                                                    qp.Constant(datetime.date(2014, 1, 1))),
                                           None, True, None)),
            "JOURNAL FROM date = 2014-01-01 CLOSE;")


class TestPrint(QueryParserTestBase):

    def test_print_empty(self):
        self.assertParse(qp.Print(None),
                         "PRINT;")

    def test_print_from(self):
        self.assertParse(
            qp.Print(qp.From(qp.Equal(qp.Column('date'),
                                      qp.Constant(datetime.date(2014, 1, 1))),
                             None, True, None)),
            "PRINT FROM date = 2014-01-01 CLOSE;")


class TestExpressionName(QueryParserTestBase):

    def test_column(self):
        self.assertEqual('date', qp.get_expression_name(
            qp.Column('date')))

    def test_function(self):
        self.assertEqual('length_date', qp.get_expression_name(
            qp.Function('length', [qp.Column('date')])))

    def test_constant(self):
        self.assertEqual('c17', qp.get_expression_name(
            qp.Constant(17)))
        self.assertEqual('c2014_01_01', qp.get_expression_name(
            qp.Constant(datetime.date(2014, 1, 1))))

    def test_unary(self):
        self.assertEqual('not_account', qp.get_expression_name(
            qp.Not(qp.Column('account'))))

    def test_binary(self):
        self.assertEqual('and_account_date', qp.get_expression_name(
            qp.And(qp.Column('account'), qp.Column('date'))))


class TestExplain(QueryParserTestBase):

    def test_explain_select(self):
        self.assertParse(qp.Explain(
            qSelect([qp.Target(qp.Column('date'), None),
                     qp.Target(qp.Column('account'), None)],
                    where_clause=qp.Match(qp.Column('account'), qp.Constant('etrade')))
            ), "EXPLAIN SELECT date, account WHERE account ~ 'etrade';")

    def test_explain_balances(self):
        self.assertParse(qp.Explain(
            qp.Balances('cost', None, None)
            ), "EXPLAIN BALANCES AT cost;")

    def test_explain_journal(self):
        self.assertParse(qp.Explain(
            qp.Journal('Assets:ETrade', 'units', None)
            ), "EXPLAIN JOURNAL 'Assets:ETrade' AT units;")
