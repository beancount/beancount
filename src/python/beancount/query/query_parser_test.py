import datetime
import unittest

from beancount.core.amount import D
from beancount.query import query_parser as q


def qSelect(target_spec=None,
            from_clause=None, where_clause=None,
            group_by=None, order_by=None, pivot_by=None,
            limit=None, distinct=None, flatten=None):
    "A convenience constructor for writing tests without having to provide all arguments."
    return q.Select(target_spec,
                    from_clause, where_clause,
                    group_by, order_by, pivot_by,
                    limit, distinct, flatten)


class QueryParserTestBase(unittest.TestCase):

    maxDiff = 8192

    def setUp(self):
        self.parser = q.Parser()

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
        with self.assertRaises(q.ParseError):
            self.parse("SELECT")

    def test_unterminated__non_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT date, account")

    def test_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT ; ")

    def test_target_wildcard(self):
        self.assertParse(qSelect(q.Wildcard()),
                         "SELECT *;")

    def test_target_one(self):
        self.assertParse(qSelect([q.Target(q.Column('date'), None)]),
                         "SELECT date;")

    def test_target_one_as(self):
        self.assertParse(qSelect([q.Target(q.Column('date'), 'xdate')]),
                         "SELECT date as xdate;")

    def test_target_multiple(self):
        self.assertParse(qSelect([q.Target(q.Column('date'), None),
                                   q.Target(q.Column('account'), None),
                                   q.Target(q.Column('change'), None)]),
                         "SELECT date, account, change;")

    def test_target_multiple_as(self):
        self.assertParse(qSelect([q.Target(q.Column('date'), 'xdate'),
                                   q.Target(q.Column('account'), None),
                                   q.Target(q.Column('change'), 'xchange')]),
                         "SELECT date as xdate, account, change as xchange;")


class TestSelectExpression(QueryParserTestBase):

    def test_expr_constant_null(self):
        self.assertParse(qSelect([q.Target(q.Constant(None), None)]),
                         "SELECT NULL;")

    def test_expr_constant_boolean(self):
        self.assertParse(qSelect([q.Target(q.Constant(True), None)]),
                         "SELECT TRUE;")

        self.assertParse(qSelect([q.Target(q.Constant(False), None)]),
                         "SELECT FALSE;")

    def test_expr_constant_integer(self):
        self.assertParse(qSelect([q.Target(q.Constant(17), None)]),
                         "SELECT 17;")

    def test_expr_constant_decimal(self):
        self.assertParse(qSelect([q.Target(q.Constant(D('17.345')), None)]),
                         "SELECT 17.345;")

        self.assertParse(qSelect([q.Target(q.Constant(D('.345')), None)]),
                         "SELECT .345;")

        self.assertParse(qSelect([q.Target(q.Constant(D('17.')), None)]),
                         "SELECT 17.;")

    def test_expr_constant_string(self):
        self.assertParse(qSelect([q.Target(q.Constant('rainy-day'), None)]),
                         "SELECT 'rainy-day';")

    def test_expr_constant_date(self):
        self.assertParse(qSelect([q.Target(q.Constant(datetime.date(1972, 5, 28)), None)]),
                         "SELECT 1972-05-28;")

        self.assertParse(qSelect([q.Target(q.Constant(datetime.date(1972, 5, 28)), None)]),
                         "SELECT #'May 28, 1972';")

    def test_expr_column(self):
        self.assertParse(
            qSelect([q.Target(q.Column('date'), None)]),
            "SELECT date;")

    def test_expr_eq(self):
        self.assertParse(
            qSelect([q.Target(q.Equal(q.Column('a'), q.Constant(42)), None)]),
            "SELECT a = 42;")

    def test_expr_ne(self):
        self.assertParse(
            qSelect([
                q.Target(q.Not(q.Equal(q.Column('a'), q.Constant(42))),
                         None)]),
            "SELECT a != 42;")

    def test_expr_gt(self):
        self.assertParse(
            qSelect([q.Target(q.Greater(q.Column('a'), q.Constant(42)), None)]),
            "SELECT a > 42;")

    def test_expr_gte(self):
        self.assertParse(
            qSelect([q.Target(q.GreaterEq(q.Column('a'), q.Constant(42)), None)]),
            "SELECT a >= 42;")

    def test_expr_lte(self):
        self.assertParse(
            qSelect([q.Target(q.Less(q.Column('a'), q.Constant(42)), None)]),
            "SELECT a < 42;")

    def test_expr_lte(self):
        self.assertParse(
            qSelect([q.Target(q.LessEq(q.Column('a'), q.Constant(42)), None)]),
            "SELECT a <= 42;")

    def test_expr_match(self):
        self.assertParse(
            qSelect([q.Target(
                q.Match(q.Column('a'), q.Constant('Assets:.*:Checking')),
                None)]),
            "SELECT a ~ 'Assets:.*:Checking';")

    def test_expr_paren_single(self):
        self.assertParse(
            qSelect([
                q.Target(q.Not(q.Equal(q.Column('a'), q.Constant(42))),
                         None)]),
            "SELECT a != (42);")

    def test_expr_paren_multi(self):
        self.assertParse(
            qSelect([
                q.Target(q.Not(q.Equal(q.Column('a'), q.Constant(42))),
                         None)]),
            "SELECT not (a = 42);")

    def test_expr_and(self):
        self.assertParse(
            qSelect([q.Target(q.And(q.Column('a'), q.Column('b')), None)]),
            "SELECT a AND b;")

    def test_expr_or(self):
        self.assertParse(
            qSelect([q.Target(q.Or(q.Column('a'), q.Column('b')), None)]),
            "SELECT a OR b;")

    def test_expr_not(self):
        self.assertParse(
            qSelect([q.Target(q.Not(q.Column('a')), None)]),
            "SELECT NOT a;")

    def test_expr_paren_multi2(self):
        self.assertParse(
            qSelect([q.Target(
                q.Not(q.Equal(
                    q.Column('a'),
                    q.Not(q.Equal(
                        q.Column('b'),
                        q.And(q.Constant(42),
                              q.Constant(17)))))),
                None)]),
            "SELECT a != (b != (42 AND 17));")

    def test_expr_function__zero_args(self):
        self.assertParse(
            qSelect([q.Target(q.Function('random', []), None)]),
            "SELECT random();")

    def test_expr_function__one_args(self):
        self.assertParse(
            qSelect([q.Target(q.Function('min', [q.Column('a')]), None)]),
            "SELECT min(a);")

    def test_expr_function__two_args(self):
        self.assertParse(
            qSelect([q.Target(q.Function('min', [q.Column('a'), q.Column('b')]), None)]),
            "SELECT min(a, b);")

    def test_expr_function__five_args(self):
        self.assertParse(
            qSelect([q.Target(q.Function('min', [q.Column('a'),
                                                  q.Column('b'),
                                                  q.Column('c'),
                                                  q.Column('d'),
                                                  q.Column('e')]), None)]),
            "SELECT min(a, b, c, d, e);")


class TestSelectPrecedence(QueryParserTestBase):

    def test_expr_function__and_or(self):
        self.assertParse(
            qSelect(q.Wildcard(),
                    where_clause=q.Or(q.And(q.Column('a'), q.Column('b')),
                                      q.And(q.Column('c'), q.Column('d')))),
            "SELECT * WHERE a AND b OR c AND d;")

    def test_expr_function__and_eq(self):
        self.assertParse(
            qSelect(q.Wildcard(),
                    where_clause=q.And(
                        q.Equal(q.Column('a'), q.Constant(2)),
                        q.Not(q.Equal(q.Column('b'), q.Constant(3))))),
            "SELECT * WHERE a = 2 AND b != 3;")

    def test_expr_function__and_not(self):
        self.assertParse(
            qSelect(q.Wildcard(),
                    where_clause=q.And(
                        q.Not(q.Column('a')), q.Column('b'))),
            "SELECT * WHERE not a AND b;")

class TestSelectFromBase(QueryParserTestBase):

    def setUp(self):
        super().setUp()
        self.targets = [q.Target(q.Column('a'), None),
                        q.Target(q.Column('b'), None)]
        self.expr = q.Equal(q.Column('d'),
                            q.And(
                                q.Function('max', [q.Column('e')]),
                                q.Constant(17)))

class TestSelectFrom(TestSelectFromBase):

    def test_from_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT a, b FROM;")

    def test_from(self):
        self.assertParse(qSelect(self.targets, q.From(self.expr, None, None, None)),
                         "SELECT a, b FROM d = (max(e) and 17);")

    def test_from_open_default(self):
        self.assertParse(qSelect(self.targets, q.From(self.expr,
                                                      datetime.date(2014, 1, 1),
                                                      None, None)),
                         "SELECT a, b FROM d = (max(e) and 17) OPEN ON 2014-01-01;")

    def test_from_close_default(self):
        self.assertParse(qSelect(self.targets, q.From(self.expr, None, True, None)),
                         "SELECT a, b FROM d = (max(e) and 17) CLOSE;")

    def test_from_close_dated(self):
        self.assertParse(qSelect(self.targets,
                                 q.From(self.expr, None, datetime.date(2014, 10, 18), None)),
                         "SELECT a, b FROM d = (max(e) and 17) CLOSE ON 2014-10-18;")

    def test_from_close_no_expr(self):
        self.assertParse(qSelect(self.targets, q.From(None, None, True, None)),
                         "SELECT a, b FROM CLOSE;")

    def test_from_close_no_expr_dated(self):
        self.assertParse(qSelect(self.targets,
                                 q.From(None, None, datetime.date(2014, 10, 18), None)),
                         "SELECT a, b FROM CLOSE ON 2014-10-18;")

    def test_from_clear_default(self):
        self.assertParse(qSelect(self.targets, q.From(self.expr, None, None, True)),
                         "SELECT a, b FROM d = (max(e) and 17) CLEAR;")

    def test_from_open_close_clear(self):
        self.assertParse(qSelect(self.targets, q.From(
            self.expr, datetime.date(2013, 10, 25), datetime.date(2014, 10, 25), True)), """
          SELECT a, b
          FROM d = (max(e) and 17) OPEN ON 2013-10-25 CLOSE ON 2014-10-25 CLEAR;
        """)


class TestSelectWhere(TestSelectFromBase):

    def test_where_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT a, b WHERE;")

    def test_where(self):
        self.assertParse(qSelect(self.targets, None, self.expr),
                         "SELECT a, b WHERE d = (max(e) and 17);")


class TestSelectFromAndWhere(TestSelectFromBase):

    def test_both(self):
        self.assertParse(qSelect(self.targets,
                                 q.From(self.expr, None, None, None),
                                 self.expr), """
          SELECT a, b
          FROM d = (max(e) and 17)
          WHERE d = (max(e) and 17);
        """)


class TestSelectFromSelect(QueryParserTestBase):

    def test_from_select(self):
        subselect = qSelect(
            q.Wildcard(),
            q.From(q.Equal(q.Column('date'),
                           q.Constant(datetime.date(2014, 5, 2))),
                   None, None, None))

        expected = qSelect([q.Target(q.Column('a'), None),
                            q.Target(q.Column('b'), None)],
                           subselect,
                           q.Equal(q.Column('c'), q.Constant(5)),
                           limit=100)

        self.assertParse(expected, """
           SELECT a, b FROM (
              SELECT * FROM date = 2014-05-02
           ) WHERE c = 5 LIMIT 100;
        """)


class TestSelectGroupBy(QueryParserTestBase):

    def test_groupby_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT * GROUP BY;")

    def test_groupby_one(self):
        self.assertParse(qSelect(q.Wildcard(), group_by=q.GroupBy([q.Column('a')], None)),
                         "SELECT * GROUP BY a;")

    def test_groupby_many(self):
        self.assertParse(qSelect(q.Wildcard(), group_by=q.GroupBy([q.Column('a'),
                                                                   q.Column('b'),
                                                                   q.Column('c')], None)),
                         "SELECT * GROUP BY a, b, c;")

    def test_groupby_expr(self):
        self.assertParse(
            qSelect(q.Wildcard(),
                    group_by=q.GroupBy([
                        q.Greater(q.Function('length', [q.Column('a')]), q.Constant(0)),
                        q.Column('b')], None)),
            "SELECT * GROUP BY length(a) > 0, b;")

    def test_groupby_having(self):
        self.assertParse(
            qSelect(q.Wildcard(),
                    group_by=q.GroupBy([q.Column('a')],
                                       q.Equal(
                                           q.Function('sum', [q.Column('change')]),
                                           q.Constant(0)))),
            "SELECT * GROUP BY a HAVING sum(change) = 0;")

    def test_groupby_numbers(self):
        self.assertParse(qSelect(q.Wildcard(), group_by=q.GroupBy([1], None)),
                         "SELECT * GROUP BY 1;")

        self.assertParse(qSelect(q.Wildcard(), group_by=q.GroupBy([2, 4, 5], None)),
                         "SELECT * GROUP BY 2, 4, 5;")


class TestSelectOrderBy(QueryParserTestBase):

    def test_orderby_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT * ORDER BY;")

    def test_orderby_one(self):
        self.assertParse(qSelect(q.Wildcard(), order_by=q.OrderBy([q.Column('a')], None)),
                         "SELECT * ORDER BY a;")

    def test_orderby_many(self):
        self.assertParse(qSelect(q.Wildcard(), order_by=q.OrderBy([q.Column('a'),
                                                                   q.Column('b'),
                                                                   q.Column('c')], None)),
                         "SELECT * ORDER BY a, b, c;")

    def test_orderby_asc(self):
        self.assertParse(qSelect(q.Wildcard(), order_by=q.OrderBy([q.Column('a')], 'ASC')),
                         "SELECT * ORDER BY a ASC;")

    def test_orderby_desc(self):
        self.assertParse(qSelect(q.Wildcard(), order_by=q.OrderBy([q.Column('a')], 'DESC')),
                         "SELECT * ORDER BY a DESC;")


class TestSelectPivotBy(QueryParserTestBase):

    def test_pivotby_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT * PIVOT BY;")

    def test_pivotby_one(self):
        self.assertParse(qSelect(q.Wildcard(), pivot_by=q.PivotBy([q.Column('a')])),
                         "SELECT * PIVOT BY a;")

    def test_pivotby_many(self):
        self.assertParse(qSelect(q.Wildcard(), pivot_by=q.PivotBy([q.Column('a'),
                                                                   q.Column('b'),
                                                                   q.Column('c')])),
                         "SELECT * PIVOT BY a, b , c;")


class TestSelectOptions(QueryParserTestBase):

    def test_distinct(self):
        self.assertParse(qSelect([q.Target(q.Column('account'), None)], distinct=True),
                         "SELECT DISTINCT account;")

    def test_limit_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT * LIMIT;")

    def test_limit_present(self):
        self.assertParse(qSelect(q.Wildcard(), limit=45),
                         "SELECT * LIMIT 45;")

    def test_flatten(self):
        self.assertParse(qSelect(q.Wildcard(), flatten=True),
                         "SELECT * FLATTEN;")

    def test_limit_and_flatten(self):
        self.assertParse(qSelect(q.Wildcard(), limit=100, flatten=True),
                         "SELECT * LIMIT 100 FLATTEN;")

class TestBalance(QueryParserTestBase):

    def test_balance_empty(self):
        self.assertParse(q.Balance(None),
                         "BALANCES;")

    def test_balance_from(self):
        self.assertParse(
            q.Balance(q.From(q.Equal(q.Column('date'),
                                     q.Constant(datetime.date(2014, 1, 1))),
                             None, True, None)),
            "BALANCES FROM date = 2014-01-01 CLOSE;")

class TestJournal(QueryParserTestBase):

    def test_journal_empty(self):
        self.assertParse(q.Journal(None, None),
                         "JOURNAL;")

    def test_journal_account(self):
        self.assertParse(q.Journal('Assets:Checking', None),
                         "JOURNAL 'Assets:Checking';")

    def test_journal_from(self):
        self.assertParse(
            q.Journal(None, q.From(q.Equal(q.Column('date'),
                                           q.Constant(datetime.date(2014, 1, 1))),
                                   None, True, None)),
            "JOURNAL FROM date = 2014-01-01 CLOSE;")

class TestPrint(QueryParserTestBase):

    def test_print_empty(self):
        self.assertParse(q.Print(None),
                         "PRINT;")

    def test_print_from(self):
        self.assertParse(
            q.Print(q.From(q.Equal(q.Column('date'),
                                   q.Constant(datetime.date(2014, 1, 1))),
                           None, True, None)),
            "PRINT FROM date = 2014-01-01 CLOSE;")

class TestExpressionName(QueryParserTestBase):

    def test_column(self):
        self.assertEqual('date', q.get_expression_name(
            q.Column('date')))

    def test_function(self):
        self.assertEqual('length_date', q.get_expression_name(
            q.Function('length', [q.Column('date')])))

    def test_constant(self):
        self.assertEqual('c17', q.get_expression_name(
            q.Constant(17)))
        self.assertEqual('c2014_01_01', q.get_expression_name(
            q.Constant(datetime.date(2014, 1, 1))))

    def test_unary(self):
        self.assertEqual('not_account', q.get_expression_name(
            q.Not(q.Column('account'))))

    def test_binary(self):
        self.assertEqual('and_account_date', q.get_expression_name(
            q.And(q.Column('account'), q.Column('date'))))
