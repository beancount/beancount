import datetime
import unittest

from beancount.core.amount import D
from beancount.query import query_parser as q


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
            print(actual)
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
        self.assertParse(q.Select(q.Wildcard(),
                                  None, None),
                         "SELECT *;")

    def test_target_one(self):
        self.assertParse(q.Select([q.Target(q.Column('date'))],
                                  None, None),
                         "SELECT date;")

    def test_target_one_as(self):
        self.assertParse(q.Select([q.Target(q.Column('date'), 'xdate')],
                                  None, None),
                         "SELECT date as xdate;")

    def test_target_multiple(self):
        self.assertParse(q.Select([q.Target(q.Column('date')),
                                   q.Target(q.Column('account')),
                                   q.Target(q.Column('change'))],
                                  None, None),
                         "SELECT date, account, change;")

    def test_target_multiple_as(self):
        self.assertParse(q.Select([q.Target(q.Column('date'), 'xdate'),
                                   q.Target(q.Column('account')),
                                   q.Target(q.Column('change'), 'xchange')],
                                  None, None),
                         "SELECT date as xdate, account, change as xchange;")


class TestSelectExpression(QueryParserTestBase):

    def test_expr_constant_null(self):
        self.assertParse(q.Select([q.Target(q.Constant(None))],
                                  None, None),
                         "SELECT NULL;")

    def test_expr_constant_boolean(self):
        self.assertParse(q.Select([q.Target(q.Constant(True))],
                                  None, None),
                         "SELECT TRUE;")

        self.assertParse(q.Select([q.Target(q.Constant(False))],
                                  None, None),
                         "SELECT FALSE;")

    def test_expr_constant_integer(self):
        self.assertParse(q.Select([q.Target(q.Constant(17))],
                                  None, None),
                         "SELECT 17;")

    def test_expr_constant_decimal(self):
        self.assertParse(q.Select([q.Target(q.Constant(D('17.345')))],
                                  None, None),
                         "SELECT 17.345;")

        self.assertParse(q.Select([q.Target(q.Constant(D('.345')))],
                                  None, None),
                         "SELECT .345;")

        self.assertParse(q.Select([q.Target(q.Constant(D('17.')))],
                                  None, None),
                         "SELECT 17.;")

    def test_expr_constant_string(self):
        self.assertParse(q.Select([q.Target(q.Constant('rainy-day'))],
                                  None, None),
                         "SELECT 'rainy-day';")

    def test_expr_constant_date(self):
        self.assertParse(q.Select([q.Target(q.Constant(datetime.date(1972, 5, 28)))],
                                  None, None),
                         "SELECT 1972-05-28;")

        self.assertParse(q.Select([q.Target(q.Constant(datetime.date(1972, 5, 28)))],
                                  None, None),
                         "SELECT #'May 28, 1972';")

    def test_expr_column(self):
        self.assertParse(
            q.Select([q.Target(q.Column('date'))],
                     None, None),
            "SELECT date;")

    def test_expr_eq(self):
        self.assertParse(
            q.Select([q.Target(q.Equal(q.Column('a'), q.Constant(42)))],
                     None, None),
            "SELECT a = 42;")

    def test_expr_ne(self):
        self.assertParse(
            q.Select([q.Target(q.Not(q.Equal(q.Column('a'), q.Constant(42))))],
                     None, None),
            "SELECT a != 42;")

    def test_expr_paren_single(self):
        self.assertParse(
            q.Select([q.Target(q.Not(q.Equal(q.Column('a'), q.Constant(42))))],
                     None, None),
            "SELECT a != (42);")

    def test_expr_paren_multi(self):
        self.assertParse(
            q.Select([q.Target(q.Not(q.Equal(q.Column('a'), q.Constant(42))))],
                     None, None),
            "SELECT not (a = 42);")

    def test_expr_and(self):
        self.assertParse(
            q.Select([q.Target(q.And(q.Column('a'), q.Column('b')))],
                     None, None),
            "SELECT a AND b;")

    def test_expr_or(self):
        self.assertParse(
            q.Select([q.Target(q.Or(q.Column('a'), q.Column('b')))],
                     None, None),
            "SELECT a OR b;")

    def test_expr_not(self):
        self.assertParse(
            q.Select([q.Target(q.Not(q.Column('a')))],
                     None, None),
            "SELECT NOT a;")

    def test_expr_paren_multi2(self):
        self.assertParse(
            q.Select([q.Target(
                q.Not(q.Equal(
                    q.Column('a'),
                    q.Not(q.Equal(
                        q.Column('b'),
                        q.And(q.Constant(42),
                              q.Constant(17)))))))],
                     None, None),
            "SELECT a != (b != (42 AND 17));")

    def test_expr_function__zero_args(self):
        self.assertParse(
            q.Select([q.Target(q.Function('random', None))],
                     None, None),
            "SELECT random();")

    def test_expr_function__one_args(self):
        self.assertParse(
            q.Select([q.Target(q.Function('min', [q.Column('a')]))],
                     None, None),
            "SELECT min(a);")

    def test_expr_function__two_args(self):
        self.assertParse(
            q.Select([q.Target(q.Function('min', [q.Column('a'), q.Column('b')]))],
                     None, None),
            "SELECT min(a, b);")

    def test_expr_function__five_args(self):
        self.assertParse(
            q.Select([q.Target(q.Function('min', [q.Column('a'),
                                                  q.Column('b'),
                                                  q.Column('c'),
                                                  q.Column('d'),
                                                  q.Column('e')]))],
                     None, None),
            "SELECT min(a, b, c, d, e);")


class TestSelectFromWhere(QueryParserTestBase):

    def setUp(self):
        super().setUp()
        self.targets = [q.Target(q.Column('a')),
                        q.Target(q.Column('b'))]
        self.expr = q.Equal(q.Column('d'),
                            q.And(
                                q.Function('max', [q.Column('e')]),
                                q.Constant(17)))

    def test_from(self):
        self.assertParse(q.Select(self.targets, self.expr, None),
                         "SELECT a, b FROM d = (max(e) and 17);")

    def test_where(self):
        self.assertParse(q.Select(self.targets, None, self.expr),
                         "SELECT a, b WHERE d = (max(e) and 17);")

    def test_both(self):
        self.assertParse(q.Select(self.targets, self.expr, self.expr), """
          SELECT a, b
          FROM d = (max(e) and 17)
          WHERE d = (max(e) and 17);
        """)
