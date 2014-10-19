import datetime
import unittest

from beancount.core.amount import D
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
        self.assertEqual(q.Not(c.AccountColumn()),
                         c.compile_expression(q.Not(q.Column('account')),
                                              c.TargetsContext()))

    def test_expr_binaryop(self):
        self.assertEqual(q.Equal(c.DateColumn(),
                                 q.Constant(datetime.date(2014, 1, 1))),
                         c.compile_expression(
                             q.Equal(q.Column('date'),
                                     q.Constant(datetime.date(2014, 1, 1))),
                             c.TargetsContext()))

    def test_expr_constant(self):
        self.assertEqual(q.Constant(D(17)),
                         c.compile_expression(q.Constant(D(17)), c.TargetsContext()))



# class TestCompile(QueryParserTestBase):

#     def test_wildcard(self):
#         self.assertCompile(None, """
#           SELECT * ;
#         """)

#     def test_simple(self):
#         self.assertCompile(None, """
#           SELECT length(a) ;
#         """)
