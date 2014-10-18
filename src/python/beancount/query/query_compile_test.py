import datetime
import unittest

from beancount.core.amount import D
from beancount.query import query_parser as q
from beancount.query import query_compile as c


class QueryParserTestBase(unittest.TestCase):

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
        return c.compile_query(statement)

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


class TestCompile(QueryParserTestBase):

    pass
