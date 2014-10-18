import unittest

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

    def assertParse(self, expected, query):
        """Assert parsed contents from 'query' is 'expected'.

        Args:
          expected: An expected AST to compare against the parsed value.
          query: An SQL query to be parsed.
        Raises:
          AssertionError: If the actual AST does not match the expected one.
        """
        actual = self.parse(query)
        try:
            self.assertEqual(expected, actual)
        except AssertionError:
            #print()
            #print(actual)
            raise

class TestQueryParserSelect(QueryParserTestBase):

    def test_unterminated(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT")

    def test_empty(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT ; ")

    def test_columns_invalid(self):
        with self.assertRaises(q.ParseError):
            self.parse("SELECT invalid;")

    def test_columns_one(self):
        self.assertParse(q.Query([q.DateColumn('date')],
                                 q.Constant(True),
                                 q.Constant(True)),
                         "SELECT date;")


# FIXME: Move function resolution and checking outside the parser. This will
# eventually also make it easier to move the parser back to flex/bison!
# Do this.
