import unittest

from beancount.query import query_parser as q


class TestQueryParser(unittest.TestCase):

    def setUp(self):
        self.parser = q.Parser()

    def test_select_only(self):
        x = self.parser.parse("""
          SELECT account;
        """.strip())
        print()
        print(x)

        expected = q.Query(q.Tauto(), q.Tauto(), [q.AccountColumn('account')])
        print(x)
        self.assertEqual(expected, x)
