import datetime
import re
import unittest

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.core import position
from beancount.query import query_parser as q
from beancount.query import query_compile as c
from beancount.query import query_contexts as cc
from beancount.query import query_execute as x
from beancount import loader


INPUT = """

2010-01-01 open Assets:Bank:Checking
2010-01-01 open Assets:Bank:Savings

2010-01-01 open Expenses:Restaurant

2011-02-10 *
  Assets:Bank:Checking       123.00 USD
  Expenses:Restaurant

"""

def setUp(module):
    # Load the global test input file.
    global entries, errors, options_map
    entries, errors, options_map = loader.load_string(INPUT)


class ExecuteQueryBase(unittest.TestCase):

    maxDiff = 8192

    # Default execution contexts.
    xcontext_entries = cc.FilterEntriesContext()
    xcontext_targets = cc.TargetsContext()
    xcontext_postings = cc.FilterPostingsContext()

    def setUp(self):
        self.parser = q.Parser()

    def execute(self, query):
        """Parse a query, execute it and compile it.

        Args:
          query: An SQL query to be parsed.
        """
        select = self.parser.parse(query.strip())
        query = c.compile_select(select,
                                 self.xcontext_targets,
                                 self.xcontext_postings,
                                 self.xcontext_entries)
        return x.execute_query(query, entries)


class TestExecute(ExecuteQueryBase):

    def test_simple(self):
        x = self.execute("""
          SELECT date, flag, account
          GROUP BY date, flag, account;
        """)
        print()
        print(x._fields)

        x = self.execute("""
          SELECT date, flag, account, sum(change) as balance
          GROUP BY date, flag, account;
        """)
        print()
        print(x._fields)

        x = self.execute("""
          SELECT first(account), last(account)
          GROUP BY account;
        """)
        print()
        print(x._fields)

        x = self.execute("""
          SELECT date, account, sum(change) as balance
          GROUP BY date, flag, account;
        """)
        print()
        print(x._fields)
