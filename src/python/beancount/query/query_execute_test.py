import datetime
import re
import unittest
import textwrap

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.core import position
from beancount.query import query_parser as q
from beancount.query import query_compile as c
from beancount.query import query_env as cc
from beancount.query import query_execute as x
from beancount.parser import printer
from beancount.parser import cmptest
from beancount import loader

from beancount.utils.misc_utils import box


INPUT = """

2010-01-01 open Assets:Bank:Checking
2010-01-01 open Assets:Bank:Savings

2010-01-01 open Expenses:Restaurant

2010-01-01 * "Dinner with Cero"
  Assets:Bank:Checking       100.00 USD
  Expenses:Restaurant

2011-01-01 * "Dinner with Uno"
  Assets:Bank:Checking       101.00 USD
  Expenses:Restaurant

2012-02-02 * "Dinner with Dos"
  Assets:Bank:Checking       102.00 USD
  Expenses:Restaurant

2013-03-03 * "Dinner with Tres"
  Assets:Bank:Checking       103.00 USD
  Expenses:Restaurant

2014-04-04 * "Dinner with Quatro"
  Assets:Bank:Checking       104.00 USD
  Expenses:Restaurant

"""

def setUp(module):
    # Load the global test input file.
    global entries, errors, options_map
    entries, errors, options_map = loader.load_string(INPUT)


class ExecuteQueryBase(unittest.TestCase):

    maxDiff = 8192

    # Default execution contexts.
    xcontext_entries = cc.FilterEntriesEnvironment()
    xcontext_targets = cc.TargetsEnvironment()
    xcontext_postings = cc.FilterPostingsEnvironment()

    def setUp(self):
        self.parser = q.Parser()

    def parse(self, bql_string):
        """Parse a query.

        Args:
          bql_string: An SQL query to be parsed.
        Returns:
          A parsed statement (Select() node).
        """
        return self.parser.parse(bql_string.strip())

    def compile(self, bql_string):
        """Parse a query and compile it.

        Args:
          bql_string: An SQL query to be parsed.
        Returns:
          A compiled EvalQuery node.
        """
        return c.compile_select(self.parse(bql_string),
                                self.xcontext_targets,
                                self.xcontext_postings,
                                self.xcontext_entries)

    def execute(self, bql_string):
        """Parse a query, execute it and compile it.

        Args:
          bql_string: An SQL query to be parsed.
        Returns:
          A list of ResultRow instances.
        """
        return x.execute_query(self.compile(bql_string),
                               entries, options_map)


class TestFilterEntries(ExecuteQueryBase, cmptest.TestCase):

    def test_filter_empty_from(self):
        # Check that no filter outputs the very same thing.
        filtered_entries = x.filter_entries(self.compile("""
          SELECT * ;
        """).c_from, entries, options_map)
        self.assertEqualEntries(entries, filtered_entries)

    def test_filter_by_year(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM year(date) = 2012;
        """).c_from, entries, options_map)
        self.assertEqualEntries("""

          2012-02-02 * "Dinner with Dos"
            Assets:Bank:Checking              102.00 USD
            Expenses:Restaurant              -102.00 USD

        """, filtered_entries)

    def test_filter_by_expr1(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type
          FROM NOT (type = 'transaction' AND
                    (year(date) = 2012 OR year(date) = 2013));
        """).c_from, entries, options_map)
        self.assertEqualEntries("""

          2010-01-01 open Assets:Bank:Checking
          2010-01-01 open Assets:Bank:Savings
          2010-01-01 open Expenses:Restaurant

          2010-01-01 * "Dinner with Cero"
            Assets:Bank:Checking              100.00 USD
            Expenses:Restaurant              -100.00 USD

          2011-01-01 * "Dinner with Uno"
            Assets:Bank:Checking              101.00 USD
            Expenses:Restaurant              -101.00 USD

          2014-04-04 * "Dinner with Quatro"
            Assets:Bank:Checking              104.00 USD
            Expenses:Restaurant              -104.00 USD

        """, filtered_entries)

    def test_filter_by_expr2(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM date < 2012-06-01;
        """).c_from, entries, options_map)
        self.assertEqualEntries("""

          2010-01-01 open Assets:Bank:Checking
          2010-01-01 open Assets:Bank:Savings
          2010-01-01 open Expenses:Restaurant

          2010-01-01 * "Dinner with Cero"
            Assets:Bank:Checking              100.00 USD
            Expenses:Restaurant              -100.00 USD

          2011-01-01 * "Dinner with Uno"
            Assets:Bank:Checking              101.00 USD
            Expenses:Restaurant              -101.00 USD

          2012-02-02 * "Dinner with Dos"
            Assets:Bank:Checking              102.00 USD
            Expenses:Restaurant              -102.00 USD

        """, filtered_entries)

    def test_filter_close_undated(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM CLEAR;
        """).c_from, entries, options_map)
        self.assertEqualEntries(INPUT + textwrap.dedent("""

          2014-04-04 T "Transfer balance for 'Expenses:Restaurant' (Transfer balance)"
            Expenses:Restaurant                                 510.00 USD
            Equity:Earnings:Current                            -510.00 USD

        """), filtered_entries)

    def test_filter_close_dated(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM CLOSE ON 2013-06-01;
        """).c_from, entries, options_map)
        self.assertEqualEntries(entries[:-1], filtered_entries)


class TestExecute(ExecuteQueryBase):

    def test_non_aggregated(self):
        x = self.execute("""
          SELECT date, flag, payee, narration;
        """)

    def __test_simple(self):
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
