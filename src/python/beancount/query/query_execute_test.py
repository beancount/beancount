import datetime
import io
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
from beancount.parser import parser
from beancount import loader

from beancount.utils.misc_utils import box


INPUT = """

2010-01-01 open Assets:Bank:Checking
2010-01-01 open Assets:ForeignBank:Checking
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

2013-10-10 * "International Transfer"
  Assets:Bank:Checking         -50.00 USD
  Assets:ForeignBank:Checking  -60.00 CAD @ 1.20 USD

2014-04-04 * "Dinner with Quatro"
  Assets:Bank:Checking       104.00 USD
  Expenses:Restaurant

"""

def setUp(module):
    # Load the global test input file.
    global entries, errors, options_map
    entries, errors, options_map = parser.parse_string(INPUT)


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

    def execute_query(self, bql_string):
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
          2010-01-01 open Assets:ForeignBank:Checking

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
          2010-01-01 open Assets:ForeignBank:Checking

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
          SELECT date, type FROM CLOSE;
        """).c_from, entries, options_map)

        self.assertEqualEntries(INPUT + textwrap.dedent("""

          2014-04-04 C "Conversion for Inventory(-50.00 USD, -60.00 CAD)"
            Equity:Conversions:Current                                              50.00 USD                    @ 0.00 NOTHING
            Equity:Conversions:Current                                              60.00 CAD                    @ 0.00 NOTHING

        """), filtered_entries)

    def test_filter_close_dated(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM CLOSE ON 2013-06-01;
        """).c_from, entries, options_map)
        self.assertEqualEntries(entries[:-2], filtered_entries)

    def test_filter_open_dated(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM OPEN ON 2013-01-01;
        """).c_from, entries, options_map)

        self.assertEqualEntries("""

          2010-01-01 open Assets:Bank:Checking
          2010-01-01 open Assets:Bank:Savings
          2010-01-01 open Expenses:Restaurant
          2010-01-01 open Assets:ForeignBank:Checking

          2012-12-31 S "Opening balance for 'Assets:Bank:Checking' (Summarization)"
            Assets:Bank:Checking                                                   303.00 USD
            Equity:Opening-Balances                                               -303.00 USD

          2012-12-31 S "Opening balance for 'Equity:Earnings:Previous' (Summarization)"
            Equity:Earnings:Previous                                              -303.00 USD
            Equity:Opening-Balances                                                303.00 USD

          2013-03-03 * "Dinner with Tres"
            Assets:Bank:Checking                                                   103.00 USD
            Expenses:Restaurant                                                   -103.00 USD

          2013-10-10 * "International Transfer"
            Assets:Bank:Checking                                                   -50.00 USD                                   ;     -50.00 USD
            Assets:ForeignBank:Checking                                            -60.00 CAD                        @ 1.20 USD ;     -72.00 USD

          2014-04-04 * "Dinner with Quatro"
            Assets:Bank:Checking                                                   104.00 USD
            Expenses:Restaurant                                                   -104.00 USD

        """, filtered_entries)

    def test_filter_clear(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM CLEAR;
        """).c_from, entries, options_map)
        self.assertEqualEntries(INPUT + textwrap.dedent("""

          2014-04-04 T "Transfer balance for 'Expenses:Restaurant' (Transfer balance)"
            Expenses:Restaurant                                 510.00 USD
            Equity:Earnings:Current                            -510.00 USD

        """), filtered_entries)


class TestExecutePrint(cmptest.TestCase):

    def test_print_with_filter(self):
        statement = q.Print(c.EvalFrom(c.EvalEqual(cc.YearEntryColumn(),
                                                   c.EvalConstant(2012)),
                                       None, None, None))
        oss = io.StringIO()
        x.execute_print(statement, entries, options_map, oss)

        self.assertEqualEntries("""

          2012-02-02 * "Dinner with Dos"
            Assets:Bank:Checking                                                   102.00 USD
            Expenses:Restaurant                                                   -102.00 USD

        """, oss.getvalue())

    def test_print_with_no_filter(self):
        statement = q.Print(c.EvalFrom(None, None, None, None))
        oss = io.StringIO()
        x.execute_print(statement, entries, options_map, oss)
        self.assertEqualEntries(INPUT, oss.getvalue())

        statement = q.Print(None)
        oss = io.StringIO()
        x.execute_print(statement, entries, options_map, oss)
        self.assertEqualEntries(INPUT, oss.getvalue())


class TestAllocation(unittest.TestCase):

    def test_allocator(self):
        allocator = x.Allocator()
        self.assertEqual(0, allocator.allocate())
        self.assertEqual(1, allocator.allocate())
        self.assertEqual(2, allocator.allocate())
        self.assertEqual([None, None, None], allocator.create_store())


class TestExecuteQuery(ExecuteQueryBase):

    INPUT1 = """

      2010-01-01 open Assets:Bank:Checking
      2010-01-01 open Expenses:Restaurant

      2010-02-23 * "Bla"
        Assets:Bank:Checking       100.00 USD
        Expenses:Restaurant

    """

    def test_non_aggregated_basic_one(self):
        entries, _, options_map = parser.parse_string(self.INPUT1)

        result_types, result_rows = x.execute_query(self.compile("""
          SELECT date;
        """), entries, options_map)

        self.assertEqual([
            ('date', datetime.date),
        ], result_types)

        self.assertEqual([
            (datetime.date(2010, 2, 23),),
            (datetime.date(2010, 2, 23),),
        ], result_rows)

    def test_non_aggregated_basic_many(self):
        entries, _, options_map = parser.parse_string(self.INPUT1)

        result_types, result_rows = x.execute_query(self.compile("""
          SELECT date, flag, payee, narration;
        """), entries, options_map)

        self.assertEqual([
            ('date', datetime.date),
            ('flag', str),
            ('payee', str),
            ('narration', str)
        ], result_types)

        self.assertEqual([
            (datetime.date(2010, 2, 23), '*', '', 'Bla'),
            (datetime.date(2010, 2, 23), '*', '', 'Bla'),
        ], result_rows)





    def __(self):
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
