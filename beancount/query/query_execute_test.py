__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import decimal
import io
import unittest
import textwrap
from decimal import Decimal

from beancount.core.number import D
from beancount.core import inventory
from beancount.query import query_parser
from beancount.query import query_compile as qc
from beancount.query import query_env as qe
from beancount.query import query_execute as qx
from beancount.parser import cmptest
from beancount.utils import misc_utils
from beancount import loader


class QueryBase(cmptest.TestCase):

    maxDiff = 8192

    # Default execution contexts.
    xcontext_entries = qe.FilterEntriesEnvironment()
    xcontext_targets = qe.TargetsEnvironment()
    xcontext_postings = qe.FilterPostingsEnvironment()

    def setUp(self):
        super().setUp()
        self.parser = query_parser.Parser()

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
        return qc.compile_select(self.parse(bql_string),
                                 self.xcontext_targets,
                                 self.xcontext_postings,
                                 self.xcontext_entries)

    def check_query(self,
                    input_string, bql_string,
                    expected_types, expected_rows,
                    sort_rows=False,
                    debug=False):

        entries, _, options_map = loader.load_string(input_string)
        query = self.compile(bql_string)
        result_types, result_rows = qx.execute_query(query, entries, options_map)

        if debug:
            with misc_utils.box('result_types'):
                print(result_types)
            with misc_utils.box('result_rows'):
                print(result_rows)
        self.assertEqual(expected_types, result_types)
        if sort_rows:
            result_rows.sort()
        self.assertEqual(expected_rows, result_rows)

    def check_sorted_query(self,
                           input_string, bql_string,
                           expected_types, expected_rows):
        return self.check_query(input_string, bql_string,
                                expected_types, expected_rows, True)


class CommonInputBase:
    INPUT = textwrap.dedent("""

    2010-01-01 open Assets:Bank:Checking
    2010-01-01 open Assets:ForeignBank:Checking
    2010-01-01 open Assets:Bank:Savings

    2010-01-01 open Expenses:Restaurant

    2010-01-01 * "Dinner with Cero"
      Assets:Bank:Checking       100.00 USD
      Expenses:Restaurant       -100.00 USD

    2011-01-01 * "Dinner with Uno"
      Assets:Bank:Checking       101.00 USD
      Expenses:Restaurant       -101.00 USD

    2012-02-02 * "Dinner with Dos"
      Assets:Bank:Checking       102.00 USD
      Expenses:Restaurant       -102.00 USD

    2013-03-03 * "Dinner with Tres"
      Assets:Bank:Checking       103.00 USD
      Expenses:Restaurant       -103.00 USD

    2013-10-10 * "International Transfer"
      Assets:Bank:Checking         -50.00 USD
      Assets:ForeignBank:Checking  -60.00 CAD @ 1.20 USD

    2014-04-04 * "Dinner with Quatro"
      Assets:Bank:Checking       104.00 USD
      Expenses:Restaurant       -104.00 USD

    """)
    def setUp(self):
        super().setUp()
        self.entries, _, self.options_map = loader.load_string(textwrap.dedent(self.INPUT))
        self.context = qx.create_row_context(self.entries, self.options_map)

class TestFilterEntries(CommonInputBase, QueryBase):

    def test_filter_empty_from(self):
        # Check that no filter outputs the very same thing.
        filtered_entries = qx.filter_entries(self.compile("""
          SELECT * ;
        """).c_from, self.entries, self.options_map, self.context)
        self.assertEqualEntries(self.entries, filtered_entries)

    def test_filter_by_year(self):
        filtered_entries = qx.filter_entries(self.compile("""
          SELECT date, type FROM year(date) = 2012;
        """).c_from, self.entries, self.options_map, self.context)
        self.assertEqualEntries("""

          2012-02-02 * "Dinner with Dos"
            Assets:Bank:Checking              102.00 USD
            Expenses:Restaurant              -102.00 USD

        """, filtered_entries)

    def test_filter_by_expr1(self):
        filtered_entries = qx.filter_entries(self.compile("""
          SELECT date, type
          FROM NOT (type = 'transaction' AND
                    (year(date) = 2012 OR year(date) = 2013));
        """).c_from, self.entries, self.options_map, self.context)
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
        filtered_entries = qx.filter_entries(self.compile("""
          SELECT date, type FROM date < 2012-06-01;
        """).c_from, self.entries, self.options_map, self.context)
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
        filtered_entries = qx.filter_entries(self.compile("""
          SELECT date, type FROM CLOSE;
        """).c_from, self.entries, self.options_map, self.context)

        self.assertEqualEntries(self.INPUT + textwrap.dedent("""

          2014-04-04 C "Conversion for (-50.00 USD, -60.00 CAD)"
            Equity:Conversions:Current  50.00 USD @ 0 NOTHING
            Equity:Conversions:Current  60.00 CAD @ 0 NOTHING

        """), filtered_entries)

    def test_filter_close_dated(self):
        filtered_entries = qx.filter_entries(self.compile("""
          SELECT date, type FROM CLOSE ON 2013-06-01;
        """).c_from, self.entries, self.options_map, self.context)
        self.assertEqualEntries(self.entries[:-2], filtered_entries)

    def test_filter_open_dated(self):
        filtered_entries = qx.filter_entries(self.compile("""
          SELECT date, type FROM OPEN ON 2013-01-01;
        """).c_from, self.entries, self.options_map, self.context)

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
        filtered_entries = qx.filter_entries(self.compile("""
          SELECT date, type FROM CLEAR;
        """).c_from, self.entries, self.options_map, self.context)

        self.assertEqualEntries(self.INPUT + textwrap.dedent("""

          2014-04-04 T "Transfer balance for 'Expenses:Restaurant' (Transfer balance)"
            Expenses:Restaurant                                 510.00 USD
            Equity:Earnings:Current                            -510.00 USD

        """), filtered_entries)


class TestExecutePrint(CommonInputBase, QueryBase):

    def test_print_with_filter(self):
        statement = qc.EvalPrint(qc.EvalFrom(qc.EvalEqual(qe.YearEntryColumn(),
                                                          qc.EvalConstant(2012)),
                                             None, None, None))
        oss = io.StringIO()
        qx.execute_print(statement, self.entries, self.options_map, oss)

        self.assertEqualEntries("""

          2012-02-02 * "Dinner with Dos"
            Assets:Bank:Checking                                                   102.00 USD
            Expenses:Restaurant                                                   -102.00 USD

        """, oss.getvalue())

    def test_print_with_no_filter(self):
        statement = qc.EvalPrint(qc.EvalFrom(None, None, None, None))
        oss = io.StringIO()
        qx.execute_print(statement, self.entries, self.options_map, oss)
        self.assertEqualEntries(self.INPUT, oss.getvalue())

        statement = qc.EvalPrint(None)
        oss = io.StringIO()
        qx.execute_print(statement, self.entries, self.options_map, oss)
        self.assertEqualEntries(self.INPUT, oss.getvalue())


class TestAllocation(unittest.TestCase):

    def test_allocator(self):
        allocator = qx.Allocator()
        self.assertEqual(0, allocator.allocate())
        self.assertEqual(1, allocator.allocate())
        self.assertEqual(2, allocator.allocate())
        self.assertEqual([None, None, None], allocator.create_store())


class TestBalanceColumn(unittest.TestCase):

    def test_uses_balance_column(self):
        c_simple = qe.BalanceColumn()
        self.assertTrue(qx.uses_balance_column(c_simple))

        c_simple_not = qe.AccountColumn()
        self.assertFalse(qx.uses_balance_column(c_simple_not))

        c_subexpr = qc.EvalEqual(qe.BalanceColumn(), qc.EvalConstant(2012))
        self.assertTrue(qx.uses_balance_column(c_subexpr))

        c_subexpr_not = qc.EvalEqual(qe.AccountColumn(), qc.EvalConstant('Assets'))
        self.assertFalse(qx.uses_balance_column(c_subexpr_not))


class TestExecuteNonAggregatedQuery(QueryBase):

    INPUT = """

      2010-01-01 open Assets:Bank:Checking
      2010-01-01 open Expenses:Restaurant

      2010-02-23 * "Bla"
        Assets:Bank:Checking       100.00 USD
        Expenses:Restaurant       -100.00 USD

    """

    def test_non_aggregate__one(self):
        self.check_query(
            self.INPUT,
            """
            SELECT date;
            """,
            [('date', datetime.date)],
            [(datetime.date(2010, 2, 23),),
             (datetime.date(2010, 2, 23),)])

    def test_non_aggregate__many(self):
        self.check_query(
            self.INPUT,
            """
            SELECT date, flag, payee, narration;
            """,
            [
                ('date', datetime.date),
                ('flag', str),
                ('payee', str),
                ('narration', str),
                ],
            [
                (datetime.date(2010, 2, 23), '*', '', 'Bla'),
                (datetime.date(2010, 2, 23), '*', '', 'Bla'),
                ])

    def test_non_aggregated_order_by_visible(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account, length(account) ORDER BY 2;
            """,
            [
                ('account', str),
                ('length_account', int),
                ],
            [
                ('Expenses:Restaurant', 19),
                ('Assets:Bank:Checking', 20),
                ])

    def test_non_aggregated_order_by_invisible(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account ORDER BY length(account);
            """,
            [
                ('account', str),
                ],
            [
                ('Expenses:Restaurant',),
                ('Assets:Bank:Checking',),
                ])

    def test_non_aggregated_order_by_none_date(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account ORDER BY cost_date;
            """,
            [
                ('account', str),
                ],
            [
                ('Assets:Bank:Checking',),
                ('Expenses:Restaurant',),
                ])

    def test_non_aggregated_order_by_none_str(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account ORDER BY posting_flag;
            """,
            [
                ('account', str),
                ],
            [
                ('Assets:Bank:Checking',),
                ('Expenses:Restaurant',),
                ])


class TestExecuteAggregatedQuery(QueryBase):

    INPUT = """

      2010-01-01 open Assets:Bank:Checking
      2010-01-01 open Expenses:Restaurant

      2010-02-23 * "Bla"
        Assets:Bank:Checking       100.00 USD
        Expenses:Restaurant       -100.00 USD

    """

    def test_aggregated_group_by_all_implicit(self):
        # There is no group-by, but all columns are aggregations.
        self.check_query(
            self.INPUT,
            """
            SELECT first(account), last(account);
            """,
            [
                ('first_account', str),
                ('last_account', str),
                ],
            [
                ('Assets:Bank:Checking', 'Expenses:Restaurant'),
                ])

    def test_aggregated_group_by_all_explicit(self):
        # All columns ('account', 'len') are subject of a group-by.
        self.check_sorted_query(
            self.INPUT,
            """
            SELECT account, length(account) as len
            GROUP BY account, len;
            """,
            [
                ('account', str),
                ('len', int),
                ],
            [
                ('Assets:Bank:Checking', 20),
                ('Expenses:Restaurant', 19),
                ])

        self.check_sorted_query(
            """
            2010-02-21 * "First"
              Assets:Bank:Checking       -1.00 USD
              Expenses:Restaurant         1.00 USD

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant         2.00 USD
            """,
            """
            SELECT account, length(account) as len
            GROUP BY account, len;
            """,
            [
                ('account', str),
                ('len', int),
                ],
            [
                ('Assets:Bank:Checking', 20),
                ('Expenses:Restaurant', 19),
                ('Liabilities:Credit-Card', 23),
                ])

    def test_aggregated_group_by_visible(self):
        # GROUP-BY: 'account' is visible.
        self.check_sorted_query(
            self.INPUT,
            """
            SELECT account, sum(position) as amount
            GROUP BY account;
            """,
            [
                ('account', str),
                ('amount', inventory.Inventory),
                ],
            [
                ('Assets:Bank:Checking', inventory.from_string('100.00 USD')),
                ('Expenses:Restaurant', inventory.from_string('-100.00 USD')),
                ])

    def test_aggregated_group_by_invisible(self):
        # GROUP-BY: 'account' is invisible.
        self.check_sorted_query(
            self.INPUT,
            """
            SELECT count(position)
            GROUP BY account;
            """,
            [
                ('count_position', int),
                ],
            [
                (1,),
                (1,),
                ])

    def test_aggregated_group_by_visible_order_by_non_aggregate_visible(self):
        # GROUP-BY: 'account' is visible.
        # ORDER-BY: 'account' is a non-aggregate and visible.
        self.check_query(
            self.INPUT,
            """
            SELECT account, sum(position) as amount
            GROUP BY account
            ORDER BY account;
            """,
            [
                ('account', str),
                ('amount', inventory.Inventory),
                ],
            [
                ('Assets:Bank:Checking', inventory.from_string('100.00 USD')),
                ('Expenses:Restaurant', inventory.from_string('-100.00 USD')),
                ])

    def test_aggregated_group_by_visible_order_by_non_aggregate_invisible(self):
        # GROUP-BY: 'account' and 'length(account)' are visible.
        # ORDER-BY: 'length(account)' is a non-aggregate and invisible.
        self.check_query(
            self.INPUT,
            """
            SELECT account, sum(position) as amount
            GROUP BY account, length(account)
            ORDER BY length(account);
            """,
            [
                ('account', str),
                ('amount', inventory.Inventory),
                ],
            [
                ('Expenses:Restaurant', inventory.from_string('-100.00 USD')),
                ('Assets:Bank:Checking', inventory.from_string('100.00 USD')),
                ])

    def test_aggregated_group_by_visible_order_by_aggregate_visible(self):
        # GROUP-BY: 'account' is visible.
        # ORDER-BY: 'sum(account)' is an aggregate and visible.
        self.check_query(
            """
            2010-02-21 * "First"
              Assets:Bank:Checking       -1.00 USD
              Expenses:Restaurant         1.00 USD

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant         2.00 USD
            """,
            """
            SELECT account, count(account) as num, sum(number) as sum
            GROUP BY account
            ORDER BY sum(number);
            """,
            [
                ('account', str),
                ('num', int),
                ('sum', Decimal),
                ],
            [
                ('Liabilities:Credit-Card', 1, D('-2.00')),
                ('Assets:Bank:Checking', 1, D('-1.00')),
                ('Expenses:Restaurant', 2, D('3.00')),
                ])

    def test_aggregated_group_by_visible_order_by_aggregate_invisible(self):
        # GROUP-BY: 'account' is visible.
        # ORDER-BY: 'sum(number)' is an aggregate and invisible.
        self.check_query(
            """
            2010-02-21 * "First"
              Assets:Bank:Checking       -1.00 USD
              Expenses:Restaurant         1.00 USD

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant         2.00 USD
            """,
            """
            SELECT account, count(account) as num
            GROUP BY account
            ORDER BY sum(number);
            """,
            [
                ('account', str),
                ('num', int),
                ],
            [
                ('Liabilities:Credit-Card', 1),
                ('Assets:Bank:Checking', 1),
                ('Expenses:Restaurant', 2),
                ])

    def test_aggregated_group_by_invisible_order_by_non_aggregate_visible(self):
        # GROUP-BY: 'account' is invisible.
        # ORDER-BY: 'len(account)' is a non-aggregate and visible.
        self.check_query(
            self.INPUT,
            """
            SELECT length(account) as len, sum(position) as amount
            GROUP BY account, len
            ORDER BY len;
            """,
            [
                ('len', int),
                ('amount', inventory.Inventory),
                ],
            [
                (19, inventory.from_string('-100.00 USD'),),
                (20, inventory.from_string('100.00 USD'),),
                ])

    def test_aggregated_group_by_invisible_order_by_non_aggregate_invis(self):
        # GROUP-BY: 'account' is invisible.
        # ORDER-BY: 'sum(number)' is an aggregate and invisible.
        self.check_query(
            """
            2010-02-21 * "First"
              Assets:Bank:Checking       -1.00 USD
              Expenses:Restaurant         1.00 USD

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant         2.00 USD
            """,
            """
            SELECT count(account) as num
            GROUP BY account
            ORDER BY sum(number);
            """,
            [
                ('num', int),
                ],
            [
                (1,),
                (1,),
                (2,),
                ])

    def test_aggregated_group_by_invisible_order_by_aggregate_visible(self):
        # GROUP-BY: 'account' is invisible.
        # ORDER-BY: 'sum(account)' is an aggregate and visible.
        self.check_query(
            """
            2010-02-21 * "First"
              Assets:Bank:Checking       -1.00 USD
              Expenses:Restaurant         1.00 USD

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant         2.00 USD
            """,
            """
            SELECT count(account) as num, sum(number) as sum
            GROUP BY account
            ORDER BY sum(number);
            """,
            [
                ('num', int),
                ('sum', Decimal),
                ],
            [
                (1, D('-2.00')),
                (1, D('-1.00')),
                (2, D('3.00')),
                ])

    def test_aggregated_group_by_invisible_order_by_aggregate_invisible(self):
        # GROUP-BY: 'account' is invisible.
        # ORDER-BY: 'sum(number)' is an aggregate and invisible.
        self.check_query(
            """
            2010-02-21 * "First"
              Assets:Bank:Checking       -1.00 USD
              Expenses:Restaurant         1.00 USD

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant         2.00 USD
            """,
            """
            SELECT count(account) as num
            GROUP BY account
            ORDER BY sum(number);
            """,
            [
                ('num', int),
                ],
            [
                (1,),
                (1,),
                (2,),
                ])


class TestExecuteOptions(QueryBase):

    INPUT = """

      2010-02-23 *
        Assets:AssetA       5.00 USD
        Assets:AssetD       2.00 USD
        Assets:AssetB       4.00 USD
        Assets:AssetC       3.00 USD
        Assets:AssetE       1.00 USD
        Equity:Rest       -15.00 USD

    """

    def test_order_by_asc_implicit(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account, number ORDER BY number;
            """,
            [
                ('account', str),
                ('number', Decimal),
                ],
            [
                ('Equity:Rest', D('-15.00')),
                ('Assets:AssetE', D('1.00')),
                ('Assets:AssetD', D('2.00')),
                ('Assets:AssetC', D('3.00')),
                ('Assets:AssetB', D('4.00')),
                ('Assets:AssetA', D('5.00')),
                ])

    def test_order_by_asc_explicit(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account, number ORDER BY number ASC;
            """,
            [
                ('account', str),
                ('number', Decimal),
                ],
            [
                ('Equity:Rest', D('-15.00')),
                ('Assets:AssetE', D('1.00')),
                ('Assets:AssetD', D('2.00')),
                ('Assets:AssetC', D('3.00')),
                ('Assets:AssetB', D('4.00')),
                ('Assets:AssetA', D('5.00')),
                ])

    def test_order_by_desc(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account, number ORDER BY number DESC;
            """,
            [
                ('account', str),
                ('number', Decimal),
                ],
            [
                ('Assets:AssetA', D('5.00')),
                ('Assets:AssetB', D('4.00')),
                ('Assets:AssetC', D('3.00')),
                ('Assets:AssetD', D('2.00')),
                ('Assets:AssetE', D('1.00')),
                ('Equity:Rest', D('-15.00')),
                ])

    def test_distinct(self):
        self.check_sorted_query(
            """
              2010-02-23 *
                Assets:AssetA       5.00 USD
                Assets:AssetA       2.00 USD
                Assets:AssetA       4.00 USD
                Equity:Rest
            """,
            """
            SELECT account ;
            """,
            [
                ('account', str),
                ],
            [
                ('Assets:AssetA',),
                ('Assets:AssetA',),
                ('Assets:AssetA',),
                ('Equity:Rest',),
                ])

        self.check_sorted_query(
            """
              2010-02-23 *
                Assets:AssetA       5.00 USD
                Assets:AssetA       2.00 USD
                Assets:AssetA       4.00 USD
                Equity:Rest        -5.00 USD
                Equity:Rest        -2.00 USD
                Equity:Rest        -4.00 USD
            """,
            """
            SELECT DISTINCT account ;
            """,
            [
                ('account', str),
                ],
            [
                ('Assets:AssetA',),
                ('Equity:Rest',),
                ])

    def test_limit(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account, number ORDER BY number LIMIT 3;
            """,
            [
                ('account', str),
                ('number', Decimal),
                ],
            [
                ('Equity:Rest', D('-15.00')),
                ('Assets:AssetE', D('1.00')),
                ('Assets:AssetD', D('2.00')),
                ])


class TestArithmeticFunctions(QueryBase):

    # You need some transactions in order to eval a simple arithmetic op.
    # This also properly sets the data type to Decimal.
    # In v2, revise this so that this works like a regular DB and support integers.

    def test_add(self):
        self.check_query(
            """
              2010-02-23 *
                Assets:Something       5.00 USD
            """,
            """
              SELECT number + 3 as result;
            """,
            [('result', Decimal)],
            [(D("8"),)])

    def test_sub(self):
        self.check_query(
            """
              2010-02-23 *
                Assets:Something       5.00 USD
            """,
            """
              SELECT number - 3 as result;
            """,
            [('result', Decimal)],
            [(D("2"),)])

    def test_mul(self):
        self.check_query(
            """
              2010-02-23 *
                Assets:Something       5.00 USD
            """,
            """
              SELECT number * 1.2 as result;
            """,
            [('result', Decimal)],
            [(D("6"),)])

    def test_div(self):
        self.check_query(
            """
              2010-02-23 *
                Assets:Something       5.00 USD
            """,
            """
              SELECT number / 2 as result;
            """,
            [('result', Decimal)],
            [(D("2.50"),)])

        # Test dbz, should fail result query.
        with self.assertRaises(decimal.DivisionByZero):
            self.check_query(
                """
                  2010-02-23 *
                    Assets:Something       5.00 USD
                """,
                """
                  SELECT number / 0 as result;
                """,
                [('result', Decimal)],
                [(D("2.50"),)])

    def test_safe_div(self):
        self.check_query(
            """
              2010-02-23 *
                Assets:Something       5.00 USD
            """,
            """
              SELECT SAFEDIV(number, 0) as result;
            """,
            [('result', Decimal)],
            [(D("0"),)])

    def test_safe_div_zerobyzero(self):
        self.check_query(
            """
              2010-02-23 *
                Assets:Something       5.00 USD
            """,
            """
              SELECT SAFEDIV(0.0, 0) as result;
            """,
            [('result', Decimal)],
            [(D("0"),)])



class TestExecuteFlatten(QueryBase):

    def test_flatten_results(self):
        ## FIXME: We need some dedicated tests of flattening results.
        pass

    INPUT = """

      plugin "beancount.plugins.auto_accounts"

      2010-02-23 *
        Assets:Something       5.00 USD
        Assets:Something       2.00 CAD
        Assets:Something       4 HOOL {531.20 USD}
        Equity:Rest

    """

    ## FIXME: Bring this back in.
    def __test_flatten(self):
        self.check_query(
            self.INPUT,
            """
            SELECT account, sum(position)
            WHERE account = 'Assets:Something'
            GROUP BY account
            FLATTEN;
            """,
            [
                ('account', str),
                ('sum_position', inventory.Inventory),
                ],
            [
                ('Assets:Something',
                 inventory.from_string("5.00 USD, 2.00 CAD, 4 HOOL {531.20 USD}")),
                ])


if __name__ == '__main__':
    unittest.main()
