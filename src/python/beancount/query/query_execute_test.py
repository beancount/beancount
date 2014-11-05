import datetime
import io
import unittest
import textwrap

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.query import query_parser as q
from beancount.query import query_compile as c
from beancount.query import query_env as cc
from beancount.query import query_execute as x
from beancount.parser import cmptest
from beancount.parser import parser
from beancount.utils import misc_utils


class QueryBase(cmptest.TestCase):

    maxDiff = 8192

    # Default execution contexts.
    xcontext_entries = cc.FilterEntriesEnvironment()
    xcontext_targets = cc.TargetsEnvironment()
    xcontext_postings = cc.FilterPostingsEnvironment()

    def setUp(self):
        super().setUp()
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

    def check_query(self,
                    input_string, bql_string,
                    expected_types, expected_rows,
                    sort_rows=False,
                    debug=False):

        entries, _, options_map = parser.parse_string(input_string)
        query = self.compile(bql_string)
        result_types, result_rows = x.execute_query(query, entries, options_map)

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
    def setUp(self):
        super().setUp()
        self.entries, _, self.options_map = parser.parse_string(textwrap.dedent(self.INPUT))


class TestFilterEntries(CommonInputBase, QueryBase):

    def test_filter_empty_from(self):
        # Check that no filter outputs the very same thing.
        filtered_entries = x.filter_entries(self.compile("""
          SELECT * ;
        """).c_from, self.entries, self.options_map)
        self.assertEqualEntries(self.entries, filtered_entries)

    def test_filter_by_year(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM year(date) = 2012;
        """).c_from, self.entries, self.options_map)
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
        """).c_from, self.entries, self.options_map)
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
        """).c_from, self.entries, self.options_map)
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
        """).c_from, self.entries, self.options_map)

        self.assertEqualEntries(self.INPUT + textwrap.dedent("""

          2014-04-04 C "Conversion for Inventory(-50.00 USD, -60.00 CAD)"
            Equity:Conversions:Current                                              50.00 USD                    @ 0.00 NOTHING
            Equity:Conversions:Current                                              60.00 CAD                    @ 0.00 NOTHING

        """), filtered_entries)

    def test_filter_close_dated(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM CLOSE ON 2013-06-01;
        """).c_from, self.entries, self.options_map)
        self.assertEqualEntries(self.entries[:-2], filtered_entries)

    def test_filter_open_dated(self):
        filtered_entries = x.filter_entries(self.compile("""
          SELECT date, type FROM OPEN ON 2013-01-01;
        """).c_from, self.entries, self.options_map)

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
        """).c_from, self.entries, self.options_map)
        self.assertEqualEntries(self.INPUT + textwrap.dedent("""

          2014-04-04 T "Transfer balance for 'Expenses:Restaurant' (Transfer balance)"
            Expenses:Restaurant                                 510.00 USD
            Equity:Earnings:Current                            -510.00 USD

        """), filtered_entries)


class TestExecutePrint(CommonInputBase, QueryBase):

    def test_print_with_filter(self):
        statement = q.Print(c.EvalFrom(c.EvalEqual(cc.YearEntryColumn(),
                                                   c.EvalConstant(2012)),
                                       None, None, None))
        oss = io.StringIO()
        x.execute_print(statement, self.entries, self.options_map, oss)

        self.assertEqualEntries("""

          2012-02-02 * "Dinner with Dos"
            Assets:Bank:Checking                                                   102.00 USD
            Expenses:Restaurant                                                   -102.00 USD

        """, oss.getvalue())

    def test_print_with_no_filter(self):
        statement = q.Print(c.EvalFrom(None, None, None, None))
        oss = io.StringIO()
        x.execute_print(statement, self.entries, self.options_map, oss)
        self.assertEqualEntries(self.INPUT, oss.getvalue())

        statement = q.Print(None)
        oss = io.StringIO()
        x.execute_print(statement, self.entries, self.options_map, oss)
        self.assertEqualEntries(self.INPUT, oss.getvalue())


class TestAllocation(unittest.TestCase):

    def test_allocator(self):
        allocator = x.Allocator()
        self.assertEqual(0, allocator.allocate())
        self.assertEqual(1, allocator.allocate())
        self.assertEqual(2, allocator.allocate())
        self.assertEqual([None, None, None], allocator.create_store())


class TestExecuteNonAggregatedQuery(QueryBase):

    INPUT = """

      2010-01-01 open Assets:Bank:Checking
      2010-01-01 open Expenses:Restaurant

      2010-02-23 * "Bla"
        Assets:Bank:Checking       100.00 USD
        Expenses:Restaurant

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


class TestExecuteAggregatedQuery(QueryBase):

    INPUT = """

      2010-01-01 open Assets:Bank:Checking
      2010-01-01 open Expenses:Restaurant

      2010-02-23 * "Bla"
        Assets:Bank:Checking       100.00 USD
        Expenses:Restaurant

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
              Expenses:Restaurant

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant
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
            SELECT account, sum(change) as amount
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
            SELECT count(change)
            GROUP BY account;
            """,
            [
                ('count_change', int),
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
            SELECT account, sum(change) as amount
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
            SELECT account, sum(change) as amount
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
              Expenses:Restaurant

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant
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
              Expenses:Restaurant

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant
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
            SELECT length(account) as len, sum(change) as amount
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
              Expenses:Restaurant

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant
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
              Expenses:Restaurant

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant
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
              Expenses:Restaurant

            2010-02-23 * "Second"
              Liabilities:Credit-Card    -2.00 USD
              Expenses:Restaurant
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
        Equity:Rest

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
                Equity:Rest
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


class TestExecuteUseCases(QueryBase):
    """Testing all the use cases from the proposal."""





# FIXME: Create test cases for all query_env, including evaluation. The list of
# tests is currently not exhaustive.




# FIXME: This will be a great test query to look at the special 'balance' column:
# select date, narration, account, change where account ~ 'Van.*Cash' ;


# FIXME: This reports an ugly error -- fix this:
# print from has_account ~ 'IRA:Cash' ;



# balances,bal,trial,ledger:
#   SELECT account, sum(change) GROUP BY account;

# balsheet:
#   SELECT account, sum(change)
#   FROM year = 2014  OPEN ON 2014-01-01  CLOSE ON 2015-01-01

# income:
#   SELECT account, sum(change)
#   WHERE account ~ '(Income|Expenses):*'

# journal,register,account:
#   SELECT date, payee, narration, change, balance
#   WHERE account = 'Assets:US:Bank:Checking'

# conversions:
#   SELECT date, payee, narration, change, balance
#   WHERE flag = 'C'
# or
#   WHERE flag = FLAGS.conversion

# documents:
#   SELECT date, account, narration
#   WHERE type = 'Document'

# holdings:
#   SELECT account, currency, cost-currency, sum(change)
#   GROUP BY account, currency, cost-currency

# holdings --by currency:
#   SELECT currency, sum(change)
#   GROUP BY currency
# holdings --by account
#   SELECT account, sum(change)
#   GROUP BY account
# networth,equity:
#   SELECT convert(sum(change), 'USD')
#   SELECT convert(sum(change), 'CAD')
# commodities:
#   SELECT DISTINCT currency
#   SELECT DISTINCT cost-currency
#   SELECT DISTINCT currency, cost-currency
# prices:
#   SELECT date, currency, cost
#   WHERE type = 'Price'
# all_prices:
#   PRINT
#   WHERE type = 'Price'
# check,validate:
#   CHECK
# errors:
#   ERRORS
# print:
#   PRINT WHERE ...
# accounts:
#   SELECT DISTINCT account
# current_events,latest_events:
#   SELECT date, location, narration
#   WHERE type = 'Event'
# events:
#   SELECT location, narration
#   WHERE type = 'Event'
# activity,updated:
#   SELECT account, LATEST(date)
# stats-types:
#   SELECT DISTINCT COUNT(type)
#   SELECT COUNT(DISTINCT type) -- unsure
# stats-directives:
#   SELECT COUNT(id)
# stats-entries:
#   SELECT COUNT(id) WHERE type = 'Transaction'
# stats-postings:
#   SELECT COUNT(*)


# SELECT
#   root_account, AVG(balance)
# FROM (
#   SELECT
#     MAXDEPTH(account, 2) as root_account
#     MONTH(date) as month,
#     SUM(change) as balance
#   WHERE date > 2014-01-01
#   GROUP BY root_account, month
# )
# GROUP BY root_account


# Look at 401k
# select account, sum(units(change)) where account ~ '2014.*401k' group by 1 order by 1;


# FIXME: from mailing-list:
# SELECT account, payee, sum(change)
# WHERE account ~ "Payable" OR account ~ "Receivable" GROUP BY 1, 2;


# FIXME: To render holdings at "average cost", e.g. when aggregating by account,
# you could provide an "AVERAGE(Inventory)" function that merges an inventory's
# lots in the same way that the holdings merge right now. THIS is how to replace
# and remove all holdings support.
