"""
Unit tests for summarization.
"""

import unittest
from datetime import date
import datetime
import textwrap
import functools

from beancount.core import realization
from beancount.ops.pad import pad
from beancount.core import realization
from beancount.ops import summarize
from beancount.loader import loaddoc
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import options
from beancount.utils import test_utils


OPENING_BALANCES = 'Equity:Opening-Balances'
TRANSFER_BALANCES = 'Equity:Retained-Earnings'

class TestSummarization(unittest.TestCase):
    pass

#    @summarizedoc(date(2012, 1, 1), OPENING_BALANCES)
#    def test_sum_basic(self, entries, sum_entries, real_accounts, sum_real_accounts):
#        """
#          2011-01-01 open Assets:Checking
#
#          2011-01-01 open Income:Job
#
#          2011-02-01 * "Salary"
#            Income:Job            -1000 USD
#            Assets:Checking        1000 USD
#
#          2012-02-01 * "Salary Next year"
#            Income:Job            -1000 USD
#            Assets:Checking        1000 USD
#        """
#        print('-' * 80)
#        for x in real_accounts: print(x)
#
#        print('-' * 80)
#        for x in sum_real_accounts: print(x)
#
#        print('-' * 80)
#        for e in sum_real_accounts[OPENING_BALANCES]:
#            print(type(e))
#            for posting in e.postings:
#                print(posting)
#
#        # self.assertTrue(compare_realizations(real_accounts, sum_real_accounts))
#        # self.assertTrue(sum_real_accounts[OPENING_BALANCES].postings)

#     @summarizedoc(date(2012, 1, 1), OPENING_BALANCES)
#     def test_with_conversions(self, entries, sum_entries, real_accounts,
#                               sum_real_accounts):
#         """
#           2011-01-01 open Assets:Checking
#
#           2011-01-01 open Income:Job
#
#           2011-02-01 * "Salary"
#             Income:Job            -1000 USD
#             Assets:Checking        1000 USD
#
#           2011-02-15 * "Conversion"
#             Assets:Checking       -1000 USD
#             Assets:Checking        1000 CAD @ 1 USD
#         """
#         self.assertTrue(compare_realizations(real_accounts, sum_real_accounts))
#         self.assertTrue(sum_real_accounts[OPENING_BALANCES].postings)
#
#
#     @summarizedoc(date(2012, 1, 1), OPENING_BALANCES)
#     def test_limit_date(self, entries, sum_entries, real_accounts, sum_real_accounts):
#         """
#           2011-01-01 open Assets:Checking
#           2011-01-01 open Income:Job
#
#           2011-06-01 * "Salary"
#             Income:Job            -1000 USD
#             Assets:Checking        1000 USD
#
#           2012-01-01 balance Assets:Checking  1000 USD
#         """
#         self.assertTrue(compare_realizations(real_accounts, sum_real_accounts))
#         self.assertTrue(sum_real_accounts[OPENING_BALANCES].postings)
#
#     @parsedoc
#     def test_transfer_and_summarization(self, entries, errors, options_map):
#         """
#           2011-01-01 open Assets:Checking
#           2011-01-01 open Income:Job
#           2011-01-01 open Expenses:Restaurant
#
#           2011-06-01 * "Salary"
#             Income:Job            -1000 USD
#             Assets:Checking        1000 USD
#
#           2011-06-02 * "Eating out"
#             Expenses:Restaurant      80 USD
#             Assets:Checking
#
#           2012-01-01 balance Assets:Checking  920 USD
#
#           2012-06-01 * "Salary"
#             Income:Job            -1000 USD
#             Assets:Checking        1000 USD
#         """
#
#         entries, pad_errors = pad(entries)
#
#         report_date = date(2012, 1, 1)
#         tran_entries = transfer_balances(entries, report_date,
#                                          is_income_statement_account, TRANSFER_BALANCES)
#
#         sum_entries, _ = summarize(tran_entries, report_date, OPENING_BALANCES)
#         real_accounts = realization.realize(sum_entries)
#
#         self.assertEqual(real_cost_as_dict(real_accounts),
#                          {'Assets:Checking': 'Inventory(1920.00 USD)',
#                           'Equity:Opening-Balancess': 'Inventory()',
#                           'Equity:Retained-Earnings': 'Inventory(-920.00 USD)',
#                           'Expenses:Restaurant': 'Inventory()',
#                           'Income:Job': 'Inventory(-1000.00 USD)'})
#
#
# class TestTransferBalances(unittest.TestCase):
#
#     @parsedoc
#     def test_basic_transfer(self, entries, errors, options_map):
#         """
#           2011-01-01 open Assets:Checking
#           2011-01-01 open Income:Job
#
#           2011-02-01 * "Salary"
#             Income:Job            -1000 USD
#             Assets:Checking        1000 USD
#
#           2012-02-01 * "Salary Next year"
#             Income:Job            -1000 USD
#             Assets:Checking        1000 USD
#         """
#         real_accounts = realization.realize(entries)
#         self.assertEqual(real_cost_as_dict(real_accounts),
#                          {'Assets:Checking': 'Inventory(2000.00 USD)',
#                           'Income:Job': 'Inventory(-2000.00 USD)'})
#
#         tran_entries = transfer_balances(entries, date(2012, 6, 1),
#                                          is_income_statement_account, TRANSFER_BALANCES)
#
#         real_accounts = realization.realize(tran_entries)
#         self.assertEqual(real_cost_as_dict(real_accounts),
#                          {'Assets:Checking': 'Inventory(2000.00 USD)',
#                           'Income:Job': 'Inventory()',
#                           'Equity:Retained-Earnings': 'Inventory(-2000.00 USD)'})




class TestSumToDate(test_utils.TestCase):

    @parser.parsedoc
    def test_sum_to_date(self, entries, errors, options_map):
        """
        """




class TestOpenAtDate(test_utils.TestCase):

    @parser.parsedoc
    def test_open_at_date(self, entries, errors, options_map):
        """
          2011-01-01 open Assets:AccountA
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC

          2011-03-15 close Assets:AccountA

          2011-04-01 open Assets:AccountD
          2011-05-01 open Assets:AccountE
          2011-06-01 open Assets:AccountF
          2011-07-01 open Assets:AccountG
          2011-08-01 open Assets:AccountH
          2011-09-01 open Assets:AccountI
          2011-10-01 open Assets:AccountJ
          2011-11-01 open Assets:AccountK
          2011-12-01 open Assets:AccountL

          2012-07-01 close Assets:AccountG
          2012-07-01 close Assets:AccountH
          2012-07-01 close Assets:AccountI
          2012-07-01 close Assets:AccountJ
          2012-07-01 close Assets:AccountK
          2012-07-01 close Assets:AccountL

        """
        self.assertTrue(entries)

        # Before all entries.
        self.assertEqualEntries("""
        """, summarize.open_at_date(entries, date(2010, 12, 1)))

        # On the day of the first entry is open.
        self.assertEqualEntries("""
        """, summarize.open_at_date(entries, date(2011, 1, 1)))

        # On the day after the first entry is open.
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
        """, summarize.open_at_date(entries, date(2011, 1, 2)))

        # On the day of the first close.
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
        """, summarize.open_at_date(entries, date(2011, 3, 15)))

        # On the day after the first close.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
        """, summarize.open_at_date(entries, date(2011, 3, 16)))

        # Other days after new opens.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
          2011-04-01 open Assets:AccountD
          2011-05-01 open Assets:AccountE
        """, summarize.open_at_date(entries, date(2011, 5, 3)))

        # After all opens.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
          2011-04-01 open Assets:AccountD
          2011-05-01 open Assets:AccountE
          2011-06-01 open Assets:AccountF
          2011-07-01 open Assets:AccountG
          2011-08-01 open Assets:AccountH
          2011-09-01 open Assets:AccountI
          2011-10-01 open Assets:AccountJ
          2011-11-01 open Assets:AccountK
          2011-12-01 open Assets:AccountL
        """, summarize.open_at_date(entries, date(2012, 1, 1)))

        # After all entries.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
          2011-04-01 open Assets:AccountD
          2011-05-01 open Assets:AccountE
          2011-06-01 open Assets:AccountF
        """, summarize.open_at_date(entries, date(2013, 1, 1)))

    @parser.parsedoc
    def test_open_at_date_duplicate_open(self, entries, errors, options_map):
        """
          2011-01-01 open Assets:AccountA
          2011-02-01 open Assets:AccountA
        """
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
        """, summarize.open_at_date(entries, date(2013, 1, 1)))

    @parser.parsedoc
    def test_open_at_date_closed_twice(self, entries, errors, options_map):
        """
          2011-01-01 open  Assets:AccountA
          2011-02-01 close Assets:AccountA
          2011-02-02 close Assets:AccountA
        """
        self.assertEqualEntries("""
        """, summarize.open_at_date(entries, date(2013, 1, 1)))

    @parser.parsedoc
    def test_open_at_date_closed_without_open(self, entries, errors, options_map):
        """
          2011-02-02 close Assets:AccountA
        """
        self.assertEqualEntries("""
        """, summarize.open_at_date(entries, date(2013, 1, 1)))







class TestConversions(unittest.TestCase):
    """
    Test conversions insertion.
    """

    @loaddoc
    def test_basic_conversions(self, entries, errors, options_map):
        """
          2013-02-01 open Income:US:Job           USD
          2013-02-01 open Assets:US:Checking      USD
          2013-02-01 open Assets:CA:Invest        CAD
          2013-02-01 open Assets:CA:Invest:NT     NT

          2011-03-01 * "Earn some money"
            Income:US:Job            -1000 USD
            Assets:US:Checking        1000 USD

          2012-03-02 * "Transfer to Investment"
            Assets:US:Checking       -800 USD
            Assets:CA:Invest          800 CAD @ 1 USD

          2012-03-03 * "Buy some stock"
            Assets:CA:Invest         -600 CAD
            Assets:CA:Invest:NT        60 NT {10 CAD}

          2013-02-01 * "Transfer some money back"
            Assets:CA:Invest         -100 CAD @ 1 USD
            Assets:US:Checking        100 USD

        """
        account_types = options.get_account_types(options_map)
        previous_accounts = options.get_previous_accounts(options_map)
        entries, _ = summarize.clamp(entries,
                                     datetime.date(2013, 1, 1), datetime.date(2014, 1, 1),
                                     account_types,
                                     *previous_accounts)

        current_accounts = options.get_current_accounts(options_map)
        entries = summarize.close(entries, account_types, *current_accounts)

        # entries = conversions(entries, ACCOUNT_CONVERSIONS1, datetime.date(2013, 1, 1))
        # entries = conversions(entries, ACCOUNT_CONVERSIONS2)

        converted_balance = realization.compute_entries_balance(entries)
        #print(converted_balance.get_cost())
        # assert converted_balance.get_cost().is_empty()

        real_accounts = realization.realize(entries)
        #realization.dump_tree_balances(real_accounts, sys.stdout)



## FIXME: Build more good examples to understand, with positions held at cost, as tests


# FIXME: You can redo this in a manual loop now, here, probably.
# def real_cost_as_dict(real_accounts):
#     """Convert a tree of real accounts as a dict for easily doing
#     comparisons for testing."""
#     return {real_account.account: str(real_account.balance.get_cost())
#             for account_name, real_account in real_accounts.items()
#             if real_account.account}






DO_PRINT = object()

def summarizedoc(date, other_account):

    def summarizedoc_deco(fun):
        """Decorator that parses, pads and summarizes, and realizes the function's
        docstring as an argument."""
        @functools.wraps(fun)
        def newfun(self):
            entries, parse_errors, options_map = parser.parse_string(
                textwrap.dedent(fun.__doc__))
            assert not parse_errors, parse_errors
            entries, pad_errors = pad(entries)
            assert not pad_errors, pad_errors

            real_accounts = realization.realize(entries)

            sum_entries, _ = summarize(entries, date, other_account)
            sum_real_accounts = realization.realize(sum_entries)

            # print('---')
            # for entry in before: print(entry)
            # print('---')
            # for entry in after: print(entry)
            # print('---')

            result = None
            try:
                result = fun(self, entries, sum_entries, real_accounts, sum_real_accounts)
            except AssertionError:
                result = DO_PRINT
                raise
            finally:
                if result is DO_PRINT:
                    print("REAL")
                    print(realization.dump_balances(real_accounts))
                    print("SUM_REAL")
                    print(realization.dump_balances(sum_real_accounts))

        return newfun
    return summarizedoc_deco




__incomplete__ = True
