"""
Unit tests for summarization.
"""

import unittest
from datetime import date
import datetime
import textwrap
import functools
import collections

from beancount.core.inventory import Inventory
from beancount.core import realization
from beancount.core import data
from beancount.core import flags
from beancount.core import compare
from beancount.ops.pad import pad
from beancount.core import realization
from beancount.ops import summarize
from beancount.loader import loaddoc
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import options
from beancount.parser import cmptest
from beancount.utils import test_utils
from beancount import loader


OPENING_BALANCES = 'Equity:Opening-Balances'
TRANSFER_BALANCES = 'Equity:Retained-Earnings'

class TestSummarization(cmptest.TestCase):
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


class TestTransferBalances(cmptest.TestCase):

    @parsedoc
    def test_transfer_balances(self, entries, errors, options_map):
        """
          2011-01-01 open Assets:Checking
          2011-01-01 open Income:Job

          2011-02-01 * "Salary"
            Income:Job            -1000 USD
            Assets:Checking        1000 USD

          2012-02-01 * "Salary Next year"
            Income:Job            -1000 USD
            Assets:Checking        1000 USD
        """
        pass
        # Test with no entries.
        # Test with date.
        # Test without date (at end).



        # real_accounts = realization.realize(entries)
        # self.assertEqual(real_cost_as_dict(real_accounts),
        #                  {'Assets:Checking': 'Inventory(2000.00 USD)',
        #                   'Income:Job': 'Inventory(-2000.00 USD)'})

        # tran_entries = transfer_balances(entries, date(2012, 6, 1),
        #                                  is_income_statement_account, TRANSFER_BALANCES)

        # real_accounts = realization.realize(tran_entries)
        # self.assertEqual(real_cost_as_dict(real_accounts),
        #                  {'Assets:Checking': 'Inventory(2000.00 USD)',
        #                   'Income:Job': 'Inventory()',
        #                   'Equity:Retained-Earnings': 'Inventory(-2000.00 USD)'})





OPENING_ACCOUNT = 'Equity:Opening-Balances'

INPUT_OPEN = """

;; These should be preserved after summarization.
2010-01-01 open  Assets:US:Chase:Checking
2010-01-01 open  Assets:US:Chase:Savings
2010-01-01 open  Assets:US:Investing:GOOG
2010-01-01 open  Assets:CA:BMO:Checking
2010-01-01 open  Liabilities:US:Chase:CreditCard
2010-01-01 open  Income:US:Employer:Salary
2010-01-01 open  Expenses:Taxes
2010-01-01 open  Expenses:Restaurant
2010-01-01 open  Expenses:Flights
2010-01-01 open  Expenses:Internet

"""

INPUT_PRICES_REDUNDANT = """

;; These prices are redundant; only the last price will be preserved after
;; summarization.
2010-02-01 price USD  1.10 CAD
2010-03-01 price USD  1.11 CAD
2010-04-01 price USD  1.12 CAD
2010-05-01 price USD  1.13 CAD
2010-08-01 price USD  1.14 CAD
2010-10-01 price USD  1.15 CAD

"""

INPUT_PRICES_LAST = """

;; This is the last price before the period, will be preserved.
2010-12-01 price USD  1.16 CAD

"""

INPUT_BEFORE = """

;; An account that gets closed before the period, should not appear in the
;; output.

2010-01-01 open  Assets:US:Temporary
2010-11-22 close  Assets:US:Temporary

2010-11-16 *
  Income:US:Employer:Salary     5000 USD
  Assets:US:Chase:Checking      3000 USD
  Expenses:Taxes                2000 USD

2010-11-20 * "First hit on credit card account"
  Liabilities:US:Chase:CreditCard   -67.20 USD
  Expenses:Restaurant

2010-11-26 * "Second hit on credit card account (same account)"
  Liabilities:US:Chase:CreditCard   -345.23 USD
  Expenses:Flights

2010-11-30 *
  Assets:US:Chase:Checking      -80.02 USD
  Expenses:Internet

2010-12-05 * "Unit held at cost"
  Assets:US:Investing:GOOG      5 GOOG {510.00 USD}
  Assets:US:Chase:Checking    -2550 USD

2010-12-05 * "Conversion"
  Assets:US:Chase:Checking    -910 USD
  Assets:CA:BMO:Checking      1000 CAD @ 0.91 USD

2010-12-16 *
  Income:US:Employer:Salary     5000 USD
  Assets:US:Chase:Checking      3000 USD
  Expenses:Taxes                2000 USD

"""

INPUT_PERIOD = """

2011-02-01 price USD  1.17 CAD
2011-04-01 price USD  1.18 CAD

2011-01-16 *
  Income:US:Employer:Salary     5000 USD
  Assets:US:Chase:Checking      3000 USD
  Expenses:Taxes                2000 USD

2011-01-20 * "Dinner at Cull & Pistol"
  Liabilities:US:Chase:CreditCard   -89.23 USD
  Expenses:Restaurant

2011-02-01 open  Assets:US:ETrade:Cash

2011-02-16 *
  Income:US:Employer:Salary     5000 USD
  Assets:US:Chase:Checking      3000 USD
  Expenses:Taxes                2000 USD

"""

# Join all the inputs.
INPUT = (INPUT_OPEN +
         INPUT_PRICES_REDUNDANT +
         INPUT_PRICES_LAST +
         INPUT_BEFORE +
         INPUT_PERIOD)


class TestSummarize(cmptest.TestCase):
    def test_summarize__complete(self):
        entries, errors, options_map = parser.parse_string(INPUT)
        summarize_date = datetime.date(2011, 1, 1)
        summarized_entries, index = summarize.summarize(entries, summarize_date, OPENING_ACCOUNT)

        # Make sure all the active open entries have been preserved.
        self.assertIncludesEntries(INPUT_OPEN, summarized_entries)
        self.assertExcludesEntries(INPUT_BEFORE, summarized_entries)
        self.assertExcludesEntries(INPUT_PRICES_REDUNDANT, summarized_entries)
        self.assertIncludesEntries(INPUT_PRICES_LAST, summarized_entries)
        self.assertIncludesEntries(INPUT_PERIOD, summarized_entries)

        summarizing_entries = [entry
                               for entry in summarized_entries
                               if (isinstance(entry, data.Transaction) and
                                   entry.flag == flags.FLAG_SUMMARIZE)]
        self.assertEqualEntries("""

        2010-12-31 S "Opening balance for 'Assets:CA:BMO:Checking' as of 2010-12-31 (Summarization)"
          Assets:CA:BMO:Checking                                                1000.00 CAD
          Equity:Opening-Balances                                              -1000.00 CAD

        2010-12-31 S "Opening balance for 'Assets:US:Chase:Checking' as of 2010-12-31 (Summarization)"
          Assets:US:Chase:Checking                                              2459.98 USD
          Equity:Opening-Balances                                              -2459.98 USD

        2010-12-31 S "Opening balance for 'Assets:US:Investing:GOOG' as of 2010-12-31 (Summarization)"
          Assets:US:Investing:GOOG                                                5.00 GOOG     {510.00 USD}                  ;    2550.00 USD
          Equity:Opening-Balances                                              -2550.00 USD                                   ;   -2550.00 USD

        2010-12-31 S "Opening balance for 'Expenses:Flights' as of 2010-12-31 (Summarization)"
          Expenses:Flights                                                       345.23 USD
          Equity:Opening-Balances                                               -345.23 USD

        2010-12-31 S "Opening balance for 'Expenses:Internet' as of 2010-12-31 (Summarization)"
          Expenses:Internet                                                       80.02 USD
          Equity:Opening-Balances                                                -80.02 USD

        2010-12-31 S "Opening balance for 'Expenses:Restaurant' as of 2010-12-31 (Summarization)"
          Expenses:Restaurant                                                     67.20 USD
          Equity:Opening-Balances                                                -67.20 USD

        2010-12-31 S "Opening balance for 'Expenses:Taxes' as of 2010-12-31 (Summarization)"
          Expenses:Taxes                                                        4000.00 USD
          Equity:Opening-Balances                                              -4000.00 USD

        2010-12-31 S "Opening balance for 'Income:US:Employer:Salary' as of 2010-12-31 (Summarization)"
          Income:US:Employer:Salary                                            10000.00 USD
          Equity:Opening-Balances                                             -10000.00 USD

        2010-12-31 S "Opening balance for 'Liabilities:US:Chase:CreditCard' as of 2010-12-31 (Summarization)"
          Liabilities:US:Chase:CreditCard                                       -412.43 USD
          Equity:Opening-Balances                                                412.43 USD

        """, summarizing_entries)

        # Check that all the transactions before the index are summarizing ones
        # and dated before the summarizing date.
        before_transactions = [entry
                               for entry in summarized_entries[:index]
                               if isinstance(entry, data.Transaction)]
        self.assertTrue(all(entry.flag == flags.FLAG_SUMMARIZE
                            for entry in before_transactions))
        self.assertTrue(all(entry.date < summarize_date
                            for entry in before_transactions))

        # Check that all the transactions after the index are not summarizing
        # ones and dated after the summarizing date.
        after_transactions = [entry
                              for entry in summarized_entries[index:]
                              if isinstance(entry, data.Transaction)]
        self.assertFalse(any(entry.flag == flags.FLAG_SUMMARIZE
                             for entry in after_transactions))
        self.assertFalse(any(entry.date < summarize_date
                             for entry in after_transactions))


class TestConversions(cmptest.TestCase):

    @parser.parsedoc
    def setUp(self, entries, _, __):
        """
          2012-01-01 open Income:US:Job
          2012-01-01 open Assets:US:Checking
          2012-01-01 open Assets:CA:Invest
          2012-01-01 open Assets:CA:Invest:NT

          2012-03-01 * "Earn some money"
            Income:US:Job            -1000 USD
            Assets:US:Checking        1000 USD

          2012-03-02 * "Transfer to Investment"
            Assets:US:Checking       -800 USD
            Assets:CA:Invest          800 CAD @ 1 USD

          2012-03-03 * "Buy some stock"
            Assets:CA:Invest         -600 CAD
            Assets:CA:Invest:NT        60 NT {10 CAD}

          2012-05-01 * "Transfer some money back"
            Assets:CA:Invest         -100 CAD @ 1 USD
            Assets:US:Checking        100 USD

        """
        self.entries = entries

    def test_conversions__empty(self):
        date =datetime.date(2012, 2, 1)
        conversion_entries = summarize.conversions(self.entries, 'Equity:Conversions', date)
        self.assertEqualEntries(self.entries, conversion_entries)

        converted_balance = realization.compute_entries_balance(conversion_entries, date=date)
        self.assertTrue(converted_balance.get_cost().is_empty())

    def test_conversions__not_needed(self):
        date = datetime.date(2012, 3, 2)
        conversion_entries = summarize.conversions(self.entries, 'Equity:Conversions', date)
        self.assertEqualEntries(self.entries, conversion_entries)

        converted_balance = realization.compute_entries_balance(conversion_entries, date=date)
        self.assertTrue(converted_balance.get_cost().is_empty())

    def test_conversions__needed_middle(self):
        date = datetime.date(2012, 3, 3)
        conversion_entries = summarize.conversions(self.entries, 'Equity:Conversions', date)
        self.assertIncludesEntries(self.entries, conversion_entries)
        self.assertIncludesEntries("""

        2012-03-02 C "Conversion for Inventory(-800.00 USD, 800.00 CAD)"
          Equity:Conversions       800.00 USD @ 0 NOTHING
          Equity:Conversions      -800.00 CAD @ 0 NOTHING

        """, conversion_entries)

        converted_balance = realization.compute_entries_balance(conversion_entries, date=date)
        self.assertTrue(converted_balance.get_cost().is_empty())

    def test_conversions__with_transactions_at_cost(self):
        date = datetime.date(2012, 3, 10)
        conversion_entries = summarize.conversions(self.entries, 'Equity:Conversions', date,
                                                   transfer_currency='XFER')
        self.assertIncludesEntries(self.entries, conversion_entries)
        self.assertIncludesEntries("""

        2012-03-09 C "Conversion for Inventory(-800.00 USD, 200.00 CAD, 60.00 NT {10.00 CAD})"
          Equity:Conversions   800.00 USD  @ 0.00 XFER
          Equity:Conversions  -800.00 CAD  @ 0.00 XFER

        """, conversion_entries)

        converted_balance = realization.compute_entries_balance(conversion_entries, date=date)
        self.assertTrue(converted_balance.get_cost().is_empty())

    def test_conversions__multiple(self):
        date = datetime.date(2012, 5, 10)
        conversion_entries = summarize.conversions(self.entries, 'Equity:Conversions', date)
        self.assertIncludesEntries(self.entries, conversion_entries)
        self.assertIncludesEntries("""

        2012-05-09 C "Conversion for Inventory(-700.00 USD, 100.00 CAD, 60.00 NT {10.00 CAD})"
          Equity:Conversions   700.00 USD  @ 0.00 NOTHING
          Equity:Conversions  -700.00 CAD  @ 0.00 NOTHING

        """, conversion_entries)

        converted_balance = realization.compute_entries_balance(conversion_entries)
        self.assertTrue(converted_balance.get_cost().is_empty())

    def test_conversions__no_date(self):
        conversion_entries = summarize.conversions(self.entries, 'Equity:Conversions')
        self.assertIncludesEntries(self.entries, conversion_entries)
        self.assertIncludesEntries("""

        2012-05-01 C "Conversion for Inventory(-700.00 USD, 100.00 CAD, 60.00 NT {10.00 CAD})"
          Equity:Conversions   700.00 USD  @ 0.00 NOTHING
          Equity:Conversions  -700.00 CAD  @ 0.00 NOTHING

        """, conversion_entries)

        converted_balance = realization.compute_entries_balance(conversion_entries)
        self.assertTrue(converted_balance.get_cost().is_empty())


class TestTruncate(cmptest.TestCase):

    @parser.parsedoc
    def setUp(self, entries, _, __):
        """
        2014-03-10 * "A"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        2014-03-11 * "B"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        2014-03-12 * "C"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        2014-03-13 * "D1"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        2014-03-13 * "D2"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        2014-03-14 * "E"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances
        """
        self.entries = entries

    def test_truncate__before(self):
        truncated_entries = summarize.truncate(self.entries, datetime.date(2014, 2, 15))
        self.assertEqualEntries(self.entries, truncated_entries)

    def test_truncate__normal1(self):
        truncated_entries = summarize.truncate(self.entries, datetime.date(2014, 3, 13))
        self.assertEqualEntries("""

        2014-03-13 * "D1"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        2014-03-13 * "D2"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        2014-03-14 * "E"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        """, truncated_entries)

    def test_truncate__normal2(self):
        truncated_entries = summarize.truncate(self.entries, datetime.date(2014, 3, 14))
        self.assertEqualEntries("""

        2014-03-14 * "E"
          Assets:US:Bank:Checking   1 USD
          Equity:OpeningBalances

        """, truncated_entries)

    def test_truncate__after(self):
        truncated_entries = summarize.truncate(self.entries, datetime.date(2014, 3, 15))
        self.assertEqual([], truncated_entries)


class TestEntriesFromBalance(cmptest.TestCase):

    SOURCE_ACCOUNT = 'Equity:OpeningBalances'
    FILELOC = data.FileLocation('<test>', 0)

    def test_create_entries_from_balances__empty(self):
        balances = collections.defaultdict(Inventory)
        balances['Assets:US:Bank:Empty']
        entries = summarize.create_entries_from_balances(balances, datetime.date.today(),
                                                         self.SOURCE_ACCOUNT, True,
                                                         self.FILELOC, '!', 'narration')
        self.assertEqual([], entries)

    def setUp(self):
        self.balances = collections.defaultdict(Inventory)
        self.balances['Assets:US:Investment'] = Inventory.from_string('10 GOOG {500 USD}')
        self.balances['Assets:US:Bank:Checking'] = Inventory.from_string('1823.23 USD')

    def test_create_entries_from_balances__simple(self):
        entries = summarize.create_entries_from_balances(self.balances, datetime.date(2014, 1, 1),
                                                         self.SOURCE_ACCOUNT, True,
                                                         self.FILELOC, '!', 'Narration for {account} at {date}')
        self.assertEqualEntries("""
          2014-01-01 ! "Narration for Assets:US:Bank:Checking at 2014-01-01"
            Assets:US:Bank:Checking                                               1823.23 USD
            Equity:OpeningBalances                                               -1823.23 USD

          2014-01-01 ! "Narration for Assets:US:Investment at 2014-01-01"
            Assets:US:Investment                                                   10.00 GOOG     {500.00 USD}
            Equity:OpeningBalances                                               -5000.00 USD
        """, entries)

    def test_create_entries_from_balances__reverse(self):
        entries = summarize.create_entries_from_balances(self.balances, datetime.date(2014, 1, 1),
                                                         self.SOURCE_ACCOUNT, False,
                                                         self.FILELOC, '*', 'Narration for {account} at {date}')
        self.assertEqualEntries("""
          2014-01-01 * "Narration for Assets:US:Bank:Checking at 2014-01-01"
            Assets:US:Bank:Checking                                              -1823.23 USD
            Equity:OpeningBalances                                                1823.23 USD

          2014-01-01 * "Narration for Assets:US:Investment at 2014-01-01"
            Assets:US:Investment                                                  -10.00 GOOG     {500.00 USD}
            Equity:OpeningBalances                                                5000.00 USD
        """, entries)


class TestBalanceByAccount(cmptest.TestCase):

    @parser.parsedoc
    def setUp(self, entries, _, __):
        """
        2014-02-01 *
          Assets:AccountA   10 USD
          Equity:OpeningBalances

        2014-03-01 *
          Assets:AccountA   1 USD
          Assets:AccountB  12 USD
          Equity:OpeningBalances
        """
        self.entries = entries

    def test_balance_by_account__no_end_date(self):
        # Test with no end date.
        balances, index = summarize.balance_by_account(self.entries)
        self.assertTrue(index is None)
        self.assertEqual({
            'Assets:AccountA': Inventory.from_string('11 USD'),
            'Equity:OpeningBalances': Inventory.from_string('-23 USD'),
            'Assets:AccountB': Inventory.from_string('12 USD')
            }, balances)

    def test_balance_by_account__first_date(self):
        # Test on the first date (should be empty).
        balances, index = summarize.balance_by_account(self.entries, datetime.date(2014, 2, 1))
        self.assertEqual(0, index)
        self.assertEqual({}, balances)

    def test_balance_by_account__middle(self):
        # Test in the middle.
        balances, index = summarize.balance_by_account(self.entries, datetime.date(2014, 2, 10))
        self.assertEqual(1, index)
        self.assertEqual({
            'Assets:AccountA': Inventory.from_string('10 USD'),
            'Equity:OpeningBalances': Inventory.from_string('-10 USD'),
            }, balances)



class TestOpenAtDate(cmptest.TestCase):

    @parser.parsedoc
    def setUp(self, entries, _, __):
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
        self.entries = entries

    def test_open_at_date__before(self):
        self.assertEqualEntries("""
        """, summarize.open_at_date(self.entries, date(2010, 12, 1)))

    def test_open_at_date__first_entry_open(self):
        # On the day of the first entry is open.
        self.assertEqualEntries("""
        """, summarize.open_at_date(self.entries, date(2011, 1, 1)))

    def test_open_at_date__after_first_entry_open(self):
        # On the day after the first entry is open.
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
        """, summarize.open_at_date(self.entries, date(2011, 1, 2)))

    def test_open_at_date__first_close(self):
        # On the day of the first close.
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
        """, summarize.open_at_date(self.entries, date(2011, 3, 15)))

    def test_open_at_date__after_first_close(self):
        # On the day after the first close.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
        """, summarize.open_at_date(self.entries, date(2011, 3, 16)))

    def test_open_at_date__after_new_opens(self):
        # Other days after new opens.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
          2011-04-01 open Assets:AccountD
          2011-05-01 open Assets:AccountE
        """, summarize.open_at_date(self.entries, date(2011, 5, 3)))

    def test_open_at_date__after_all_opens(self):
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
        """, summarize.open_at_date(self.entries, date(2012, 1, 1)))

    def test_open_at_date__after_all_entries(self):
        # After all entries.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
          2011-04-01 open Assets:AccountD
          2011-05-01 open Assets:AccountE
          2011-06-01 open Assets:AccountF
        """, summarize.open_at_date(self.entries, date(2013, 1, 1)))


    @parser.parsedoc
    def test_open_at_date__duplicate_open(self, entries, errors, _):
        """
          2011-01-01 open Assets:AccountA
          2011-02-01 open Assets:AccountA
        """
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
        """, summarize.open_at_date(entries, date(2013, 1, 1)))

    @parser.parsedoc
    def test_open_at_date__closed_twice(self, entries, errors, _):
        """
          2011-01-01 open  Assets:AccountA
          2011-02-01 close Assets:AccountA
          2011-02-02 close Assets:AccountA
        """
        self.assertEqualEntries("""
        """, summarize.open_at_date(entries, date(2013, 1, 1)))

    @parser.parsedoc
    def test_open_at_date__closed_without_open(self, entries, errors, _):
        """
          2011-02-02 close Assets:AccountA
        """
        self.assertEqualEntries("""
        """, summarize.open_at_date(entries, date(2013, 1, 1)))










## FIXME: Build more good examples to understand, with positions held at cost, as tests


# FIXME: You can redo this in a manual loop now, here, probably.
# def real_cost_as_dict(real_accounts):
#     """Convert a tree of real accounts as a dict for easily doing
#     comparisons for testing."""
#     return {real_account.account: str(real_account.balance.get_cost())
#             for account_name, real_account in real_accounts.items()
#             if real_account.account}



        # account_types = options.get_account_types(options_map)
        # previous_accounts = options.get_previous_accounts(options_map)
        # entries, _ = summarize.clamp(entries,
        #                              datetime.date(2013, 1, 1), datetime.date(2014, 1, 1),
        #                              account_types,
        #                              *previous_accounts)

        # current_accounts = options.get_current_accounts(options_map)
        # entries = summarize.close(entries, account_types, *current_accounts)




__incomplete__ = True
