"""
Unit tests for summarization.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"


from datetime import date
import datetime
import collections
import re
import unittest

from beancount.core import inventory
from beancount.core import data
from beancount.core import flags
from beancount.core import interpolate
from beancount.core import convert
from beancount.ops import summarize
from beancount.parser import printer
from beancount.parser import options
from beancount.parser import cmptest
from beancount.utils import misc_utils
from beancount import loader


class TestOpenClose(cmptest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, errors, options_map):
        """
        option "account_previous_earnings"    "Earnings:Previous"
        option "account_previous_balances"    "Opening-Balances"
        option "account_previous_conversions" "Conversions:Previous"

        option "account_current_earnings"     "Earnings:Current"
        option "account_current_conversions"  "Conversions:Current"
        option "conversion_currency"          "NOTHING"

        2012-01-01 open Income:Salary
        2012-01-01 open Expenses:Taxes
        2012-01-01 open Assets:US:Checking
        2012-01-01 open Assets:CA:Checking

        2012-03-01 * "Some income and expense to be summarized"
          Income:Salary        10000 USD
          Expenses:Taxes        3600 USD
          Assets:US:Checking  -13600 USD

        2012-03-02 * "Some conversion to be summarized"
          Assets:US:Checking   -5000 USD @ 1.2 CAD
          Assets:CA:Checking    6000 CAD

        ;; 2012-06-01  BEGIN --------------------------------

        2012-08-01 * "Some income and expense to show"
          Income:Salary        11000 USD
          Expenses:Taxes        3200 USD
          Assets:US:Checking  -14200 USD

        2012-08-02 * "Some other conversion to be summarized"
          Assets:US:Checking   -3000 USD @ 1.25 CAD
          Assets:CA:Checking    3750 CAD

        ;; 2012-09-01  END   --------------------------------

        2012-11-01 * "Some income and expense to be truncated"
          Income:Salary        10000 USD
          Expenses:Taxes        3600 USD
          Assets:US:Checking  -13600 USD

        """
        self.assertFalse(errors)
        self.entries = entries
        self.options_map = options_map

        self.account_types = options.get_account_types(options_map)

    def do_open(self, entries, date, *args):
        return summarize.open(entries, date, *args)

    def do_close(self, entries, date, *args):
        return summarize.close(entries, date, *args)

    def do_clear(self, entries, date, *args):
        return summarize.clear(entries, date, *args)

    def test_open(self):
        date = datetime.date(2012, 6, 1)
        opened_entries, index = self.do_open(self.entries, date,
                                             self.account_types,
                                             'NOTHING',
                                             'Equity:Earnings:Previous',
                                             'Equity:Opening-Balances',
                                             'Equity:Conversions:Previous')

        self.assertEqualEntries("""

        2012-01-01 open Income:Salary
        2012-01-01 open Expenses:Taxes
        2012-01-01 open Assets:US:Checking
        2012-01-01 open Assets:CA:Checking

        2012-05-31 S "Opening balance for 'Assets:CA:Checking' (Summarization)"
          Assets:CA:Checking             6,000 CAD
          Equity:Opening-Balances       -6,000 CAD

        2012-05-31 S "Opening balance for 'Assets:US:Checking' (Summarization)"
          Assets:US:Checking           -18,600 USD
          Equity:Opening-Balances       18,600 USD

        2012-05-31 S "Opening balance for 'Equity:Earnings:Previous' (Summarization)"
          Equity:Earnings:Previous      13,600 USD
          Equity:Opening-Balances      -13,600 USD

        2012-05-31 S "Opening balance for 'Equity:Conversions:Previous' (Summarization)"
          Equity:Conversions:Previous    5,000 USD
          Equity:Opening-Balances       -5,000 USD
          Equity:Conversions:Previous   -6,000 CAD
          Equity:Opening-Balances        6,000 CAD

        ;; 2012-06-01  BEGIN --------------------------------

        2012-08-01 * "Some income and expense to show"
          Income:Salary                 11,000 USD
          Expenses:Taxes                 3,200 USD
          Assets:US:Checking           -14,200 USD

        2012-08-02 * "Some other conversion to be summarized"
          Assets:US:Checking            -3,000 USD @ 1.25 CAD ;  -3,750 CAD
          Assets:CA:Checking             3,750 CAD            ;   3,750 CAD

        2012-11-01 * "Some income and expense to be truncated"
          Income:Salary                 10,000 USD
          Expenses:Taxes                 3,600 USD
          Assets:US:Checking           -13,600 USD

        """, opened_entries)

        # Check the index is correctly beginning after the list of summarizing entries.
        self.assertEqual(8, index)

        # Check that our original example list of entries does not balance.
        input_balance = interpolate.compute_entries_balance(self.entries)
        self.assertFalse(input_balance.is_empty())

        # Check that the summarized entries add up to precisely zero.
        summarized_entries = opened_entries[:index]
        balances = interpolate.compute_entries_balance(summarized_entries)
        self.assertTrue(balances.is_empty())

        # Check further conversions aren't accounted for (the close operation
        # takes care of this).
        opened_balance = interpolate.compute_entries_balance(opened_entries)
        self.assertFalse(opened_balance.is_empty())


    def test_close(self):
        date = datetime.date(2012, 9, 1)
        closed_entries, index = self.do_close(self.entries, date,
                                              'NOTHING',
                                              'Equity:Conversions:Current')

        self.assertEqualEntries("""

        2012-01-01 open Income:Salary
        2012-01-01 open Expenses:Taxes
        2012-01-01 open Assets:US:Checking
        2012-01-01 open Assets:CA:Checking

        2012-03-01 * "Some income and expense to be summarized"
          Income:Salary        10000 USD
          Expenses:Taxes        3600 USD
          Assets:US:Checking  -13600 USD

        2012-03-02 * "Some conversion to be summarized"
          Assets:US:Checking   -5000 USD @ 1.2 CAD
          Assets:CA:Checking    6000 CAD

        ;; 2012-06-01  BEGIN --------------------------------

        2012-08-01 * "Some income and expense to show"
          Income:Salary        11000 USD
          Expenses:Taxes        3200 USD
          Assets:US:Checking  -14200 USD

        2012-08-02 * "Some other conversion to be summarized"
          Assets:US:Checking   -3000 USD @ 1.25 CAD
          Assets:CA:Checking    3750 CAD

        ;; 2012-09-01  END   --------------------------------

        2012-08-31 C "Conversion for (-8000 USD, 9750 CAD)"
          Equity:Conversions:Current    8000 USD  @ 0 NOTHING
          Equity:Conversions:Current   -9750 CAD  @ 0 NOTHING

        """, closed_entries)

        # Check the index is correctly beginning after the list of summarizing entries.
        self.assertEqual(8, index)

        # Check that our original example list of entries does not balance.
        input_balance = interpolate.compute_entries_balance(self.entries)
        self.assertFalse(input_balance.is_empty())

        # Check that the truncated entries does not balance.
        balances = interpolate.compute_entries_balance(closed_entries[:index])
        self.assertFalse(balances.is_empty())

        # Check that the closed entries add up to precisely zero.
        balances = interpolate.compute_entries_balance(closed_entries)
        self.assertTrue(balances.is_empty())


    def test_clear(self):
        date = datetime.date(2013, 1, 1)
        clear_entries, index = self.do_clear(self.entries, date,
                                             self.account_types,
                                             'Equity:Earnings:Current')

        self.assertEqualEntries("""

        2012-01-01 open Income:Salary
        2012-01-01 open Expenses:Taxes
        2012-01-01 open Assets:US:Checking
        2012-01-01 open Assets:CA:Checking

        2012-03-01 * "Some income and expense to be summarized"
          Income:Salary        10000 USD
          Expenses:Taxes        3600 USD
          Assets:US:Checking  -13600 USD

        2012-03-02 * "Some conversion to be summarized"
          Assets:US:Checking   -5000 USD @ 1.2 CAD
          Assets:CA:Checking    6000 CAD

        ;; 2012-06-01  BEGIN --------------------------------

        2012-08-01 * "Some income and expense to show"
          Income:Salary        11000 USD
          Expenses:Taxes        3200 USD
          Assets:US:Checking  -14200 USD

        2012-08-02 * "Some other conversion to be summarized"
          Assets:US:Checking   -3000 USD @ 1.25 CAD
          Assets:CA:Checking    3750 CAD

        ;; 2012-09-01  END   --------------------------------

        2012-11-01 * "Some income and expense to be truncated"
          Income:Salary        10000 USD
          Expenses:Taxes        3600 USD
          Assets:US:Checking  -13600 USD

        2012-12-31 T "Transfer balance for 'Expenses:Taxes' (Transfer balance)"
          Expenses:Taxes           -10400 USD
          Equity:Earnings:Current   10400 USD

        2012-12-31 T "Transfer balance for 'Income:Salary' (Transfer balance)"
          Income:Salary             -31000 USD
          Equity:Earnings:Current    31000 USD

        """, clear_entries)

        # Check the index is correctly beginning after the list of summarizing entries.
        self.assertEqual(9, index)

        # Check that the cleared entries do not necessarily up to precisely zero
        # without closing.
        balances = interpolate.compute_entries_balance(clear_entries)
        self.assertFalse(balances.is_empty())


    def test_open_close_clear(self):
        # Test out the full use case of a balance sheet for a particular year,
        # opening, closing and clearing.
        begin_date = datetime.date(2012, 6, 1)
        end_date = datetime.date(2012, 9, 1)
        clear_date = datetime.date(2013, 1, 1)
        opened_entries, index = self.do_open(self.entries, begin_date,
                                             self.account_types,
                                             'NOTHING',
                                             'Equity:Earnings:Previous',
                                             'Equity:Opening-Balances',
                                             'Equity:Conversions:Previous')

        closed_entries, index = self.do_close(opened_entries, end_date,
                                              'NOTHING',
                                              'Equity:Conversions:Current')

        clear_entries, index = self.do_clear(closed_entries, clear_date,
                                             self.account_types,
                                             'Equity:Earnings:Current')

        self.assertEqualEntries("""

        2012-01-01 open Income:Salary
        2012-01-01 open Expenses:Taxes
        2012-01-01 open Assets:US:Checking
        2012-01-01 open Assets:CA:Checking

        2012-05-31 S "Opening balance for 'Assets:CA:Checking' (Summarization)"
          Assets:CA:Checking            6,000 CAD
          Equity:Opening-Balances      -6,000 CAD

        2012-05-31 S "Opening balance for 'Assets:US:Checking' (Summarization)"
          Assets:US:Checking          -18,600 USD
          Equity:Opening-Balances      18,600 USD

        2012-05-31 S "Opening balance for 'Equity:Earnings:Previous' (Summarization)"
          Equity:Earnings:Previous     13,600 USD
          Equity:Opening-Balances     -13,600 USD

        2012-05-31 S "Opening balance for 'Equity:Conversions:Previous' (Summarization)"
          Equity:Conversions:Previous   5,000 USD
          Equity:Opening-Balances      -5,000 USD
          Equity:Conversions:Previous  -6,000 CAD
          Equity:Opening-Balances       6,000 CAD

        ;; 2012-06-01  BEGIN --------------------------------

        2012-08-01 * "Some income and expense to show"
          Income:Salary                11,000 USD
          Expenses:Taxes                3,200 USD
          Assets:US:Checking          -14,200 USD

        2012-08-02 * "Some other conversion to be summarized"
          Assets:US:Checking           -3,000 USD @ 1.25 CAD ;  -3,750 CAD
          Assets:CA:Checking            3,750 CAD            ;   3,750 CAD

        ;; 2012-09-01  END   --------------------------------

        2012-08-31 C "Conversion for (-3000 USD, 3750 CAD)"
          Equity:Conversions:Current    3,000 USD @ 0 NOTHING
          Equity:Conversions:Current   -3,750 CAD @ 0 NOTHING

        2012-12-31 T "Transfer balance for 'Income:Salary' (Transfer balance)"
          Income:Salary               -11,000 USD
          Equity:Earnings:Current      11,000 USD

        2012-12-31 T "Transfer balance for 'Expenses:Taxes' (Transfer balance)"
          Expenses:Taxes               -3,200 USD
          Equity:Earnings:Current       3,200 USD

        """, clear_entries)


class TestOpenCloseWithOptions(TestOpenClose):
    "Same test as the previous, but invoking all with options."

    def do_open(self, entries, date, *args):
        return summarize.open_opt(entries, date, self.options_map)

    def do_close(self, entries, date, *args):
        return summarize.close_opt(entries, date, self.options_map)

    def do_clear(self, entries, date, *args):
        return summarize.clear_opt(entries, date, self.options_map)


class TestClamp(cmptest.TestCase):

    @loader.load_doc()
    def test_clamp(self, entries, errors, options_map):
        """
        2012-01-01 open Income:Salary
        2012-01-01 open Expenses:Taxes
        2012-01-01 open Assets:US:Checking
        2012-01-01 open Assets:CA:Checking

        2012-03-01 * "Some income and expense to be summarized"
          Income:Salary        10000.00 USD
          Expenses:Taxes        3600.00 USD
          Assets:US:Checking  -13600.00 USD

        2012-03-02 * "Some conversion to be summarized"
          Assets:US:Checking   -5000.00 USD @ 1.2 CAD
          Assets:CA:Checking    6000.00 CAD

        ;; 2012-06-01  BEGIN --------------------------------

        2012-08-01 * "Some income and expense to show"
          Income:Salary        11000.00 USD
          Expenses:Taxes        3200.00 USD
          Assets:US:Checking  -14200.00 USD

        2012-08-02 * "Some other conversion to be summarized"
          Assets:US:Checking   -3000.00 USD @ 1.25 CAD
          Assets:CA:Checking    3750.00 CAD

        ;; 2012-09-01  END   --------------------------------

        2012-11-01 * "Some income and expense to be truncated"
          Income:Salary        10000.00 USD
          Expenses:Taxes        3600.00 USD
          Assets:US:Checking  -13600.00 USD

        """
        self.assertFalse(errors)

        begin_date = datetime.date(2012, 6, 1)
        end_date = datetime.date(2012, 9, 1)
        account_types = options.get_account_types(options_map)
        clamped_entries, index = summarize.clamp(entries, begin_date, end_date,
                                                 account_types,
                                                 'NOTHING',
                                                 'Equity:Earnings',
                                                 'Equity:Opening-Balances',
                                                 'Equity:Conversions')
        self.assertEqualEntries("""

        2012-01-01 open Income:Salary
        2012-01-01 open Expenses:Taxes
        2012-01-01 open Assets:US:Checking
        2012-01-01 open Assets:CA:Checking

        2012-05-31 S "Opening balance for 'Assets:CA:Checking' (Summarization)"
          Assets:CA:Checking              6000.00 CAD
          Equity:Opening-Balances         -6000.00 CAD

        2012-05-31 S "Opening balance for 'Assets:US:Checking' (Summarization)"
          Assets:US:Checking            -18600.00 USD
          Equity:Opening-Balances         18600.00 USD

        2012-05-31 S "Opening balance for 'Equity:Earnings' (Summarization)"
          Equity:Earnings                13600.00 USD
          Equity:Opening-Balances        -13600.00 USD

        ;; 2012-06-01  BEGIN --------------------------------

        2012-08-01 * "Some income and expense to show"
          Income:Salary                  11000.00 USD
          Expenses:Taxes                  3200.00 USD
          Assets:US:Checking            -14200.00 USD

        2012-08-02 * "Some other conversion to be summarized"
          Assets:US:Checking             -3000.00 USD  @ 1.25 CAD
          Assets:CA:Checking              3750.00 CAD

        ;; 2012-09-01  END   --------------------------------

        2012-08-31 C "Conversion for (-3000.00 USD, 3750.00 CAD)"
          Equity:Conversions              3000.00 USD  @ 0 NOTHING
          Equity:Conversions             -3750.00 CAD  @ 0 NOTHING

        """, clamped_entries)

        self.assertEqual(7, index)

        input_balance = interpolate.compute_entries_balance(entries)
        self.assertFalse(input_balance.is_empty())

        clamped_balance = interpolate.compute_entries_balance(clamped_entries)
        self.assertTrue(clamped_balance.is_empty())


class TestCap(cmptest.TestCase):

    @loader.load_doc()
    def test_cap(self, entries, errors, options_map):
        """
        plugin "beancount.plugins.auto_accounts"

        2014-03-01 * "Some income and expense"
          Income:Salary        10000.00 USD
          Expenses:Taxes        3500.00 USD
          Assets:US:Checking

        2014-02-01 * "Some conversion"
          Assets:US:Checking   -5000.00 USD @ 1.2 CAD
          Assets:CA:Checking    6000.00 CAD
        """
        self.assertFalse(errors)
        account_types = options.get_account_types(options_map)
        capd_entries = summarize.cap(entries, account_types,
                                     'NOTHING',
                                     'Equity:Earnings',
                                     'Equity:Conversions')

        self.assertIncludesEntries(entries, capd_entries)
        self.assertIncludesEntries("""

        2014-03-01 T "Transfer balance for 'Expenses:Taxes' (Transfer balance)"
          Expenses:Taxes       -3500.00 USD
          Equity:Earnings       3500.00 USD

        2014-03-01 T "Transfer balance for 'Income:Salary' (Transfer balance)"
          Income:Salary       -10000.00 USD
          Equity:Earnings      10000.00 USD

        2014-03-01 C "Conversion for (-5000.00 USD, 6000.00 CAD)"
          Equity:Conversions    5000.00 USD @ 0 NOTHING
          Equity:Conversions   -6000.00 CAD @ 0 NOTHING

        """, capd_entries)
        self.assertEqual(9, len(capd_entries))


INPUT_PLUGINS = """

option "plugin_processing_mode" "raw"

"""

INPUT_OPEN = """

;; These should be preserved after summarization.
2010-01-01 open  Assets:US:Chase:Checking
2010-01-01 open  Assets:US:Investing:HOOL
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

;; An account that gets capped before the period, should not appear in the
;; output.

2010-01-01 open  Assets:US:Temporary
2010-11-22 close  Assets:US:Temporary

2010-11-16 *
  Income:US:Employer:Salary    -5000.00 USD
  Assets:US:Chase:Checking      3000.00 USD
  Expenses:Taxes                2000.00 USD

2010-11-20 * "First hit on credit card account"
  Liabilities:US:Chase:CreditCard   -67.20 USD
  Expenses:Restaurant                67.20 USD

2010-11-26 * "Second hit on credit card account (same account)"
  Liabilities:US:Chase:CreditCard   -345.23 USD
  Expenses:Flights                   345.23 USD

2010-11-30 *
  Assets:US:Chase:Checking      -80.02 USD
  Expenses:Internet              80.02 USD

2010-12-05 * "Unit held at cost"
  Assets:US:Investing:HOOL        5 HOOL {510.00 USD}
  Assets:US:Chase:Checking    -2550 USD

2010-12-05 * "Conversion"
  Assets:US:Chase:Checking    -910.00 USD
  Assets:CA:BMO:Checking      1000.00 CAD @ 0.91 USD

2010-12-16 *
  Income:US:Employer:Salary    -5000.00 USD
  Assets:US:Chase:Checking      3000.00 USD
  Expenses:Taxes                2000.00 USD

"""

INPUT_PERIOD = """

2011-02-01 price USD  1.17 CAD
2011-04-01 price USD  1.18 CAD

2011-01-16 *
  Income:US:Employer:Salary    -5000.00 USD
  Assets:US:Chase:Checking      3000.00 USD
  Expenses:Taxes                2000.00 USD

2011-01-20 * "Dinner at Cull & Pistol"
  Liabilities:US:Chase:CreditCard   -89.23 USD
  Expenses:Restaurant                89.23 USD

2011-02-01 open  Assets:Cash

2011-02-02 * "Cafe Mogador"
  Expenses:Restaurant      37.92 USD
  Assets:Cash             -37.92 USD

2011-02-16 *
  Income:US:Employer:Salary    -5000.00 USD
  Assets:US:Chase:Checking      3000.00 USD
  Expenses:Taxes                2000.00 USD

"""

INPUT_PERIOD_REMOVED = """

2011-03-15 balance Assets:US:Chase:Checking    8459.98 USD

"""

# Join all the inputs.
INPUT = (INPUT_PLUGINS +
         INPUT_OPEN +
         INPUT_PRICES_REDUNDANT +
         INPUT_PRICES_LAST +
         INPUT_BEFORE +
         INPUT_PERIOD +
         INPUT_PERIOD_REMOVED)


class TestTransferBalances(cmptest.TestCase):

    TRANSFER_ACCOUNT = 'Equity:Transfer'

    def setUp(self):
        self.entries, errors, __ = loader.load_string(INPUT)
        printer.print_errors(errors)
        self.assertFalse(errors)

    def test_transfer_balances__empty(self):
        xfer_entries = summarize.transfer_balances(
            [], datetime.date(2011, 1, 1),
            lambda account: account.startswith('Assets:US:Chase'),
            self.TRANSFER_ACCOUNT)
        self.assertEqual([], xfer_entries)

    def test_transfer_balances__middle_assets(self):
        date = datetime.date(2011, 1, 1)
        xfer_entries = summarize.transfer_balances(
            self.entries, date,
            lambda account: account.startswith('Assets:US:Chase'),
            self.TRANSFER_ACCOUNT)
        self.assertIncludesEntries((INPUT_OPEN +
                                    INPUT_PRICES_REDUNDANT +
                                    INPUT_PRICES_LAST +
                                    INPUT_BEFORE +
                                    INPUT_PERIOD), xfer_entries)
        self.assertIncludesEntries("""

        2010-12-31 T "Transfer balance for 'Assets:US:Chase:Checking' (Transfer balance)"
          Assets:US:Chase:Checking                                             -2459.98 USD
          Equity:Transfer                                                       2459.98 USD

        """, xfer_entries)
        self.assertEqual(len(self.entries) + 1 - 1, len(xfer_entries))

    def test_transfer_balances__middle_at_cost(self):
        date = datetime.date(2011, 1, 1)
        xfer_entries = summarize.transfer_balances(
            self.entries, date,
            lambda account: account.startswith('Assets:US:Investing'),
            self.TRANSFER_ACCOUNT)
        self.assertIncludesEntries(self.entries, xfer_entries)
        self.assertIncludesEntries("""

        2010-12-31 T "Transfer balance for 'Assets:US:Investing:HOOL' (Transfer balance)"
          Assets:US:Investing:HOOL         -5 HOOL {510.00 USD, 2010-12-05} ;   -2550.00 USD
          Equity:Transfer             2550.00 USD                           ;    2550.00 USD

        """, xfer_entries)
        self.assertEqual(len(self.entries) + 1, len(xfer_entries))

    def test_transfer_balances__end_assets_implicit(self):
        xfer_entries = summarize.transfer_balances(
            self.entries, datetime.date(2011, 3, 1),
            lambda account: account.startswith('Assets:US:Chase'),
            self.TRANSFER_ACCOUNT)
        self.assertIncludesEntries((INPUT_OPEN +
                                    INPUT_PRICES_REDUNDANT +
                                    INPUT_PRICES_LAST +
                                    INPUT_BEFORE +
                                    INPUT_PERIOD), xfer_entries)
        self.assertIncludesEntries("""

        2011-02-28 T "Transfer balance for 'Assets:US:Chase:Checking' (Transfer balance)"
          Assets:US:Chase:Checking                                             -8459.98 USD
          Equity:Transfer                                                       8459.98 USD

        """, xfer_entries)
        self.assertEqual(len(self.entries) + 1 - 1, len(xfer_entries))

    def test_transfer_balances__end_assets_explicit(self):
        xfer_entries = summarize.transfer_balances(
            self.entries, None,
            lambda account: account.startswith('Assets:US:Chase'),
            self.TRANSFER_ACCOUNT)
        self.assertIncludesEntries(self.entries, xfer_entries)
        self.assertIncludesEntries("""

        2011-04-01 T "Transfer balance for 'Assets:US:Chase:Checking' (Transfer balance)"
          Assets:US:Chase:Checking                                             -8459.98 USD
          Equity:Transfer                                                       8459.98 USD

        """, xfer_entries)
        self.assertEqual(len(self.entries) + 1, len(xfer_entries))

    def test_transfer_balances__middle_income(self):
        date = datetime.date(2011, 1, 1)
        xfer_entries = summarize.transfer_balances(
            self.entries, date,
            lambda account: re.match('(Income|Expenses):', account),
            self.TRANSFER_ACCOUNT)
        self.assertIncludesEntries(self.entries, xfer_entries)
        self.assertIncludesEntries("""

        2010-12-31 T "Transfer balance for 'Expenses:Flights' (Transfer balance)"
          Expenses:Flights                                                      -345.23 USD
          Equity:Transfer                                                        345.23 USD

        2010-12-31 T "Transfer balance for 'Expenses:Internet' (Transfer balance)"
          Expenses:Internet                                                      -80.02 USD
          Equity:Transfer                                                         80.02 USD

        2010-12-31 T "Transfer balance for 'Expenses:Restaurant' (Transfer balance)"
          Expenses:Restaurant                                                    -67.20 USD
          Equity:Transfer                                                         67.20 USD

        2010-12-31 T "Transfer balance for 'Expenses:Taxes' (Transfer balance)"
          Expenses:Taxes                                                       -4000.00 USD
          Equity:Transfer                                                       4000.00 USD

        2010-12-31 T "Transfer balance for 'Income:US:Employer:Salary' (Transfer balance)"
          Income:US:Employer:Salary                                            10000.00 USD
          Equity:Transfer                                                     -10000.00 USD

        """, xfer_entries)
        self.assertEqual(len(self.entries) + 5, len(xfer_entries))


class TestSummarize(cmptest.TestCase):

    OPENING_ACCOUNT = 'Equity:Opening-Balances'

    def test_summarize__complete(self):
        entries, errors, options_map = loader.load_string(INPUT)
        self.assertFalse(errors)
        summarize_date = datetime.date(2011, 1, 1)
        summarized_entries, index = summarize.summarize(entries, summarize_date,
                                                        self.OPENING_ACCOUNT)

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

        2010-12-31 S "Opening balance for 'Assets:CA:BMO:Checking' (Summarization)"
          Assets:CA:BMO:Checking                                                1000.00 CAD
          Equity:Opening-Balances                                              -1000.00 CAD

        2010-12-31 S "Opening balance for 'Assets:US:Chase:Checking' (Summarization)"
          Assets:US:Chase:Checking                                              2459.98 USD
          Equity:Opening-Balances                                              -2459.98 USD

        2010-12-31 S "Opening balance for 'Assets:US:Investing:HOOL' (Summarization)"
          Assets:US:Investing:HOOL                                                5 HOOL     {510.00 USD, 2010-12-05}  ;    2550.00 USD
          Equity:Opening-Balances                                              -2550.00 USD                            ;   -2550.00 USD

        2010-12-31 S "Opening balance for 'Expenses:Flights' (Summarization)"
          Expenses:Flights                                                       345.23 USD
          Equity:Opening-Balances                                               -345.23 USD

        2010-12-31 S "Opening balance for 'Expenses:Internet' (Summarization)"
          Expenses:Internet                                                       80.02 USD
          Equity:Opening-Balances                                                -80.02 USD

        2010-12-31 S "Opening balance for 'Expenses:Restaurant' (Summarization)"
          Expenses:Restaurant                                                     67.20 USD
          Equity:Opening-Balances                                                -67.20 USD

        2010-12-31 S "Opening balance for 'Expenses:Taxes' (Summarization)"
          Expenses:Taxes                                                        4000.00 USD
          Equity:Opening-Balances                                              -4000.00 USD

        2010-12-31 S "Opening balance for 'Income:US:Employer:Salary' (Summarization)"
          Income:US:Employer:Salary                                           -10000.00 USD
          Equity:Opening-Balances                                              10000.00 USD

        2010-12-31 S "Opening balance for 'Liabilities:US:Chase:CreditCard' (Summarization)"
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

    @loader.load_doc()
    def test_summarize__ordering_non_transactions(self, entries, _, __):
        """
          2016-01-15 price HOOL    123.45 USD

          2016-02-01 open Assets:Invest:Cash
          2016-02-01 open Assets:Invest:HOOL

          2016-02-16 *
            Assets:Invest:HOOL    10 HOOL {143.45 USD}
            Assets:Invest:Cash

          2016-04-01 *
            Assets:Invest:HOOL      2 HOOL {156.32 USD}
            Assets:Invest:Cash
        """
        summarize_date = datetime.date(2016, 3, 1)
        summarized_entries, index = summarize.summarize(entries, summarize_date,
                                                        self.OPENING_ACCOUNT)
        self.assertTrue(misc_utils.is_sorted(summarized_entries, lambda entry: entry.date))


class TestConversions(cmptest.TestCase):

    ACCOUNT = 'Equity:Conversions'

    @loader.load_doc()
    def setUp(self, entries, _, __):
        """
          2012-01-01 open Income:US:Job
          2012-01-01 open Assets:US:Checking
          2012-01-01 open Assets:CA:Invest
          2012-01-01 open Assets:CA:Invest:NT

          2012-03-01 * "Earn some money"
            Income:US:Job            -1000.00 USD
            Assets:US:Checking        1000.00 USD

          2012-03-02 * "Transfer to Investment"
            Assets:US:Checking       -800.00 USD
            Assets:CA:Invest          800.00 CAD @ 1 USD

          2012-03-03 * "Buy some stock"
            Assets:CA:Invest         -600.00 CAD
            Assets:CA:Invest:NT        60 NT {10 CAD}

          2012-05-01 * "Transfer some money back"
            Assets:CA:Invest         -100.00 CAD @ 1 USD
            Assets:US:Checking        100.00 USD

        """
        self.entries = entries

    def test_conversions__empty(self):
        date = datetime.date(2012, 2, 1)
        conversion_entries = summarize.conversions(self.entries, self.ACCOUNT,
                                                   'NOTHING', date)
        self.assertEqualEntries(self.entries, conversion_entries)

        converted_balance = interpolate.compute_entries_balance(conversion_entries,
                                                                date=date)
        self.assertTrue(converted_balance.reduce(convert.get_cost).is_empty())

    def test_conversions__not_needed(self):
        date = datetime.date(2012, 3, 2)
        conversion_entries = summarize.conversions(self.entries, self.ACCOUNT,
                                                   'NOTHING', date)
        self.assertEqualEntries(self.entries, conversion_entries)

        converted_balance = interpolate.compute_entries_balance(conversion_entries,
                                                                date=date)
        self.assertTrue(converted_balance.reduce(convert.get_cost).is_empty())

    def test_conversions__needed_middle(self):
        date = datetime.date(2012, 3, 3)
        conversion_entries = summarize.conversions(self.entries, self.ACCOUNT,
                                                   'NOTHING', date)
        self.assertIncludesEntries(self.entries, conversion_entries)
        self.assertIncludesEntries("""

        2012-03-02 C "Conversion for (-800.00 USD, 800.00 CAD)"
          Equity:Conversions       800.00 USD @ 0 NOTHING
          Equity:Conversions      -800.00 CAD @ 0 NOTHING

        """, conversion_entries)

        converted_balance = interpolate.compute_entries_balance(conversion_entries,
                                                                date=date)
        self.assertTrue(converted_balance.reduce(convert.get_cost).is_empty())

    def test_conversions__with_transactions_at_cost(self):
        date = datetime.date(2012, 3, 10)
        conversion_entries = summarize.conversions(self.entries, self.ACCOUNT,
                                                   'XFER', date)
        self.assertIncludesEntries(self.entries, conversion_entries)
        self.assertIncludesEntries("""

        2012-03-09 C "Conversion for (-800.00 USD, 200.00 CAD, 60 NT {10 CAD, 2012-03-03})"
          Equity:Conversions   800.00 USD  @ 0 XFER
          Equity:Conversions  -800.00 CAD  @ 0 XFER

        """, conversion_entries)

        converted_balance = interpolate.compute_entries_balance(conversion_entries,
                                                                date=date)
        self.assertTrue(converted_balance.reduce(convert.get_cost).is_empty())

    def test_conversions__multiple(self):
        date = datetime.date(2012, 5, 10)
        conversion_entries = summarize.conversions(self.entries, self.ACCOUNT,
                                                   'NOTHING', date)

        self.assertIncludesEntries(self.entries, conversion_entries)
        self.assertIncludesEntries("""

        2012-05-09 C "Conversion for (-700.00 USD, 100.00 CAD, 60 NT {10 CAD, 2012-03-03})"
          Equity:Conversions   700.00 USD  @ 0 NOTHING
          Equity:Conversions  -700.00 CAD  @ 0 NOTHING

        """, conversion_entries)

        converted_balance = interpolate.compute_entries_balance(conversion_entries)
        self.assertTrue(converted_balance.reduce(convert.get_cost).is_empty())

    def test_conversions__no_date(self):
        conversion_entries = summarize.conversions(self.entries, self.ACCOUNT,
                                                   'NOTHING')
        self.assertIncludesEntries(self.entries, conversion_entries)
        self.assertIncludesEntries("""

        2012-05-01 C "Conversion for (-700.00 USD, 100.00 CAD, 60 NT {10 CAD, 2012-03-03})"
          Equity:Conversions   700.00 USD  @ 0 NOTHING
          Equity:Conversions  -700.00 CAD  @ 0 NOTHING

        """, conversion_entries)

        converted_balance = interpolate.compute_entries_balance(conversion_entries)
        self.assertTrue(converted_balance.reduce(convert.get_cost).is_empty())

    @loader.load_doc()
    def test_conversions__non_empty_but_empty_cost(self, entries, _, __):
        """
          2012-01-01 open Assets:Checking
          2012-01-01 open Assets:Invest

          2012-03-01 *
            Assets:Checking        -800.00 USD
            Assets:Invest           40 HOOL {20.00 USD}
        """
        conversion_entries = summarize.conversions(entries, self.ACCOUNT, 'XFER')
        self.assertEqualEntries(entries, conversion_entries)

        converted_balance = interpolate.compute_entries_balance(entries)
        self.assertTrue(converted_balance.reduce(convert.get_cost).is_empty())


class TestTruncate(cmptest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, _, __):
        """
        2014-01-01 open Assets:US:Bank:Checking
        2014-01-01 open Equity:Opening-Balances

        2014-03-10 * "A"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-11 * "B"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-12 * "C"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-13 * "D1"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-13 * "D2"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-14 * "E"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD
        """
        self.entries = entries

    def test_truncate__before(self):
        truncated_entries = summarize.truncate(self.entries, datetime.date(2014, 2, 15))
        self.assertEqualEntries("""
        2014-01-01 open Assets:US:Bank:Checking
        2014-01-01 open Equity:Opening-Balances
        """, truncated_entries)

    def test_truncate__normal1(self):
        truncated_entries = summarize.truncate(self.entries, datetime.date(2014, 3, 13))
        self.assertEqualEntries("""

        2014-01-01 open Assets:US:Bank:Checking
        2014-01-01 open Equity:Opening-Balances

        2014-03-10 * "A"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-11 * "B"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-12 * "C"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        """, truncated_entries)

    def test_truncate__normal2(self):
        truncated_entries = summarize.truncate(self.entries, datetime.date(2014, 3, 14))
        self.assertEqualEntries("""

        2014-01-01 open Assets:US:Bank:Checking
        2014-01-01 open Equity:Opening-Balances

        2014-03-10 * "A"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-11 * "B"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-12 * "C"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-13 * "D1"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        2014-03-13 * "D2"
          Assets:US:Bank:Checking   1 USD
          Equity:Opening-Balances  -1 USD

        """, truncated_entries)

    def test_truncate__after(self):
        truncated_entries = summarize.truncate(self.entries, datetime.date(2014, 3, 15))
        self.assertEqual(self.entries, truncated_entries)


class TestEntriesFromBalance(cmptest.TestCase):

    SOURCE_ACCOUNT = 'Equity:Opening-Balances'
    META = data.new_metadata('<test>', 0)

    def test_create_entries_from_balances__empty(self):
        balances = collections.defaultdict(inventory.Inventory)
        _ = balances['Assets:US:Bank:Empty']
        entries = summarize.create_entries_from_balances(balances, datetime.date.today(),
                                                         self.SOURCE_ACCOUNT, True,
                                                         self.META, '!', 'narration')
        self.assertEqual([], entries)

    def setUp(self):
        self.balances = collections.defaultdict(inventory.Inventory)
        self.balances['Assets:US:Investment'] = (
            inventory.from_string('10 HOOL {500.00 USD, 2014-01-01}'))
        self.balances['Assets:US:Bank:Checking'] = inventory.from_string('1823.23 USD')

    def test_create_entries_from_balances__simple(self):
        entries = summarize.create_entries_from_balances(
            self.balances, datetime.date(2014, 1, 1),
            self.SOURCE_ACCOUNT, True,
            self.META, '!', 'Narration for {account} at {date}')
        self.assertEqualEntries("""
          2014-01-01 ! "Narration for Assets:US:Bank:Checking at 2014-01-01"
            Assets:US:Bank:Checking                                               1823.23 USD
            Equity:Opening-Balances                                               -1823.23 USD

          2014-01-01 ! "Narration for Assets:US:Investment at 2014-01-01"
            Assets:US:Investment                                                   10 HOOL     {500.00 USD}
            Equity:Opening-Balances                                               -5000.00 USD
        """, entries)

    def test_create_entries_from_balances__reverse(self):
        entries = summarize.create_entries_from_balances(
            self.balances, datetime.date(2014, 1, 1),
            self.SOURCE_ACCOUNT, False,
            self.META, '*', 'Narration for {account} at {date}')
        self.assertEqualEntries("""
          2014-01-01 * "Narration for Assets:US:Bank:Checking at 2014-01-01"
            Assets:US:Bank:Checking                                              -1823.23 USD
            Equity:Opening-Balances                                                1823.23 USD

          2014-01-01 * "Narration for Assets:US:Investment at 2014-01-01"
            Assets:US:Investment                                                  -10 HOOL     {500.00 USD}
            Equity:Opening-Balances                                                5000.00 USD
        """, entries)


class TestBalanceByAccount(cmptest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, _, __):
        """
        2001-01-01 open Assets:AccountA
        2001-01-01 open Assets:AccountB
        2001-01-01 open Equity:Opening-Balances

        2014-02-01 *
          Assets:AccountA   10 USD
          Equity:Opening-Balances

        2014-03-01 *
          Assets:AccountA   1 USD
          Assets:AccountB  12 USD
          Equity:Opening-Balances
        """
        self.entries = entries

    def test_balance_by_account__no_end_date(self):
        # Test with no end date.
        balances, index = summarize.balance_by_account(self.entries)
        self.assertEqual(len(self.entries), index)
        self.assertEqual({
            'Assets:AccountA': inventory.from_string('11 USD'),
            'Equity:Opening-Balances': inventory.from_string('-23 USD'),
            'Assets:AccountB': inventory.from_string('12 USD')
            }, balances)

    def test_balance_by_account__first_date(self):
        # Test on the first date (should be empty).
        balances, index = summarize.balance_by_account(self.entries,
                                                       datetime.date(2014, 2, 1))
        self.assertEqual(3, index)
        self.assertEqual({}, balances)

    def test_balance_by_account__middle(self):
        # Test in the middle.
        balances, index = summarize.balance_by_account(self.entries,
                                                       datetime.date(2014, 2, 10))
        self.assertEqual(4, index)
        self.assertEqual({
            'Assets:AccountA': inventory.from_string('10 USD'),
            'Equity:Opening-Balances': inventory.from_string('-10 USD'),
            }, balances)



class TestOpenAtDate(cmptest.TestCase):

    @loader.load_doc()
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

    def test_get_open_entries__before(self):
        self.assertEqualEntries("""
        """, summarize.get_open_entries(self.entries, date(2010, 12, 1)))

    def test_get_open_entries__first_entry_open(self):
        # On the day of the first entry is open.
        self.assertEqualEntries("""
        """, summarize.get_open_entries(self.entries, date(2011, 1, 1)))

    def test_get_open_entries__after_first_entry_open(self):
        # On the day after the first entry is open.
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
        """, summarize.get_open_entries(self.entries, date(2011, 1, 2)))

    def test_get_open_entries__first_close(self):
        # On the day of the first close.
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
        """, summarize.get_open_entries(self.entries, date(2011, 3, 15)))

    def test_get_open_entries__after_first_close(self):
        # On the day after the first close.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
        """, summarize.get_open_entries(self.entries, date(2011, 3, 16)))

    def test_get_open_entries__after_new_opens(self):
        # Other days after new opens.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
          2011-04-01 open Assets:AccountD
          2011-05-01 open Assets:AccountE
        """, summarize.get_open_entries(self.entries, date(2011, 5, 3)))

    def test_get_open_entries__after_all_opens(self):
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
        """, summarize.get_open_entries(self.entries, date(2012, 1, 1)))

    def test_get_open_entries__after_all_entries(self):
        # After all entries.
        self.assertEqualEntries("""
          2011-02-01 open Assets:AccountB
          2011-03-01 open Assets:AccountC
          2011-04-01 open Assets:AccountD
          2011-05-01 open Assets:AccountE
          2011-06-01 open Assets:AccountF
        """, summarize.get_open_entries(self.entries, date(2013, 1, 1)))


    @loader.load_doc(expect_errors=True)
    def test_get_open_entries__duplicate_open(self, entries, errors, _):
        """
          2011-01-01 open Assets:AccountA
          2011-02-01 open Assets:AccountA
        """
        self.assertEqualEntries("""
          2011-01-01 open Assets:AccountA
        """, summarize.get_open_entries(entries, date(2013, 1, 1)))

    @loader.load_doc(expect_errors=True)
    def test_get_open_entries__closed_twice(self, entries, errors, _):
        """
          2011-01-01 open  Assets:AccountA
          2011-02-01 close Assets:AccountA
          2011-02-02 close Assets:AccountA
        """
        self.assertEqualEntries("""
        """, summarize.get_open_entries(entries, date(2013, 1, 1)))

    @loader.load_doc(expect_errors=True)
    def test_get_open_entries__closed_without_open(self, entries, errors, _):
        """
          2011-02-02 close Assets:AccountA
        """
        self.assertEqualEntries("""
        """, summarize.get_open_entries(entries, date(2013, 1, 1)))


if __name__ == '__main__':
    unittest.main()
