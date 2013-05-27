"""
Unit tests for summarization.
"""

import unittest
import sys
from datetime import date
import textwrap
import functools

from beancount2.inventory import Position, Lot
from beancount2 import parser
from beancount2 import validation
from beancount2 import realization
from beancount2.data import Open, Close, Note, Pad, Check, Transaction
from beancount2.data import Decimal, Amount, Account
from beancount2 import data
from beancount2.realization import RealPosting, RealEntry
from beancount2.inventory import Inventory
from beancount2.parser.parser_test import parsedoc
from beancount2 import summarize



DO_PRINT = object()

def summarizedoc(date, account):
    def summarizedoc_deco(fun):
        """Decorator that parses, pads and summarizes, and realizes the function's
        docstring as an argument."""
        @functools.wraps(fun)
        def newfun(self):
            contents = parser.parse_string(textwrap.dedent(fun.__doc__))
            assert not contents.parse_errors, contents.parse_errors
            entries, pad_errors = realization.pad(contents.entries)
            assert not pad_errors, pad_errors

            real_accounts, real_errors = realization.realize(entries, do_check=True)
            assert not real_errors, real_errors

            before, after = summarize.summarize(entries, date, account)
            sum_entries = before + after
            sum_real_accounts, sum_real_errors = realization.realize(sum_entries, do_check=True)
            assert not sum_real_errors, sum_real_errors

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
                    realization.dump_tree_balances(real_accounts, sys.stdout)
                    print("SUM_REAL")
                    realization.dump_tree_balances(sum_real_accounts, sys.stdout)

        return newfun
    return summarizedoc_deco


OPENING_BALANCES = Account('Equity:OpeningBalances', 'Equity')

class TestSummarization(unittest.TestCase):

    @summarizedoc(date(2012, 1, 1), OPENING_BALANCES)
    def test_sum_basic(self, entries, sum_entries, real_accounts, sum_real_accounts):
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
        self.assertTrue(realization.compare_realizations(real_accounts, sum_real_accounts))
        self.assertTrue(sum_real_accounts[OPENING_BALANCES.name].postings)

    @summarizedoc(date(2012, 1, 1), OPENING_BALANCES)
    def test_with_conversions(self, entries, sum_entries, real_accounts, sum_real_accounts):
        """
          2011-01-01 open Assets:Checking

          2011-01-01 open Income:Job

          2011-02-01 * "Salary"
            Income:Job            -1000 USD
            Assets:Checking        1000 USD

          2011-02-15 * "Conversion"
            Assets:Checking       -1000 USD
            Assets:Checking        1000 CAD @ 1 USD
        """
        self.assertTrue(realization.compare_realizations(real_accounts, sum_real_accounts))
        self.assertTrue(sum_real_accounts[OPENING_BALANCES.name].postings)


    @summarizedoc(date(2012, 1, 1), OPENING_BALANCES)
    def test_limit_date(self, entries, sum_entries, real_accounts, sum_real_accounts):
        """
          2011-01-01 open Assets:Checking
          2011-01-01 open Income:Job

          2011-06-01 * "Salary"
            Income:Job            -1000 USD
            Assets:Checking        1000 USD

          2012-01-01 check Assets:Checking  1000 USD
        """
        self.assertTrue(realization.compare_realizations(real_accounts, sum_real_accounts))
        self.assertTrue(sum_real_accounts[OPENING_BALANCES.name].postings)


class TestTransferBalances(unittest.TestCase):

    @parsedoc
    def test_basic_transfer(self, contents):
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
        tran_entries = summarize.transfer(contents.entries, date(2012, 6, 1), data.is_income_statement_account, OPENING_BALANCES)

        real_accounts, real_errors = realization.realize(contents.entries, do_check=True)
        self.assertEqual(realization.real_cost_as_dict(real_accounts),
                         {'Assets:Checking': 'Inventory(2000.00 USD)',
                          'Income:Job': 'Inventory(-2000.00 USD)'})

        real_accounts, real_errors = realization.realize(tran_entries, do_check=True)
        self.assertEqual(realization.real_cost_as_dict(real_accounts),
                         {'Assets:Checking': 'Inventory(2000.00 USD)',
                          'Income:Job': 'Inventory()',
                          'Equity:OpeningBalances': 'Inventory(-2000.00 USD)'})


class TestOpenAtDate(unittest.TestCase):

    @parsedoc
    def test_open_at_date(self, contents):
        """
          2011-02-01 open Assets:CheckingA
          2011-02-28 open Assets:CheckingB
          2011-02-10 open Assets:CheckingC
          2011-02-20 open Assets:CheckingD
          2011-01-20 open Income:Job

          2011-06-01 * "Salary"
            Income:Job            -1000 USD
            Assets:Checking        1000 USD

        """
        open_entries = summarize.open_at_date(contents.entries, date(2012, 1, 1))
        dates = [entry.date for entry in open_entries]
        self.assertEqual(dates, sorted(dates))
