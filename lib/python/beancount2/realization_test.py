"""
Unit tests for realizations.
"""

import unittest
import sys
from textwrap import dedent
from datetime import date

from beancount2.inventory import Position, Lot
from beancount2 import parser
from beancount2 import validation
from beancount2 import realization
from beancount2.data import Open, Close, Note, Pad, Check, Transaction
from beancount2.data import Decimal, Amount
from beancount2 import data
from beancount2.realization import RealPosting, RealEntry
from beancount2.inventory import Inventory


class TestRealization(unittest.TestCase):

    def parsetest_check_error(self, contents):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-03 check Assets:US:Checking   100 USD
        """
        errors = validation.validate(contents.entries, contents.accounts)
        self.assertFalse(errors)

        real_accounts, real_errors = realization.realize(contents.entries, True)
        assert len(real_errors) == 1

    def parsetest_check_okay(self, contents):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-03 check Assets:US:Checking   100 USD

        """
        errors = validation.validate(contents.entries, contents.accounts)
        self.assertFalse(errors)

        entries, pad_errors = realization.pad(contents.entries)

        real_accounts, real_errors = realization.realize(entries, True)
        data.print_errors(real_errors)
        self.assertEqual(len(real_errors), 0)

    # This test ensures that the 'check' directives apply at the beginning of
    # the day.
    def parsetest_check_samedate(self, contents):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-02 check Assets:US:Checking     0 USD
          2013-05-03 check Assets:US:Checking   100 USD
        """
        errors = validation.validate(contents.entries, contents.accounts)
        self.assertFalse(errors)

        real_accounts, real_errors = realization.realize(contents.entries, True)
        assert len(real_errors) == 0

parser.create_parsetest_methods(TestRealization)


class TestRealizationPadding(unittest.TestCase):

    def checkRealTypes(self, real_account, real_entry_types):
        """Check the types of entries rendered."""
        self.assertEqual(list(type(rp.entry) for rp in real_account.postings),
                         real_entry_types)

    def parsetest_pad(self, contents):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Checking   Equity:OpeningBalances

          2013-05-03 check Assets:Checking   172.45 USD
        """
        real_accounts, real_errors = realization.realize(contents.entries, True)
        self.assertEqual(len(real_errors), 0)
        self.checkRealTypes(real_accounts['Assets:Checking'], [Open, Pad, Pad, Check])
        self.checkRealTypes(real_accounts['Equity:OpeningBalances'], [Open, Pad, Pad])

        final_balance = real_accounts['Assets:Checking'].postings[-1].balance
        self.assertEqual(final_balance.get_positions()[0],
                         Position(Lot('USD', None, None), Decimal('172.45')))

    def parsetest_with_cost(self, contents):
        """
          2013-05-01 open Assets:Invest
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Invest   Equity:OpeningBalances

          2013-05-03 check Assets:Invest   172.45 GOOG {12.00 USD}
        """
        real_accounts, real_errors = realization.realize(contents.entries, True)
        assert len(real_errors) == 0
        self.checkRealTypes(real_accounts['Assets:Invest'], [Open, Pad, Pad, Check])
        self.checkRealTypes(real_accounts['Equity:OpeningBalances'], [Open, Pad, Pad])

        final_balance = real_accounts['Assets:Invest'].postings[-1].balance
        self.assertEqual(final_balance.get_positions()[0],
                         Position(Lot('GOOG', Amount('12.00', 'USD'), None), Decimal('172.45')))

    def parsetest_with_cost_and_lotdate(self, contents):
        """
          2013-05-01 open Assets:Invest
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Invest   Equity:OpeningBalances

          2013-05-03 check Assets:Invest   172.45 GOOG {12.00 USD / 2000-01-01}
        """
        real_accounts, real_errors = realization.realize(contents.entries, True)
        assert len(real_errors) == 0
        self.checkRealTypes(real_accounts['Assets:Invest'], [Open, Pad, Pad, Check])
        self.checkRealTypes(real_accounts['Equity:OpeningBalances'], [Open, Pad, Pad])

        final_balance = real_accounts['Assets:Invest'].postings[-1].balance
        self.assertEqual(final_balance.get_positions()[0],
                         Position(Lot('GOOG', Amount('12.00', 'USD'), date(2000, 1, 1)), Decimal('172.45')))

    def parsetest_pad_fail(self, contents):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Checking   Equity:OpeningBalances

          2013-05-03 check Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-06-01 check Assets:Checking   200 USD

        """
        real_accounts, real_errors = realization.realize(contents.entries, True)
        self.assertEqual(len(real_errors), 1)


    def parsetest_pad_used_twice(self, contents):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Checking   Equity:OpeningBalances

          2013-05-03 check Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-05-20 pad  Assets:Checking   Equity:OpeningBalances

          2013-06-01 check Assets:Checking   200 USD

        """
        real_accounts, real_errors = realization.realize(contents.entries, True)
        self.assertEqual(len(real_errors), 0)
        self.checkRealTypes(real_accounts['Assets:Checking'],
                            [Open, Pad, Pad, Check, Transaction, Pad, Pad, Check])

    def parsetest_pad_check_balances(self, contents):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Checking   Equity:OpeningBalances

          2013-05-03 txn "Add 20$"
            Assets:Checking             10 USD
            Assets:Cash

          2013-05-10 check Assets:Checking   105 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-05-16 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-06-01 check Assets:Checking   125 USD

        """
        real_accounts, real_errors = realization.realize(contents.entries, True)
        # self.assertEqual(len(real_errors), 0)
        # self.checkRealTypes(real_accounts['Assets:Checking'],
        #                     [Open, Pad, Pad, Check, Transaction, Pad, Pad, Check])

        balances = []
        for real_posting in real_accounts['Assets:Checking'].postings:
            balances.append((type(real_posting.entry),
                             getattr(real_posting, 'balance', Inventory()).get_amount('USD')))

        realization.dump_tree_balances(real_accounts, sys.stderr)

        for entry_type, amount in balances:
            print(entry_type, amount)
        self.assertEqual(balances, [(Open, Amount('0.00', 'USD')),
                                    (Pad, Amount('0.00', 'USD')),
                                    (Transaction, Amount('95.00', 'USD')),
                                    (Transaction, Amount('105.00', 'USD')),
                                    (Check, Amount('105.00', 'USD')),
                                    (Transaction, Amount('125.00', 'USD')),
                                    (Transaction, Amount('145.00', 'USD')),
                                    (Check, Amount('145.00', 'USD'))])


parser.create_parsetest_methods(TestRealizationPadding)


# FIXME: please DO test the realization of a transaction that has multiple legs on the same account!



# FIXME: Write a test that padding a parent account wouldn't pad its child accounts.
