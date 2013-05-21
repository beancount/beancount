"""
Unit tests for realizations.
"""

import unittest
import sys
from textwrap import dedent
from datetime import date

from beancount2.inventory import Position, Lot
from beancount2 import parser
from beancount2 import checks
from beancount2 import realization
from beancount2.data import Open, Close, Note, Pad
from beancount2.data import Decimal, Amount
from beancount2.realization import RealPosting, RealPadPosting, RealCheck


class TestRealization(unittest.TestCase):

    def parsetest_check_error(self, contents):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-03 check Assets:US:Checking   100 USD
        """
        errors = checks.check(contents.entries, contents.accounts)
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
        errors = checks.check(contents.entries, contents.accounts)
        self.assertFalse(errors)

        real_accounts, real_errors = realization.realize(contents.entries, True)
        assert len(real_errors) == 0

    # This test ensures that the 'check' directives apply at the beginning of
    # the day.
    def parsetest_check_samedate(self, contents):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-02 check Assets:US:Checking   100 USD
        """
        errors = checks.check(contents.entries, contents.accounts)
        self.assertFalse(errors)

        real_accounts, real_errors = realization.realize(contents.entries, True)
        assert len(real_errors) == 0


parser.create_parsetest_methods(TestRealization)


class TestRealizationPadding(unittest.TestCase):

    def checkRealTypes(self, real_account, real_entry_types):
        """Check the types of entries rendered."""
        self.assertEqual(list(map(type, real_account.postings)),
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
        self.checkRealTypes(real_accounts['Assets:Checking'], [Open, Pad, RealPadPosting, RealCheck])
        self.checkRealTypes(real_accounts['Equity:OpeningBalances'], [Open, Pad, RealPadPosting])

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
        self.checkRealTypes(real_accounts['Assets:Invest'], [Open, Pad, RealPadPosting, RealCheck])
        self.checkRealTypes(real_accounts['Equity:OpeningBalances'], [Open, Pad, RealPadPosting])

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
        self.checkRealTypes(real_accounts['Assets:Invest'], [Open, Pad, RealPadPosting, RealCheck])
        self.checkRealTypes(real_accounts['Equity:OpeningBalances'], [Open, Pad, RealPadPosting])

        final_balance = real_accounts['Assets:Invest'].postings[-1].balance
        self.assertEqual(final_balance.get_positions()[0],
                         Position(Lot('GOOG', Amount('12.00', 'USD'), date(2000, 1, 1)), Decimal('172.45')))


parser.create_parsetest_methods(TestRealizationPadding)


# FIXME: please DO test the realization of a transaction that has multiple legs on the same account!
