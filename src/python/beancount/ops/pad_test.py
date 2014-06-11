"""
Unit tests for padding.
"""

import unittest

from beancount.core.position import Lot, Position
from beancount.core.inventory import Inventory
from beancount.core.data import Open, Pad, Balance, Posting
from beancount.core.amount import Decimal, Amount
from beancount.loader import loaddoc
from beancount.parser import parser
from beancount.parser import printer
from beancount.ops import pad
from beancount.ops import check
from beancount.utils import test_utils


class TestPadUtils(unittest.TestCase):

    @parser.parsedoc
    def test_group_postings_by_account(self, entries, _, __):
        """
        2010-01-01 open Assets:Account1
        2010-01-01 open Assets:Account2
        2010-01-01 open Assets:Account3
        2010-01-01 open Equity:OpeningBalances

        2014-01-01 pad Assets:Account1 Equity:OpeningBalances

        2014-06-01 *
          Assets:Account1             1 USD
          Assets:Account2             2 USD
          Assets:Account3            -3 USD

        2014-06-05 balance  Assets:Account2  2 USD
        """
        by_accounts = pad.group_postings_by_account(entries, {'Assets:Account1',
                                                              'Assets:Account2'})
        self.assertEqual({'Assets:Account1', 'Assets:Account2'}, set(by_accounts.keys()))


class TestPadding(test_utils.TestCase):

    @loaddoc
    def test_pad_simple(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:OpeningBalances

          ;; Test the simple case that this directive generates a padding entry.
          2013-05-01 pad Assets:Checking Equity:OpeningBalances

          2013-05-03 balance Assets:Checking                                 172.45 USD
        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad Assets:Checking Equity:OpeningBalances

          ;; Check this is inserted.
          2013-05-01 P "(Padding inserted for Balance of 172.45 USD for difference 172.45 USD)"
            Assets:Checking                                                        172.45 USD
            Equity:OpeningBalances                                                -172.45 USD

          2013-05-03 balance Assets:Checking                                 172.45 USD
        """, entries)


    @loaddoc
    def test_pad_no_overflow(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          ;; Pad before the next check.
          2013-05-01 pad Assets:Checking Equity:OpeningBalances

          ;; The check that is being padded.
          2013-05-03 balance Assets:Checking                                 172.45 USD

          2013-05-15 * "Add 20$"
            Assets:Checking                                                         20.00 USD
            Assets:Cash                                                            -20.00 USD

          ;; This is the next check, should not have been padded.
          2013-06-01 balance Assets:Checking                                 200.00 USD

        """
        self.assertEqual([check.BalanceError], list(map(type, errors)))
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad Assets:Checking Equity:OpeningBalances

          2013-05-01 P "(Padding inserted for Balance of 172.45 USD for difference 172.45 USD)"
            Assets:Checking                                                        172.45 USD
            Equity:OpeningBalances                                                -172.45 USD

          2013-05-03 balance Assets:Checking                                 172.45 USD

          2013-05-15 * "Add 20$"
            Assets:Checking                                                         20.00 USD
            Assets:Cash                                                            -20.00 USD

          2013-06-01 balance Assets:Checking                                 200.00 USD

        """, entries)

## FIXME: Continue here.












    # def check_real_types(self, real_account, entry_types):
    #     """Check the types of entries rendered."""
    #     self.assertEqual(list(map(type, real_account.postings)),
    #                      entry_types)

    # def check_balance(self, real_account, position):
    #     self.assertEqual(real_account.balance.get_position(position.lot), position)

    @loaddoc
    def __test_pad_used_twice(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Checking   Equity:OpeningBalances

          2013-05-03 balance Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-05-20 pad  Assets:Checking   Equity:OpeningBalances

          2013-06-01 balance Assets:Checking   200 USD

        """
        self.assertEqual(len(errors), 0)
        self.check_real_types(real_accounts['Assets:Checking'],
                              [Open, Pad, Posting, Balance, Posting, Pad, Posting, Balance])

    @loaddoc
    def __test_pad_check_balances(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Checking   Equity:OpeningBalances

          2013-05-03 txn "Add 20$"
            Assets:Checking             10 USD
            Assets:Cash

          2013-05-10 balance Assets:Checking   105 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-05-16 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-06-01 balance Assets:Checking   145 USD

        """


        balances = []
        balance = Inventory()
        for posting in real_accounts['Assets:Checking'].postings:
            if isinstance(posting, Posting):
                balance.add_position(posting.position)
            balances.append((type(posting), balance.get_amount('USD')))

        self.assertEqual(balances, [(Open, Amount('0.00', 'USD')),
                                    (Pad, Amount('0.00', 'USD')),
                                    (Posting, Amount('95.00', 'USD')),
                                    (Posting, Amount('105.00', 'USD')),
                                    (Balance, Amount('105.00', 'USD')),
                                    (Posting, Amount('125.00', 'USD')),
                                    (Posting, Amount('145.00', 'USD')),
                                    (Balance, Amount('145.00', 'USD'))])




# FIXME: Write a test that padding a parent account wouldn't pad its child accounts.


__incomplete__ = True
