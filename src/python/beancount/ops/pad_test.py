"""
Unit tests for padding.
"""

import unittest

from beancount.core.position import Lot, Position
from beancount.core.inventory import Inventory
from beancount.core.data import Open, Pad, Balance, Posting
from beancount.core.amount import Decimal, Amount
from beancount.core.realization_test import realizedoc


class __TestPadding(unittest.TestCase):

    def check_real_types(self, real_account, entry_types):
        """Check the types of entries rendered."""
        self.assertEqual(list(map(type, real_account.postings)),
                         entry_types)

    def check_balance(self, real_account, position):
        self.assertEqual(real_account.balance.get_position(position.lot), position)

    @realizedoc
    def test_pad(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balancess

          2013-05-03 check Assets:Checking   172.45 USD
        """
        self.assertEqual(len(errors), 0)
        self.check_real_types(real_accounts['Assets:Checking'], [Open, Pad, Posting, Balance])
        self.check_real_types(real_accounts['Equity:Opening-Balancess'], [Open, Pad, Posting])

        self.check_balance(real_accounts['Assets:Checking'],
                           Position(Lot('USD', None, None), Decimal('172.45')))

    @realizedoc
    def test_pad_fail(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balancess

          2013-05-03 check Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          ;; This should fail here:
          2013-06-01 check Assets:Checking   200 USD

        """
        ## FIXME - if we run check() , this should be true:
        ## self.assertEqual(len(errors), 1)


    @realizedoc
    def test_pad_used_twice(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balancess

          2013-05-03 check Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-05-20 pad  Assets:Checking   Equity:Opening-Balancess

          2013-06-01 check Assets:Checking   200 USD

        """
        self.assertEqual(len(errors), 0)
        self.check_real_types(real_accounts['Assets:Checking'],
                            [Open, Pad, Posting, Balance, Posting, Pad, Posting, Balance])

    @realizedoc
    def test_pad_check_balances(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balancess

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

          2013-06-01 check Assets:Checking   145 USD

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
