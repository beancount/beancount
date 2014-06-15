"""
Unit tests for padding.
"""
from beancount.core.inventory import Inventory
from beancount.core.data import Open, Pad, Balance, Posting
from beancount.core.amount import Amount
from beancount.core import realization
from beancount.loader import loaddoc
from beancount.ops import pad
from beancount.ops import balance
from beancount.utils import test_utils


class TestPadding(test_utils.TestCase):

    @loaddoc
    def test_pad_simple(self, entries, errors, __):
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
    def test_pad_no_overflow(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          ;; Pad before the next check.
          2013-05-01 pad Assets:Checking Equity:OpeningBalances

          ;; The check that is being padded.
          2013-05-03 balance Assets:Checking                                 172.45 USD

          2013-05-15 * "Add 20$"
            Assets:Checking                                                   20.00 USD
            Assets:Cash                                                      -20.00 USD

          ;; This is the next check, should not have been padded.
          2013-06-01 balance Assets:Checking                                 200.00 USD

        """
        self.assertEqual([balance.BalanceError], list(map(type, errors)))
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

    @loaddoc
    def test_pad_used_twice_legally(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          ;; First pad.
          2013-05-01 pad  Assets:Checking   Equity:OpeningBalances

          2013-05-03 balance Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          ;; Second pad.
          2013-05-20 pad  Assets:Checking   Equity:OpeningBalances

          2013-06-01 balance Assets:Checking   200 USD

        """
        self.assertFalse(errors)
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

          2013-05-20 pad Assets:Checking Equity:OpeningBalances

          2013-05-20 P "(Padding inserted for Balance of 200.00 USD for difference 7.55 USD)"
            Assets:Checking                                                          7.55 USD
            Equity:OpeningBalances                                                  -7.55 USD

          2013-06-01 balance Assets:Checking                                 200.00 USD

        """, entries)

    @loaddoc
    def test_pad_used_twice_illegally(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-03 balance Assets:Checking   0.00 USD

          ;; Two pads in between checks.
          2013-05-10 pad  Assets:Checking   Equity:OpeningBalances
          2013-05-20 pad  Assets:Checking   Equity:OpeningBalances

          2013-06-01 balance Assets:Checking   200 USD

        """
        self.assertEqual([pad.PadError], list(map(type, errors)))
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-03 balance Assets:Checking                                 0.00 USD

          2013-05-10 pad Assets:Checking Equity:OpeningBalances

          2013-05-20 pad Assets:Checking Equity:OpeningBalances

          2013-05-20 P "(Padding inserted for Balance of 200.00 USD for difference 200.00 USD)"
            Assets:Checking                                                        200.00 USD
            Equity:OpeningBalances                                                -200.00 USD

          2013-06-01 balance Assets:Checking                                 200.00 USD

        """, entries)

    @loaddoc
    def test_pad_unused(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-10 * "Add 200$"
            Assets:Checking       200.00 USD
            Assets:Cash          -200.00 USD

          ;; This pad will do nothing, should raise a warning..
          2013-05-20 pad  Assets:Checking   Equity:OpeningBalances

          2013-06-01 balance Assets:Checking   200.0 USD

        """
        self.assertEqual([pad.PadError], list(map(type, errors)))
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-10 * "Add 200$"
            Assets:Checking                                                        200.00 USD
            Assets:Cash                                                           -200.00 USD

          2013-05-20 pad Assets:Checking Equity:OpeningBalances

          2013-06-01 balance Assets:Checking                                 200.00 USD

        """, entries)

    @loaddoc
    def test_pad_parents(self, entries, errors, __):
        """

          2013-05-01 open Assets:US
          2013-05-01 open Assets:US:Bank1:Checking
          2013-05-01 open Assets:US:Bank1:Savings
          2013-05-01 open Assets:US:Bank2:Checking
          2013-05-01 open Assets:US:Bank2:Savings
          2013-05-01 open Equity:OpeningBalances

          2013-05-10 *
            Assets:US:Bank1:Checking                                 1.00 USD
            Assets:US:Bank1:Savings                                  2.00 USD
            Assets:US:Bank2:Checking                                 3.00 USD
            Assets:US:Bank2:Savings                                  4.00 USD
            Equity:OpeningBalances                                 -10.00 USD

          2013-05-20 pad Assets:US Equity:OpeningBalances

          2013-06-01 balance Assets:US                                       100.00 USD

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

          2013-05-01 open Assets:US
          2013-05-01 open Assets:US:Bank1:Checking
          2013-05-01 open Assets:US:Bank1:Savings
          2013-05-01 open Assets:US:Bank2:Checking
          2013-05-01 open Assets:US:Bank2:Savings
          2013-05-01 open Equity:OpeningBalances

          2013-05-10 *
            Assets:US:Bank1:Checking                                                 1.00 USD
            Assets:US:Bank1:Savings                                                  2.00 USD
            Assets:US:Bank2:Checking                                                 3.00 USD
            Assets:US:Bank2:Savings                                                  4.00 USD
            Equity:OpeningBalances                                                 -10.00 USD

          2013-05-20 pad Assets:US Equity:OpeningBalances

          ;; A single pad that does not include child accounts should be inserted.
          2013-05-20 P "(Padding inserted for Balance of 100.00 USD for difference 90.00 USD)"
            Assets:US                                                              90.00 USD
            Equity:OpeningBalances                                                -90.00 USD

          2013-06-01 balance Assets:US                                       100.00 USD

        """, entries)

    @loaddoc
    def test_pad_multiple_currencies(self, entries, errors, __):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-10 *
            Assets:Checking                     1.00 USD
            Assets:Checking                     1.00 CAD
            Assets:Checking                     1.00 EUR
            Equity:OpeningBalances

          ;; This should insert two entries: one for USD, one for CAD (different
          ;; amount) and none for EUR.
          2013-05-20 pad Assets:Checking Equity:OpeningBalances

          2013-06-01 balance Assets:Checking    5.00 USD
          2013-06-01 balance Assets:Checking    3.00 CAD
          2013-06-01 balance Assets:Checking    1.00 EUR

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:OpeningBalances

          2013-05-10 *
            Assets:Checking                                                          1.00 USD
            Assets:Checking                                                          1.00 CAD
            Assets:Checking                                                          1.00 EUR
            Equity:OpeningBalances                                                  -1.00 USD
            Equity:OpeningBalances                                                  -1.00 CAD
            Equity:OpeningBalances                                                  -1.00 EUR

          2013-05-20 pad Assets:Checking Equity:OpeningBalances

          2013-05-20 P "(Padding inserted for Balance of 5.00 USD for difference 4.00 USD)"
            Assets:Checking                                                          4.00 USD
            Equity:OpeningBalances                                                  -4.00 USD

          2013-05-20 P "(Padding inserted for Balance of 3.00 CAD for difference 2.00 CAD)"
            Assets:Checking                                                          2.00 CAD
            Equity:OpeningBalances                                                  -2.00 CAD

          2013-06-01 balance Assets:Checking                                 5.00 USD
          2013-06-01 balance Assets:Checking                                 3.00 CAD
          2013-06-01 balance Assets:Checking                                 1.00 EUR

        """, entries)

    @loaddoc
    def test_pad_check_balances(self, entries, errors, __):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:OpeningBalances

          2013-05-01 pad  Assets:Checking   Equity:OpeningBalances

          2013-05-03 txn "Add 20$"
            Assets:Checking                        10 USD
            Assets:Cash

          2013-05-10 balance Assets:Checking      105 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking                        20 USD
            Assets:Cash

          2013-05-16 txn "Add 20$"
            Assets:Checking                        20 USD
            Assets:Cash

          2013-06-01 balance Assets:Checking      145 USD

        """
        post_map = realization.postings_by_account(entries)
        postings = post_map['Assets:Checking']

        balances = []
        pad_balance = Inventory()
        for posting in postings:
            if isinstance(posting, Posting):
                pad_balance.add_position(posting.position, False)
            balances.append((type(posting), pad_balance.get_amount('USD')))

        self.assertEqual(balances, [(Open, Amount('0.00', 'USD')),
                                    (Pad, Amount('0.00', 'USD')),
                                    (Posting, Amount('95.00', 'USD')),
                                    (Posting, Amount('105.00', 'USD')),
                                    (Balance, Amount('105.00', 'USD')),
                                    (Posting, Amount('125.00', 'USD')),
                                    (Posting, Amount('145.00', 'USD')),
                                    (Balance, Amount('145.00', 'USD'))])


    # Note: You could try padding A into B and B into A to see if it works.
