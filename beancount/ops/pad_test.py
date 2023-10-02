__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount.core.amount import A
from beancount.core import inventory
from beancount.core import data
from beancount.core import realization
from beancount.ops import pad
from beancount.ops import balance
from beancount.parser import cmptest
from beancount import loader


class TestPadding(cmptest.TestCase):

    @loader.load_doc()
    def test_pad_simple(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balances

          ;; Test the simple case that this directive generates a padding entry.
          2013-05-01 pad Assets:Checking Equity:Opening-Balances

          2013-05-03 balance Assets:Checking                                 172.45 USD

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-01 pad Assets:Checking Equity:Opening-Balances

          ;; Check this is inserted.
          2013-05-01 P "(Padding inserted for Balance of 172.45 USD for difference 172.45 USD)"
            Assets:Checking                                                        172.45 USD
            Equity:Opening-Balances                                                -172.45 USD

          2013-05-03 balance Assets:Checking                                 172.45 USD

        """, entries)

    @loader.load_doc()
    def test_pad_to_zero(self, entries, errors, __):
        """
          ;; Test the case that this directive generates a padding entry, padding to zero.
          2013-01-01 open Assets:Checking
          2013-01-01 open Equity:Opening-Balances

          2013-02-01 *
            Assets:Checking           234.56 USD
            Equity:Opening-Balances  -234.56 USD

          2013-05-01 pad Assets:Checking Equity:Opening-Balances

          2013-05-03 balance Assets:Checking                                 0.00 USD

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

          2013-01-01 open Assets:Checking
          2013-01-01 open Equity:Opening-Balances

          2013-02-01 *
            Assets:Checking           234.56 USD
            Equity:Opening-Balances  -234.56 USD

          2013-05-01 pad Assets:Checking Equity:Opening-Balances

          ;; Check this is inserted.
          2013-05-01 P "(Padding inserted for Balance of 0.00 USD for difference -234.56 USD)"
            Assets:Checking          -234.56 USD
            Equity:Opening-Balances   234.56 USD

          2013-05-03 balance Assets:Checking                                 0.00 USD

        """, entries)

    @loader.load_doc(expect_errors=True)
    def test_pad_no_overflow(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balances

          ;; Pad before the next check.
          2013-05-01 pad Assets:Checking Equity:Opening-Balances

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
          2013-05-01 open Equity:Opening-Balances

          2013-05-01 pad Assets:Checking Equity:Opening-Balances

          2013-05-01 P "(Padding inserted for Balance of 172.45 USD for difference 172.45 USD)"
            Assets:Checking                                                        172.45 USD
            Equity:Opening-Balances                                                -172.45 USD

          2013-05-03 balance Assets:Checking                                 172.45 USD

          2013-05-15 * "Add 20$"
            Assets:Checking                                                         20.00 USD
            Assets:Cash                                                            -20.00 USD

          2013-06-01 balance Assets:Checking                                 200.00 USD

        """, entries)

    @loader.load_doc()
    def test_pad_used_twice_legally(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balances

          ;; First pad.
          2013-05-01 pad  Assets:Checking   Equity:Opening-Balances

          2013-05-03 balance Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          ;; Second pad.
          2013-05-20 pad  Assets:Checking   Equity:Opening-Balances

          2013-06-01 balance Assets:Checking   200 USD

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balances

          2013-05-01 pad Assets:Checking Equity:Opening-Balances

          2013-05-01 P "(Padding inserted for Balance of 172.45 USD for difference 172.45 USD)"
            Assets:Checking                                                        172.45 USD
            Equity:Opening-Balances                                                -172.45 USD

          2013-05-03 balance Assets:Checking                                 172.45 USD

          2013-05-15 * "Add 20$"
            Assets:Checking                                                         20 USD
            Assets:Cash                                                            -20 USD

          2013-05-20 pad Assets:Checking Equity:Opening-Balances

          2013-05-20 P "(Padding inserted for Balance of 200 USD for difference 7.55 USD)"
            Assets:Checking                                                          7.55 USD
            Equity:Opening-Balances                                                  -7.55 USD

          2013-06-01 balance Assets:Checking                                 200 USD

        """, entries)

    @loader.load_doc(expect_errors=True)
    def test_pad_used_twice_illegally(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-03 balance Assets:Checking   0.00 USD

          ;; Two pads in between checks.
          2013-05-10 pad  Assets:Checking   Equity:Opening-Balances
          2013-05-20 pad  Assets:Checking   Equity:Opening-Balances

          2013-06-01 balance Assets:Checking   200 USD

        """
        self.assertEqual([pad.PadError], list(map(type, errors)))
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-03 balance Assets:Checking                                 0.00 USD

          2013-05-10 pad Assets:Checking Equity:Opening-Balances

          2013-05-20 pad Assets:Checking Equity:Opening-Balances

          2013-05-20 P "(Padding inserted for Balance of 200 USD for difference 200 USD)"
            Assets:Checking                                                        200 USD
            Equity:Opening-Balances                                               -200 USD

          2013-06-01 balance Assets:Checking                                 200 USD

        """, entries)

    @loader.load_doc(expect_errors=True)
    def test_pad_unused(self, entries, errors, __):
        """

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balances

          2013-05-10 * "Add 200$"
            Assets:Checking       200.00 USD
            Assets:Cash          -200.00 USD

          ;; This pad will do nothing, should raise a warning..
          2013-05-20 pad  Assets:Checking   Equity:Opening-Balances

          2013-06-01 balance Assets:Checking   200.00 USD

        """
        self.assertEqual([pad.PadError], list(map(type, errors)))
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balances

          2013-05-10 * "Add 200$"
            Assets:Checking                                                        200.00 USD
            Assets:Cash                                                           -200.00 USD

          2013-05-20 pad Assets:Checking Equity:Opening-Balances

          2013-06-01 balance Assets:Checking                                 200.00 USD

        """, entries)

    @loader.load_doc()
    def test_pad_parents(self, entries, errors, __):
        """

          2013-05-01 open Assets:US
          2013-05-01 open Assets:US:Bank1:Checking
          2013-05-01 open Assets:US:Bank1:Savings
          2013-05-01 open Assets:US:Bank2:Checking
          2013-05-01 open Assets:US:Bank2:Savings
          2013-05-01 open Equity:Opening-Balances

          2013-05-10 *
            Assets:US:Bank1:Checking                                 1.00 USD
            Assets:US:Bank1:Savings                                  2.00 USD
            Assets:US:Bank2:Checking                                 3.00 USD
            Assets:US:Bank2:Savings                                  4.00 USD
            Equity:Opening-Balances                                 -10.00 USD

          2013-05-20 pad Assets:US Equity:Opening-Balances

          2013-06-01 balance Assets:US                                       100.00 USD

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

          2013-05-01 open Assets:US
          2013-05-01 open Assets:US:Bank1:Checking
          2013-05-01 open Assets:US:Bank1:Savings
          2013-05-01 open Assets:US:Bank2:Checking
          2013-05-01 open Assets:US:Bank2:Savings
          2013-05-01 open Equity:Opening-Balances

          2013-05-10 *
            Assets:US:Bank1:Checking                                                 1.00 USD
            Assets:US:Bank1:Savings                                                  2.00 USD
            Assets:US:Bank2:Checking                                                 3.00 USD
            Assets:US:Bank2:Savings                                                  4.00 USD
            Equity:Opening-Balances                                                 -10.00 USD

          2013-05-20 pad Assets:US Equity:Opening-Balances

          ;; A single pad that does not include child accounts should be inserted.
          2013-05-20 P "(Padding inserted for Balance of 100.00 USD for difference 90.00 USD)"
            Assets:US                                                              90.00 USD
            Equity:Opening-Balances                                                -90.00 USD

          2013-06-01 balance Assets:US                                       100.00 USD

        """, entries)

    @loader.load_doc()
    def test_pad_multiple_currencies(self, entries, errors, __):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-10 *
            Assets:Checking                     1.00 USD
            Assets:Checking                     1.00 CAD
            Assets:Checking                     1.00 EUR
            Equity:Opening-Balances

          ;; This should insert two entries: one for USD, one for CAD (different
          ;; amount) and none for EUR.
          2013-05-20 pad Assets:Checking Equity:Opening-Balances

          2013-06-01 balance Assets:Checking    5.00 USD
          2013-06-01 balance Assets:Checking    3.00 CAD
          2013-06-01 balance Assets:Checking    1.00 EUR

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-05-10 *
            Assets:Checking                    1.00 USD
            Assets:Checking                    1.00 CAD
            Assets:Checking                    1.00 EUR
            Equity:Opening-Balances           -1.00 USD
            Equity:Opening-Balances           -1.00 CAD
            Equity:Opening-Balances           -1.00 EUR

          2013-05-20 pad Assets:Checking Equity:Opening-Balances

          2013-05-20 P "(Padding inserted for Balance of 5.00 USD for difference 4.00 USD)"
            Assets:Checking                    4.00 USD
            Equity:Opening-Balances           -4.00 USD

          2013-05-20 P "(Padding inserted for Balance of 3.00 CAD for difference 2.00 CAD)"
            Assets:Checking                    2.00 CAD
            Equity:Opening-Balances           -2.00 CAD

          2013-06-01 balance Assets:Checking   5.00 USD
          2013-06-01 balance Assets:Checking   3.00 CAD
          2013-06-01 balance Assets:Checking   1.00 EUR

        """, entries)

    @loader.load_doc()
    def test_pad_check_balances(self, entries, errors, __):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balances

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balances

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
        txn_postings = post_map['Assets:Checking']

        balances = []
        pad_balance = inventory.Inventory()
        for txn_posting in txn_postings:
            if isinstance(txn_posting, data.TxnPosting):
                position_, _ = pad_balance.add_position(txn_posting.posting)
            balances.append((type(txn_posting), pad_balance.get_currency_units('USD')))

        self.assertEqual(balances, [(data.Open, A('0.00 USD')),
                                    (data.Pad, A('0.00 USD')),
                                    (data.TxnPosting, A('95.00 USD')),
                                    (data.TxnPosting, A('105.00 USD')),
                                    (data.Balance, A('105.00 USD')),
                                    (data.TxnPosting, A('125.00 USD')),
                                    (data.TxnPosting, A('145.00 USD')),
                                    (data.Balance, A('145.00 USD'))])

    # Note: You could try padding A into B and B into A to see if it works.

    @loader.load_doc(expect_errors=True)
    def test_pad_multiple_times(self, entries, errors, __):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balances

          2013-06-01 pad Assets:Checking Equity:Opening-Balances
          2013-07-01 pad Assets:Checking Equity:Opening-Balances

          2013-10-01 balance Assets:Checking    5.00 USD
        """
        self.assertEqual([pad.PadError], list(map(type, errors)))

    @loader.load_doc(expect_errors=True)
    def test_pad_at_cost(self, entries, errors, __):
        """
          2013-05-01 open Assets:Investments
          2013-05-01 open Equity:Opening-Balances

          2013-05-15 *
            Assets:Investments   10 MSFT {54.30 USD}
            Equity:Opening-Balances

          2013-06-01 pad Assets:Investments Equity:Opening-Balances

          2013-10-01 balance Assets:Investments   12 MSFT
        """
        self.assertEqual([pad.PadError], list(map(type, errors)))
        self.assertRegex(errors[0].message, 'Attempt to pad an entry with cost for')

    @loader.load_doc()
    def test_pad_parent(self, entries, errors, __):
        """
          1998-01-01 open Assets:CA:Bank:Checking     CAD
          1998-01-01 open Assets:CA:Bank:CheckingOld  CAD
          1998-01-01 open Income:CA:Something         USD
          1998-01-01 open Equity:Beginning-Balances

          2006-01-01 pad Assets:CA:Bank:Checking   Equity:Beginning-Balances

          2006-04-04 balance Assets:CA:Bank:Checking      742.50 CAD

          2001-01-15 * "Referral"
            Assets:CA:Bank:CheckingOld   1000.00 CAD @ 0.6625 USD
            Income:CA:Something          -662.50 USD
        """
        self.assertFalse(errors)

    @loader.load_doc(expect_errors=True)
    def test_pad_tolerance(self, entries, errors, __):
        """
          1998-01-01 open Assets:CA:Bank:Checking
          1998-01-01 open Income:CA:Something
          1998-01-01 open Equity:Beginning-Balances

          2006-01-01 pad Assets:CA:Bank:Checking   Equity:Beginning-Balances

          2001-01-15 * "Referral"
            Assets:CA:Bank:Checking   999.95 CAD
            Income:CA:Something

          2006-04-04 balance Assets:CA:Bank:Checking      1000.00 ~ 0.05 CAD

        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, 'Unused Pad entry')

    @loader.load_doc(expect_errors=True)
    def test_pad_zero_padding_issue78a(self, entries, errors, __):
        """
        1970-01-01 open Assets:Cash
        1970-01-01 open Expenses:Food

        2015-09-15 pad Assets:Cash Expenses:Food

        2015-09-16 balance Assets:Cash 0 HKD

        ;; The error should be reported here, not on the previous balance check.
        2015-11-02 balance Assets:Cash 1000 HKD
        """
        self.assertEqual(2, len(errors))

        self.assertRegex(errors[0].message, "Unused")
        self.assertEqual(datetime.date(2015, 9, 15), errors[0].entry.date)

        self.assertRegex(errors[1].message, "Balance failed")
        self.assertEqual(datetime.date(2015, 11, 2), errors[1].entry.date)

    @loader.load_doc(expect_errors=True)
    def test_pad_zero_padding_issue78a_original(self, entries, errors, __):
        """
        1970-01-01 open Assets:Cash
        1970-01-01 open Expenses:Food

        ;; Inserted this before the pad, because it was in the original example.
        2015-09-15 balance Assets:Cash 0 HKD

        2015-09-15 pad Assets:Cash Expenses:Food

        2015-09-16 balance Assets:Cash 0 HKD

        ;; The error should be reported here, not on the previous balance check.
        2015-11-02 balance Assets:Cash 1000 HKD
        """
        self.assertEqual(2, len(errors))

        self.assertRegex(errors[0].message, "Unused")
        self.assertEqual(datetime.date(2015, 9, 15), errors[0].entry.date)

        self.assertRegex(errors[1].message, "Balance failed")
        self.assertEqual(datetime.date(2015, 11, 2), errors[1].entry.date)

    @loader.load_doc(expect_errors=True)
    def test_pad_zero_padding_issue78b(self, entries, errors, __):
        """
        1970-01-01 open Assets:Cash
        1970-01-01 open Expenses:Food

        ;; Balance should fail here.
        2015-09-15 balance Assets:Cash 1 HKD

        ;; This will not end up being marked as unused because it will fill in a transaction
        ;; for the missing amount, even though there was a matching balance check earlier. A
        ;; failing balance check does not automatically bring the balance to its value.
        2015-09-15 pad Assets:Cash Expenses:Food

        2015-09-16 balance Assets:Cash 1 HKD
        """
        self.assertTrue(any(isinstance(entry, data.Transaction)
                            for entry in entries))

        self.assertEqual(1, len(errors))

        self.assertRegex(errors[0].message, "Balance failed")
        self.assertEqual(datetime.date(2015, 9, 15), errors[0].entry.date)

    @loader.load_doc()
    def test_pad_issue362(self, entries, errors, __):
        """
        1970-01-01 open Assets:Bank
        1970-01-01 open Assets:Bank-Two
        1970-01-01 open Equity:Opening-Balance
        1970-01-01 open Equity:Adjustments
        1970-01-01 open Expenses:Food

        2019-01-01 * "Opening balance"
          Assets:Bank                  20.00 GBP
          Equity:Opening-Balance      -20.00 GBP

        2019-01-03 * "Tesco" "Buy food"
          Expenses:Food                30.00 GBP
          Assets:Bank                 -30.00 GBP

        2019-01-04 balance Assets:Bank -10.00 GBP

        2019-01-03 * "Tesco" "Buy food"
          Expenses:Food                 6.00 GBP
          Assets:Bank-Two              -6.00 GBP

        ; Get rid of overdraft somehow and get a £50 balance

        2019-01-04 pad Assets:Bank Equity:Adjustments
        2019-01-05 balance Assets:Bank 50.00 GBP
        """
        # self.assertTrue(any(isinstance(entry, data.Transaction)
        #                     for entry in entries))
        # self.assertEqual(1, len(errors))
        # self.assertRegex(errors[0].message, "Balance failed")
        # self.assertEqual(datetime.date(2015, 9, 15), errors[0].entry.date)


if __name__ == '__main__':
    unittest.main()
