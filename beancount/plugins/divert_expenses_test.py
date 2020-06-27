__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.parser import cmptest


class TestDivertExpenses(cmptest.TestCase):

    @loader.load_doc()
    def test_divert(self, entries, errors, __):
        """
        plugin "beancount.plugins.divert_expenses" "{
          'tag': 'kai',
          'account': 'Expenses:Kai',
        }"

        2012-01-01 open Expenses:Baby:Gear
        2012-01-01 open Expenses:Groceries
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Expenses:Kai

        2013-02-15 * "Groceries"
          Liabilities:CreditCard           -17.00 USD
          Expenses:Groceries                17.00 USD

        2013-02-15 * "Stroller" #kai
          Liabilities:CreditCard           -900.00 USD
          Expenses:Baby:Gear                900.00 USD

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

        2012-01-01 open Expenses:Baby:Gear
        2012-01-01 open Expenses:Groceries
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Expenses:Kai

        2013-02-15 * "Groceries"
          Liabilities:CreditCard           -17.00 USD
          Expenses:Groceries                17.00 USD

        2013-02-15 * "Stroller" #kai
          Liabilities:CreditCard           -900.00 USD
          Expenses:Kai                      900.00 USD
            diverted_account: "Expenses:Baby:Gear"
        """, entries)

    @loader.load_doc()
    def test_divert_some_postings(self, entries, errors, __):
        """
        plugin "beancount.plugins.divert_expenses" "{
          'tag': 'kai',
          'account': 'Expenses:Kai',
        }"

        2012-01-01 open Expenses:Pharmacy
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Expenses:Kai

        2018-05-05 * "CVS/PHARMACY" "" #kai
          Liabilities:CreditCard        -66.38 USD
          Expenses:Pharmacy              21.00 USD  ;; Vitamins for Kai
          Expenses:Pharmacy              45.38 USD
            divert: FALSE

        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

        2012-01-01 open Expenses:Pharmacy
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Expenses:Kai

        2018-05-05 * "CVS/PHARMACY" "" #kai
          Liabilities:CreditCard        -66.38 USD
          Expenses:Kai                   21.00 USD
            diverted_account: "Expenses:Pharmacy"
          Expenses:Pharmacy              45.38 USD
            divert: FALSE
        """, entries)

    @loader.load_doc()
    def test_divert_non_expenses(self, entries, errors, __):
        """
        plugin "beancount.plugins.divert_expenses" "{
          'tag': 'kai',
          'account': 'Expenses:Kai',
        }"

        2012-01-01 open Assets:Checking
        2012-01-01 open Assets:Daycare:Deposit
        2012-01-01 open Expenses:Kai

        2018-05-05 * "Daycare" "Deposit for daycare" #kai
          Assets:Checking           -6100.00 USD
          Assets:Daycare:Deposit
            divert: TRUE
        """
        self.assertFalse(errors)
        self.assertEqualEntries("""

        2012-01-01 open Assets:Checking
        2012-01-01 open Assets:Daycare:Deposit
        2012-01-01 open Expenses:Kai

        2018-05-05 * "Daycare" "Deposit for daycare" #kai
          Assets:Checking           -6100.00 USD
          Expenses:Kai               6100.00 USD
            diverted_account: "Assets:Daycare:Deposit"
        """, entries)


if __name__ == '__main__':
    unittest.main()
