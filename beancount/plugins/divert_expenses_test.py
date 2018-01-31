__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

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
