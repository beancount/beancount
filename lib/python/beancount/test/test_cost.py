"""
Test cost balancing.
"""

# stdlib imports
import sys
from datetime import date

# beancount imports
from beancount.ledger import Ledger, compute_balsheet
from beancount.wallet import Wallet
from beancount.beantest import ledger_str



class TestCostBalancing(object):

    def_accounts = """
@defaccount De Assets:Bank
@defaccount De Income:Salary
@defaccount De Expenses:TripPlan
@defaccount Cr Income:Planning
"""

    def test_simple(self):

        # Simple transaction.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Bank               100.00 USD
  Income:Salary

""", 'simple')

        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        assert txn.postings[0].amount == Wallet('100 USD')
        assert lgr.get_account('Assets:Bank').total == Wallet('100 USD')
        assert lgr.get_account('Income:Salary').total == Wallet('-100 USD')


        # Empty unbalanced virtual posting should fail.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Bank               100.00 USD
  Income:Salary            -100.00 USD
  (Expenses:TripPlan)

""", 'virtempty')
        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        assert len(txn.postings) == 3
        assert txn.postings[2].amount == Wallet()
## FIXME: how do we assert that there was a warning here?


        # Normal and virtual postings should balance independently.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Bank              170.00 USD
  Income:Salary
  [Income:Planning]         42.00 USD
  [Expenses:TripPlan]

""", 'twoempty')
        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        assert len(txn.postings) == 4
        for i, amt in enumerate(('170 USD', '-170 USD',
                                 '42 USD', '-42 USD')):
            assert txn.postings[i].amount == Wallet(amt)



        # Normal and virtual postings should balance independently.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Bank              170.00 USD
  Income:Salary            169.00 USD

""", 'twoempty')
        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
## FIXME: how do we assert an error here?











"""
2008-01-10 * Bought some shares
  Assets:Bank               100.00 USD
  Income:Salary            -100.00 USD
  (Expenses:TripPlan)
"""

##         # Unbalanced posting.
##         lgr = ledger_str(self.def_accounts + """

## 2008-01-10 * Bought some shares
##   Assets:Bank               100.00 USD
##   Income:Salary
##   (Expenses:TripPlan)        10.00 USD

## """, 'unbalanced')

##         assert len(lgr.transactions) == 1
##         txn = lgr.transactions[0]
##         assert txn.postings[0].amount == Wallet('100 USD')

##         assert lgr.get_account('').total == Wallet()
##         assert lgr.get_account('Assets:Bank').total == Wallet('100 USD')
##         assert lgr.get_account('Income:Salary').total == Wallet('-100 USD')
##         assert lgr.get_account('Expenses:TripPlan').total == Wallet('10 USD')

