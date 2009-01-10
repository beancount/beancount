"""
Test cost balancing.
"""

# stdlib imports
import sys
from datetime import date

# beancount imports
from beancount.ledger import Ledger
from beancount.wallet import Wallet
from beancount.beantest import ledger_str



class TestCostBalancing(object):

    def_accounts = """
@defaccount De Assets:Bank
@defaccount De Income:Salary
@defaccount De Expenses:TripPlan
@defaccount Cr Income:Planning
@defaccount Cr Income:CapGains
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
        assert lgr.get_account('Assets:Bank').balances['total'] == Wallet('100 USD')
        assert lgr.get_account('Income:Salary').balance['total'] == Wallet('-100 USD')


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

2008-01-10 * 
  Assets:Bank              170.00 USD
  Income:Salary            169.00 USD

""", 'twoempty')
        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
## FIXME: how do we assert an error here?




    def test_price(self):

        # Normal price.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Broker               10 AAPL @ 121.00 USD
  Assert:Checking

""", 'normal-price')

        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        assert txn.postings[0].amount == Wallet('10 AAPL')
        assert txn.postings[0].price == Wallet('121 USD')
        assert txn.postings[0].cost == Wallet('1210 USD')
        assert lgr.get_account('Assets:Broker').total == Wallet('10 AAPL')
        assert lgr.get_account('Assert:Checking').total == Wallet('-1210 USD')


        # Total price.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Broker               10 AAPL @@ 1210.00 USD
  Assert:Checking

""", 'total-price')

        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        assert txn.postings[0].amount == Wallet('10 AAPL')
        assert txn.postings[0].price == Wallet('121 USD')
        assert txn.postings[0].cost == Wallet('1210 USD')
        assert lgr.get_account('Assets:Broker').total == Wallet('10 AAPL')
        assert lgr.get_account('Assert:Checking').total == Wallet('-1210 USD')


    def test_explicit_cost(self):

        # Cost-per-share.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Broker               10 AAPL {111.00 USD} @ 121.00 USD
  Assert:Checking

""", 'cost-per-share')

        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        assert txn.postings[0].amount == Wallet('10 AAPL')
        assert txn.postings[0].cost == Wallet('1110 USD')
        assert lgr.get_account('Assets:Broker').total == Wallet('10 AAPL')
        assert lgr.get_account('Assert:Checking').total == Wallet('-1110 USD')


        # Total cost.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Broker               10 AAPL {{1110.00 USD}} @ 121.00 USD
  Assert:Checking

""", 'cost-per-share')

        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        assert txn.postings[0].amount == Wallet('10 AAPL')
        assert txn.postings[0].cost == Wallet('1110 USD')
        assert lgr.get_account('Assets:Broker').total == Wallet('10 AAPL')
        assert lgr.get_account('Assert:Checking').total == Wallet('-1110 USD')


    def test_truestory(self):

        # Some other examples.
        lgr = ledger_str(self.def_accounts + """

2007-12-19 * Redemption
  Assets:Broker                 -29.4650 "AIM681" {{-326.62 CAD}} @ 11.197 CAD
  Assets:Checking                    329.92 CAD   ; adjusted cost base
  Income:CapGains  

""", 'true-story-1')

        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        assert lgr.get_account('Income:CapGains').total == Wallet('-3.30 CAD')




