"""
Test the directive which automatically inserts a transaction to pad for
surrounding checks.
"""

# stdlib imports
import sys
from datetime import date

# beancount imports
from beancount.ledger import Ledger, compute_balsheet
from beancount.wallet import Wallet
from beancount.beantest import ledger_str



class TestAutoPad(object):

    def_accounts = """
@defaccount De Assets:Bag
@defaccount Cr Equity:Opening-Balances
"""

    def test_padding(self):

        # Testing padding before the first check.
        lgr = ledger_str(self.def_accounts + """
@pad 2008-04-10  Assets:Bag  Equity:Opening-Balances

2008-04-15 * Misc
  Assets:Bag             10.00 USD
  Equity:Opening-Balances

@check 2008-04-20  Assets:Bag  1203.23 USD
        """, 'before')
        assert len(lgr.transactions) == 2
        txn = filter(lambda x: x.flag == 'A', lgr.transactions)[0]
        assert txn.actual_date == date(2008, 4, 10)
        assert txn.postings[0].amount == Wallet('1193.23 USD')


        # Testing padding between checks before a transaction.
        lgr = ledger_str(self.def_accounts + """
2008-04-01 * Misc
  Assets:Bag             43.00 USD
  Equity:Opening-Balances

@check 2008-04-20  Assets:Bag  43.00 USD

@pad 2008-04-22  Assets:Bag  Equity:Opening-Balances

2008-04-23 * Misc
  Assets:Bag             10.00 USD
  Equity:Opening-Balances

@check 2008-04-25  Assets:Bag  64.00 USD
""", 'between1')
        assert len(lgr.transactions) == 3
        txn = filter(lambda x: x.flag == 'A', lgr.transactions)[0]
        assert txn.actual_date == date(2008, 4, 22)
        ## print txn
        assert txn.postings[0].amount == Wallet('11.00 USD')



        # Test padding between checks between transactions.
        lgr = ledger_str(self.def_accounts + """
2008-04-01 * Misc
  Assets:Bag             21.00 USD
  Equity:Opening-Balances

@check 2008-04-20  Assets:Bag  21.00 USD

2008-04-21 * Misc
  Assets:Bag             10.00 USD
  Equity:Opening-Balances

@pad 2008-04-22  Assets:Bag  Equity:Opening-Balances

2008-04-23 * Misc
  Assets:Bag             10.00 USD
  Equity:Opening-Balances

@check 2008-04-24  Assets:Bag  53.01 USD
""", 'between2')
        assert len(lgr.transactions) == 4
        txn = filter(lambda x: x.flag == 'A', lgr.transactions)[0]
        assert txn.actual_date == date(2008, 4, 22)
        assert txn.postings[0].amount == Wallet('12.01 USD')


        # Test padding between checks after transactions.
        lgr = ledger_str(self.def_accounts + """

2008-04-01 * Misc
  Assets:Bag             43.00 USD
  Equity:Opening-Balances

@check 2008-04-20  Assets:Bag  43.00 USD

2008-04-23 * Misc
  Assets:Bag             10.00 USD
  Equity:Opening-Balances

@pad 2008-04-22  Assets:Bag  Equity:Opening-Balances

@check 2008-04-25  Assets:Bag  64.02 USD
""", 'between3')
        assert len(lgr.transactions) == 3
        txn = filter(lambda x: x.flag == 'A', lgr.transactions)[0]
        assert txn.actual_date == date(2008, 4, 22)
        assert txn.postings[0].amount == Wallet('11.02 USD')


        # Test padding between after checks (should fail).
        lgr = ledger_str(self.def_accounts + """
2008-01-01 * Misc
  Assets:Bag             17.00 USD
  Equity:Opening-Balances

@check 2008-04-20  Assets:Bag  17.00 USD

@pad 2008-04-21  Assets:Bag  Equity:Opening-Balances
""", 'after')
        assert len(lgr.transactions) == 1


        # Test padding for an empty amount: should be no padding at all.
        lgr = ledger_str(self.def_accounts + """
2008-01-01 * Misc
  Assets:Bag             43.00 USD
  Equity:Opening-Balances

@check 2008-04-20  Assets:Bag  43.00 USD

@pad 2008-04-22  Assets:Bag  Equity:Opening-Balances

2008-04-23 * Misc
  Assets:Bag             10.00 USD
  Equity:Opening-Balances

@check 2008-04-25  Assets:Bag  53.00 USD
""", 'empty1')
        assert len(lgr.transactions) == 2
        assert not filter(lambda x: x.flag == 'A', lgr.transactions)


        lgr = ledger_str(self.def_accounts + """
@pad 2008-04-12  Assets:Bag  Equity:Opening-Balances
@check 2008-04-20  Assets:Bag  0.00 USD
""", 'empty2')
        assert len(lgr.transactions) == 0


    def test_manycomm(self):
        "Test padding in the presence of many commodities."

        lgr = ledger_str(self.def_accounts + """
@pad 2008-01-01  Assets:Bag  Equity:Opening-Balances

@check 2008-04-02  Assets:Bag    1 CAD
@check 2008-04-02  Assets:Bag    2 USD
@check 2008-04-02  Assets:Bag    3 AAPL
""", 'manycomm')
        assert len(lgr.transactions) == 1
        txn = lgr.transactions[0]
        w = Wallet('1 CAD, 2 USD, 3 AAPL')
        assert txn.postings[0].amount == w
        assert txn.postings[1].amount == -w


