"""
Test a case of capital gains booking.
"""

# stdlib imports
import sys
from datetime import date
from decimal import Decimal

# beancount imports
from beancount.ledger import Ledger, compute_balsheet
from beancount.wallet import Wallet
from beancount.beantest import ledger_str



class TestCapitalGains(object):

    def_accounts = """
@defaccount De Assets:Bank
@defaccount De Assets:Broker
@defaccount Cr Income:CapitalGains
@defaccount De Expenses:Commissions
@defaccount De Expenses:Deductible-Costs
"""

    def test_custom_cost(self):

        # Unbooked capital gains.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares.
  Assets:Broker              10 AAPL @ 120.00 USD
  Assets:Bank

2008-01-10 * Sold some shares.
  Assets:Broker              -10 AAPL @ 125.00 USD
  Assets:Bank

""", 'unbooked-gains')
        assert len(lgr.transactions) == 2
        assert lgr.transactions[0].postings[1].amount == Wallet('-1200 USD')
        assert lgr.transactions[1].postings[1].amount == Wallet('1250 USD')


        # Forgotten capital gains.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares.
  Assets:Broker              10 AAPL @ 120.00 USD
  Assets:Bank

2008-01-11 * Sold some shares.
  Assets:Broker              -10 AAPL {120.00 USD} @ 125.00 USD
  Assets:Bank

""", 'forgot-gains')
        assert len(lgr.transactions) == 2
        assert lgr.transactions[0].postings[1].amount == Wallet('-1200 USD')
        assert lgr.transactions[1].postings[1].amount == Wallet('1200 USD')


    def test_custom_cost2(self):

        # Booked capital gains.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares.
  Assets:Broker              10 AAPL @ 120.00 USD
  Assets:Bank          -1200 USD

2008-01-11 * Sold some shares.
  Assets:Broker              -10 AAPL {120.00 USD} @ 125.00 USD
  Assets:Bank           1250 USD
  Income:CapitalGains      -50 USD

""", 'booked-gains')
        assert len(lgr.transactions) == 2
        assert lgr.transactions[1].postings[0].cost == Wallet('-1200 USD')
        assert lgr.transactions[1].postings[1].amount == Wallet('1250 USD')
        assert lgr.transactions[1].postings[2].amount == Wallet('-50 USD')


    def test_with_commissions(self):

        # Booked capital gains.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares.
  Assets:Broker              10 AAPL @ 120.00 USD
  Expenses:Commissions      9.95 USD
  Assets:Bank          -1209.95 USD

2008-01-11 * Sold some shares.
  Assets:Broker              -10 AAPL {120.00 USD} @ 125.00 USD
  Assets:Bank            1240.05 USD      ;; actual amount deposited (easy to find on statement)
  Expenses:Commissions      9.95 USD      ;; actual commission for closing the trade
  Income:CapitalGains                     ;; automatically computed gain (from share cost above)
  [Income:CapitalGains]       19.90 USD   ;; offset for commissions to open and close this trade, manually entered
  [Expenses:Deductible-Costs]             ;; an account that track costs for closed trades

""", 'booked-gains')
        assert len(lgr.transactions) == 2

        commisions = 2*Decimal('9.95')
        for accname, amount in (
            ('Assets:Bank', Decimal('50')-commisions),
            ('Expenses:Commissions', commisions),
            ('Income:CapitalGains', -(Decimal('50')-commisions)),
            ):
            assert (lgr.get_account(accname).total == Wallet('USD', amount))








