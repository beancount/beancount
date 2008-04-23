"""
Test a case of capital gains booking.
"""

# stdlib imports
import sys
from datetime import date

# beancount imports
from beancount.ledger import Ledger, compute_balsheet
from beancount.wallet import Wallet
from beancount.beantest import ledger_str



class TestCapitalGains(object):

    def_accounts = """
@defaccount De Assets:Bank
@defaccount De Assets:Broker
@defaccount De Assets:CapitalGains
"""

    def test_capgains(self):

        # Simple transaction with a capital gain manually booked.
        lgr = ledger_str(self.def_accounts + """

2008-01-10 * Bought some shares
  Assets:Broker                 10 AAPL @ 11.00 USD
  Assets:Bank               -110.00 USD
  
2008-01-02 * Sold some shares
  Assets:Broker                 -10 AAPL @ 12.00 USD
  Assets:Bank                120.00 USD

""", 'capgains1')

## (Assets:CapitalGains)

        assert len(lgr.transactions) == 2
        root = lgr.get_root_account()
        assert not root.total, root.total



        # Book capital gains, making sure that the commissions aren't counted
        # in.


