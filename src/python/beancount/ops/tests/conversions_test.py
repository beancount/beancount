"""
Test conversions insertion.
"""
import unittest
import sys
import datetime

from beancount.core import realization
from beancount.ops import summarize
from beancount.loader import loaddoc
from beancount.parser import parser


class TestConversions(unittest.TestCase):

    @loaddoc
    def test_basic_conversions(self, entries, errors, options):
        """
          2013-02-01 open Income:US:Job           USD
          2013-02-01 open Assets:US:Checking      USD
          2013-02-01 open Assets:CA:Invest        CAD
          2013-02-01 open Assets:CA:Invest:NT     NT

          2011-03-01 * "Earn some money"
            Income:US:Job            -1000 USD
            Assets:US:Checking        1000 USD

          2012-03-02 * "Transfer to Investment"
            Assets:US:Checking       -800 USD
            Assets:CA:Invest          800 CAD @ 1 USD

          2012-03-03 * "Buy some stock"
            Assets:CA:Invest         -600 CAD
            Assets:CA:Invest:NT        60 NT {10 CAD}

          2013-02-01 * "Transfer some money back"
            Assets:CA:Invest         -100 CAD @ 1 USD
            Assets:US:Checking        100 USD

        """
        previous_accounts = parser.get_previous_accounts(options)
        entries, _ = summarize.clamp(entries,
                                     datetime.date(2013, 1, 1), datetime.date(2014, 1, 1),
                                     *previous_accounts)

        current_accounts = parser.get_current_accounts(options)
        entries = summarize.close(entries, *current_accounts)

        # entries = conversions(entries, ACCOUNT_CONVERSIONS1, datetime.date(2013, 1, 1))
        # entries = conversions(entries, ACCOUNT_CONVERSIONS2)

        converted_balance = summarize.compute_total_balance(entries)
        print(converted_balance.get_cost())
        # assert converted_balance.get_cost().is_empty()

        real_accounts = realization.realize(entries)
        realization.dump_tree_balances(real_accounts, sys.stdout)



## FIXME: Build more good examples to understand, with positions held at cost, as tests
