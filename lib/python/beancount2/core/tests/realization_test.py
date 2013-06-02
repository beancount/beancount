"""
Unit tests for realizations.
"""

import unittest
import sys
from datetime import date
import textwrap
import functools
from copy import copy

from beancount2 import parser
from beancount2.parser.tests.parser_test import parsedoc

from beancount2.core.inventory import Position, Lot, Inventory
from beancount2.core import validation
from beancount2.core import realization
from beancount2.core.data import Open, Close, Note, Pad, Check, Transaction, Posting
from beancount2.core.data import Decimal, Amount
from beancount2.core import data



do_trace = False

def realizedoc(fun):
    """Decorator that parses, pads and realizes the function's docstring as an
    argument."""
    @functools.wraps(fun)
    def newfun(self):
        contents = parser.parse_string(textwrap.dedent(fun.__doc__))
        entries, pad_errors = realization.pad(contents.entries)
        errors = contents.errors + pad_errors

        real_accounts = realization.realize(entries, do_check=True)
        if do_trace and errors:
            trace_errors(real_accounts, errors)
        return fun(self, entries, real_accounts, errors)
    return newfun

def trace_errors(real_accounts, errors):
    print()
    print("ERRORS")
    data.print_errors(errors)
    print()
    print("REAL_ACCOUNTS")
    for account_name, real_account in real_accounts.items():
        if real_account.postings:
            print('  ', real_account.account.name)
            for posting in real_account.postings:
                print('      {}'.format(posting))
    print()



## FIXME: this needs become TestCheck().

class __TestRealization(unittest.TestCase):

    @realizedoc
    def test_check_error(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-03 check Assets:US:Checking   100 USD
        """
        self.assertEqual(len(errors), 1)

    @realizedoc
    def test_check_okay(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-03 check Assets:US:Checking   100 USD

        """
        self.assertEqual(len(errors), 0)

    # This test ensures that the 'check' directives apply at the beginning of
    # the day.
    @realizedoc
    def test_check_samedate(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:US:Checking   USD
          2013-05-01 open Expenses:Something

          2013-05-02 txn "Testing!"
            Assets:US:Checking            100 USD
            Expenses:Something           -100 USD

          2013-05-02 check Assets:US:Checking     0 USD
          2013-05-03 check Assets:US:Checking   100 USD
        """
        data.print_errors(errors)
        assert len(errors) == 0


class TestPadding(unittest.TestCase):

    def check_real_types(self, real_account, entry_types):
        """Check the types of entries rendered."""
        self.assertEqual(list(map(type,  real_account.postings)),
                         entry_types)

    def check_balance(self, real_account, position):
        self.assertEqual(real_account.balance.get_position(position.lot), position)

    @realizedoc
    def test_pad(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balancess

          2013-05-03 check Assets:Checking   172.45 USD
        """
        self.assertEqual(len(errors), 0)
        self.check_real_types(real_accounts['Assets:Checking'], [Open, Pad, Posting, Check])
        self.check_real_types(real_accounts['Equity:Opening-Balancess'], [Open, Pad, Posting])

        self.check_balance(real_accounts['Assets:Checking'],
                           Position(Lot('USD', None, None), Decimal('172.45')))

    @realizedoc
    def test_with_cost(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Invest
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Invest   Equity:Opening-Balancess

          2013-05-03 check Assets:Invest   172.45 GOOG {12.00 USD}
        """
        assert len(errors) == 0
        self.check_real_types(real_accounts['Assets:Invest'], [Open, Pad, Posting, Check])
        self.check_real_types(real_accounts['Equity:Opening-Balancess'], [Open, Pad, Posting])

        self.check_balance(real_accounts['Assets:Invest'],
                           Position(Lot('GOOG', Amount('12.00', 'USD'), None), Decimal('172.45')))


    @realizedoc
    def test_with_cost_and_lotdate(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Invest
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Invest   Equity:Opening-Balancess

          2013-05-03 check Assets:Invest   172.45 GOOG {12.00 USD / 2000-01-01}
        """
        assert len(errors) == 0
        self.check_real_types(real_accounts['Assets:Invest'], [Open, Pad, Posting, Check])
        self.check_real_types(real_accounts['Equity:Opening-Balancess'], [Open, Pad, Posting])

        self.check_balance(real_accounts['Assets:Invest'],
                           Position(Lot('GOOG', Amount('12.00', 'USD'), date(2000, 1, 1)), Decimal('172.45')))

    @realizedoc
    def test_pad_fail(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balancess

          2013-05-03 check Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          ;; This should fail here:
          2013-06-01 check Assets:Checking   200 USD

        """
        ## FIXME - if we run check() , this should be true: 
        ## self.assertEqual(len(errors), 1)


    @realizedoc
    def test_pad_used_twice(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balancess

          2013-05-03 check Assets:Checking   172.45 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-05-20 pad  Assets:Checking   Equity:Opening-Balancess

          2013-06-01 check Assets:Checking   200 USD

        """
        self.assertEqual(len(errors), 0)
        self.check_real_types(real_accounts['Assets:Checking'],
                            [Open, Pad, Posting, Check, Posting, Pad, Posting, Check])

    @realizedoc
    def test_pad_check_balances(self, entries, real_accounts, errors):
        """
          2013-05-01 open Assets:Checking
          2013-05-01 open Assets:Cash
          2013-05-01 open Equity:Opening-Balancess

          2013-05-01 pad  Assets:Checking   Equity:Opening-Balancess

          2013-05-03 txn "Add 20$"
            Assets:Checking             10 USD
            Assets:Cash

          2013-05-10 check Assets:Checking   105 USD

          2013-05-15 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-05-16 txn "Add 20$"
            Assets:Checking             20 USD
            Assets:Cash

          2013-06-01 check Assets:Checking   145 USD

        """


        balances = []
        balance = Inventory()
        for posting in real_accounts['Assets:Checking'].postings:
            if isinstance(posting, Posting):
                balance.add_position(posting.position)
            balances.append((type(posting), balance.get_amount('USD')))

        self.assertEqual(balances, [(Open, Amount('0.00', 'USD')),
                                    (Pad, Amount('0.00', 'USD')),
                                    (Posting, Amount('95.00', 'USD')),
                                    (Posting, Amount('105.00', 'USD')),
                                    (Check, Amount('105.00', 'USD')),
                                    (Posting, Amount('125.00', 'USD')),
                                    (Posting, Amount('145.00', 'USD')),
                                    (Check, Amount('145.00', 'USD'))])


# FIXME: please DO test the realization of a transaction that has multiple legs on the same account!


# FIXME: Write a test that padding a parent account wouldn't pad its child accounts.
