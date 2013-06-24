"""
Unit tests for realizations.
"""

import unittest
import textwrap
import functools

from beancount import parser

from beancount.core import realization
from beancount.core import data



do_trace = False

def realizedoc(fun):
    """Decorator that parses, pads and realizes the function's docstring as an
    argument."""
    @functools.wraps(fun)
    def newfun(self):
        entries, errors, options = parser.parse_string(textwrap.dedent(fun.__doc__))

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


# FIXME: please DO test the realization of a transaction that has multiple legs on the same account!






# FIXME: We need a test that triggers all the possible kinds of errors that we
# may issue, everywhere actually. That's a great way to start coverage.
