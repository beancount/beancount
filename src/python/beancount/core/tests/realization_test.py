"""
Unit tests for realizations.
"""

import unittest
import textwrap
import functools

from beancount import parser

from beancount.parser import parsedoc
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



class TestRealization(unittest.TestCase):

    @parsedoc
    def test_simple_realize(self, entries, errors, options):
        """
          2013-05-01 open Assets:US:Checking:Sub   USD
          2013-05-01 open Expenses:Stuff
          2013-05-02 txn "Testing!"
            Assets:US:Checking:Sub            100 USD
            Expenses:Stuff           -100 USD
        """
        real_accounts = realization.realize(entries)
        for real_account in real_accounts:
            assert isinstance(real_account, realization.RealAccount)

        real_accounts2 = realization.realize2(entries)

        for name in 'Assets:US:Checking:Sub', 'Expenses:Stuff':
            lookup_account1 = real_accounts[name]
            assert lookup_account1.fullname == name
            lookup_account2 = real_accounts2[name]
            assert lookup_account2.fullname == name






# FIXME: please DO test the realization of a transaction that has multiple legs on the same account!
