"""
Unit tests for realizations.
"""
import unittest
import textwrap
import functools
import re

from beancount import parser

from beancount.core.realization import RealAccount
from beancount.parser import parsedoc
from beancount.parser import printer
from beancount.core import realization
from beancount.core import data
from beancount.core import account


class TestRealAccount(unittest.TestCase):

    def test_ctor(self):
        account1 = account.account_from_name('Assets:US:Bank:Checking')
        real_account1 = RealAccount(account1.name, account1)
        with self.assertRaises(AssertionError):
            real_account2 = RealAccount('Assets:US:Bank:Savings', account1)
        account2 = RealAccount('', None)

    def test_str(self):
        account1 = account.account_from_name('Assets:US:Bank:Checking')
        real_account1 = RealAccount(account1.name, account1)
        account1_str = str(real_account1)
        self.assertTrue(re.search('RealAccount', account1_str))
        self.assertTrue(re.search(account1.name, account1_str))

    def create_hierarchy(self):
        account1 = account.account_from_name('Assets')
        real_account1 = RealAccount(account1.name, account1)

        account2 = account.account_from_name('Assets:US')
        real_account2 = RealAccount(account2.name, account2)

        account3 = account.account_from_name('Assets:US:Bank')
        real_account3 = RealAccount(account3.name, account3)

        account4 = account.account_from_name('Assets:US:Bank:Checking')
        real_account4 = RealAccount(account4.name, account4)

        real_account1.add(real_account2)
        real_account2.add(real_account3)
        real_account3.add(real_account4)

        return real_account1

    def test_add(self):
        self.create_hierarchy()

    def test_getitem(self):
        real_account = self.create_hierarchy()

        with self.assertRaises(KeyError):
            real_account['Unknown']

        real_child1 = real_account['US']
        self.assertEqual(real_child1.fullname, 'Assets:US')

        real_child2 = real_child1['Bank']
        self.assertEqual(real_child2.fullname, 'Assets:US:Bank')

        real_child3 = real_child2['Checking']
        self.assertEqual(real_child3.fullname, 'Assets:US:Bank:Checking')

    def test_iter(self):
        real_account = self.create_hierarchy()

        real_accounts = list(real_account)
        self.assertTrue(all(isinstance(real_account, RealAccount)
                            for real_account in real_accounts))
        self.assertEqual(4, len(real_accounts))

    def test_contains(self):
        real_account = self.create_hierarchy()
        real_child1 = real_account['US']
        self.assertTrue('US' in real_account)
        self.assertFalse('Assets:US' in real_account)
        self.assertTrue('US:Bank' in real_account)
        self.assertTrue('US:Bank:Checking' in real_account)


class TestRealization(unittest.TestCase):

    def test_realize(self):
        pass

    def test_assoc_entry_with_real_account(self):
        pass

    def test_create_real_accounts_tree(self):
        pass

    def test_filter_tree(self):
        pass

    def test_get_subpostings(self):
        pass

    def test__get_subpostings(self):
        pass

    def test_dump_tree_balances(self):
        pass

    def test_compare_realizations(self):
        pass

    def test_real_cost_as_dict(self):
        pass

    def test_iterate_with_balance(self):
        pass




# do_trace = False

# def realizedoc(fun):
#     """Decorator that parses, pads and realizes the function's docstring as an
#     argument."""
#     @functools.wraps(fun)
#     def newfun(self):
#         entries, errors, options = parser.parse_string(textwrap.dedent(fun.__doc__))

#         real_accounts = realization.realize(entries)
#         if do_trace and errors:
#             trace_errors(real_accounts, errors)
#         return fun(self, entries, real_accounts, errors)
#     return newfun

# def trace_errors(real_accounts, errors):
#     print()
#     print("ERRORS")
#     print(printer.format_errors(errors))
#     print()
#     print("REAL_ACCOUNTS")
#     for account_name, real_account in real_accounts.items():
#         if real_account.postings:
#             print('  ', real_account.account.name)
#             for posting in real_account.postings:
#                 print('      {}'.format(posting))
#     print()



# class TestRealization(unittest.TestCase):

#     @parsedoc
#     def test_simple_realize(self, entries, errors, options):
#         """
#           2013-05-01 open Assets:US:Checking:Sub   USD
#           2013-05-01 open Expenses:Stuff
#           2013-05-02 txn "Testing!"
#             Assets:US:Checking:Sub            100 USD
#             Expenses:Stuff           -100 USD
#         """
#         real_accounts = realization.realize(entries)
#         for real_account in real_accounts:
#             assert isinstance(real_account, realization.RealAccount)

#         real_accounts2 = realization.realize(entries)

#         for name in 'Assets:US:Checking:Sub', 'Expenses:Stuff':
#             lookup_account1 = real_accounts[name]
#             assert lookup_account1.fullname == name
#             lookup_account2 = real_accounts2[name]
#             assert lookup_account2.fullname == name






# FIXME: please DO test the realization of a transaction that has multiple legs on the same account!




__incomplete__ = True
