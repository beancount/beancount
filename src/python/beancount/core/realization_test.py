"""
Unit tests for realizations.
"""
import unittest
import textwrap
import functools
import re

from beancount import parser

from beancount.core.realization import RealAccount
from beancount.core import realization
from beancount.core import data
from beancount.parser import documents
from beancount.loader import loaddoc


def create_simple_account():
    ra = RealAccount('')
    ra['Assets'] = RealAccount('Assets')
    ra['Assets']['US'] = RealAccount('Assets:US')
    ra['Assets']['US']['Bank'] = RealAccount('Assets:US:Bank')
    ra['Assets']['US']['Bank']['Checking'] = RealAccount('Assets:US:Bank:Checking')
    return ra


class TestRealAccount(unittest.TestCase):

    def test_ctor(self):
        ra = RealAccount('Assets:US:Bank:Checking')
        self.assertEqual(0, len(ra))
        ra = RealAccount('Equity')
        ra = RealAccount('')
        with self.assertRaises(Exception):
            ra = RealAccount(None)

    def test_str(self):
        ra = RealAccount('Assets:US:Bank:Checking')
        self.assertEqual('{}', str(ra))

        ra = create_simple_account()
        ra_str = str(ra)
        self.assertTrue(re.search('Assets', ra_str))
        self.assertTrue(re.search('Bank', ra_str))
        self.assertTrue(re.search('Checking', ra_str))

    def test_getitem_setitem(self):
        ra = create_simple_account()
        self.assertTrue(isinstance(ra['Assets'], RealAccount))
        self.assertTrue(isinstance(ra['Assets']['US'], RealAccount))
        with self.assertRaises(KeyError):
            ra['Liabilities']

    def test_setitem_constraints(self):
        ra = RealAccount('')
        ra['Assets'] = RealAccount('Assets')
        with self.assertRaises(KeyError):
            ra['Assets'][42] = RealAccount('Assets:US')
        with self.assertRaises(ValueError):
            ra['Assets']['US'] = 42
        with self.assertRaises(ValueError):
            ra['Assets']['US'] = RealAccount('Assets:US:Checking')


class TestRealGetters(unittest.TestCase):

    def test_get(self):
        ra = create_simple_account()
        self.assertEqual('Assets',
                         realization.get(ra, 'Assets').account)
        self.assertEqual('Assets:US:Bank',
                         realization.get(ra, 'Assets:US:Bank').account)
        self.assertEqual('Assets:US:Bank:Checking',
                         realization.get(ra, 'Assets:US:Bank:Checking').account)
        self.assertEqual(None, realization.get(ra, 'Assets:US:Bank:Savings'))
        self.assertEqual(42, realization.get(ra, 'Assets:US:Bank:Savings', 42))
        with self.assertRaises(ValueError):
            self.assertEqual(42, realization.get(ra, None))
        self.assertEqual(None, realization.get(ra, ''))

    def test_get_or_create(self):
        ra = RealAccount('')
        ra_checking = realization.get_or_create(ra, 'Assets:US:Bank:Checking')
        ra_savings = realization.get_or_create(ra, 'Assets:US:Bank:Savings')
        self.assertEqual('Assets:US:Bank:Checking', ra_checking.account)
        self.assertEqual({'Assets'}, ra.keys())
        self.assertEqual({'Checking', 'Savings'}, ra['Assets']['US']['Bank'].keys())

        ra_assets = ra['Assets']
        ra_assets2 = realization.get_or_create(ra, 'Assets')
        self.assertTrue(ra_assets2 is ra_assets)

    def test_contains(self):
        ra = RealAccount('')
        ra_checking = realization.get_or_create(ra, 'Assets:US:Bank:Checking')
        ra_savings = realization.get_or_create(ra, 'Assets:US:Bank:Savings')
        self.assertTrue(realization.contains(ra, 'Assets:US:Bank:Checking'))
        self.assertTrue(realization.contains(ra, 'Assets:US:Bank:Savings'))
        self.assertFalse(realization.contains(ra, 'Assets:US:Cash'))

    def test_iter_children(self):
        ra = RealAccount('')
        for account_name in ['Assets:US:Bank:Checking',
                             'Assets:US:Bank:Savings',
                             'Assets:US:Cash',
                             'Assets:CA:Cash']:
            realization.get_or_create(ra, account_name)

        # Test enumerating all accounts.
        self.assertEqual(['',
                          'Assets',
                          'Assets:CA',
                          'Assets:CA:Cash',
                          'Assets:US',
                          'Assets:US:Bank',
                          'Assets:US:Bank:Checking',
                          'Assets:US:Bank:Savings',
                          'Assets:US:Cash'],
                         [ra.account for ra in realization.iter_children(ra)])

        # Test enumerating leaves only.
        self.assertEqual(['Assets:CA:Cash',
                          'Assets:US:Bank:Checking',
                          'Assets:US:Bank:Savings',
                          'Assets:US:Cash'],
                         [ra.account for ra in realization.iter_children(ra, True)])


class TestRealization(unittest.TestCase):

    @loaddoc
    def test_group_by_account(self, entries, errors, _):
        """
        2012-01-01 open Expenses:Restaurant
        2012-01-01 open Expenses:Movie
        2012-01-01 open Assets:Cash
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Equity:OpeningBalances

        2012-01-15 pad Liabilities:CreditCard Equity:OpeningBalances

        2012-03-01 * "Food"
          Expenses:Restaurant     100 CAD
          Assets:Cash

        2012-03-10 * "Food again"
          Expenses:Restaurant     80 CAD
          Liabilities:CreditCard

        ;; Two postings on the same account.
        2012-03-15 * "Two Movies"
          Expenses:Movie     10 CAD
          Expenses:Movie     10 CAD
          Liabilities:CreditCard

        2012-03-20 note Liabilities:CreditCard "Called Amex, asked about 100 CAD dinner"

        2012-03-28 document Liabilities:CreditCard "march-statement.pdf"

        2013-04-01 balance Liabilities:CreditCard   204 CAD

        2014-01-01 close Liabilities:CreditCard
        """
        self.assertEqual([documents.DocumentError], list(map(type, errors)))

        postings_map = realization.group_by_account(entries)
        self.assertTrue(isinstance(postings_map, dict))

        self.assertEqual([data.Open, data.Posting],
                         list(map(type, postings_map['Assets:Cash'])))

        self.assertEqual([data.Open, data.Posting, data.Posting],
                         list(map(type, postings_map['Expenses:Restaurant'])))

        self.assertEqual([data.Open,
                          data.Posting,
                          data.Posting],
                         list(map(type, postings_map['Expenses:Movie'])))

        self.assertEqual([data.Open,
                          data.Pad,
                          data.Posting, data.Posting, data.Posting,
                          data.Note,
                          data.Document,
                          data.Balance,
                          data.Close],
                         list(map(type, postings_map['Liabilities:CreditCard'])))

        self.assertEqual([data.Open, data.Pad, data.Posting],
                         list(map(type, postings_map['Equity:OpeningBalances'])))


    def test_compute_postings_balance(self):
        pass
        #balance = realization.compute_postings_balance(entries)


    def test_realize(self):
        pass






#         real_account = realization.realize(entries)

#         self.assertEqual({
#             'Assets': {
#                 'Cash': {}},
#             'Equity': {
#                 'OpeningBalances': {}},
#             'Expenses': {
#                 'Movie': {},
#                 'Restaurant': {}},
#             'Liabilities': {
#                 'CreditCard': {}}},
#                          real_account.asdict())





#     def test_ensure_min_accounts(self):
#         root = RealAccount('')
#         us = RealAccount('Assets:US')
#         assets = RealAccount('Assets')
#         root.add(assets)
#         assets.add(us)
#         realization.ensure_min_accounts(root, ['Assets', 'Income', 'Expenses'])
#         self.assertEqual({'Assets': {'US': {}}, 'Expenses': {}, 'Income': {}},
#                          root.asdict())
#         self.assertEqual(['Assets', 'Income', 'Expenses'],
#                          [x.fullname for x in root.get_children()])



#     def test_assoc_entry_with_real_account(self):
#         pass

#     def test_create_real_accounts_tree(self):
#         pass

#     def test_filter_tree(self):
#         pass

#     def test_get_subpostings(self):
#         pass

#     def test__get_subpostings(self):
#         pass

#     def test_dump_tree_balances(self):
#         pass

#     def test_compare_realizations(self):
#         pass

#     def test_real_cost_as_dict(self):
#         pass

#     def test_iterate_with_balance(self):
#         pass




# do_trace = False

def realizedoc(fun):
    """Decorator that parses, pads and realizes the function's docstring as an
    argument."""
    @functools.wraps(fun)
    def newfun(self):
        entries, errors, options_map = parser.parse_string(textwrap.dedent(fun.__doc__))
        real_accounts = realization.realize(entries)
        if do_trace and errors:
            trace_errors(real_accounts, errors)
        return fun(self, entries, real_accounts, errors)
    return newfun

# def trace_errors(real_accounts, errors):
#     print()
#     print("ERRORS")
#     print(printer.format_errors(errors))
#     print()
#     print("REAL_ACCOUNTS")
#     for account_name, real_account in real_accounts.items():
#         if real_account.postings:
#             print('  ', real_account.fullname)
#             for posting in real_account.postings:
#                 print('      {}'.format(posting))
#     print()



# class TestRealization(unittest.TestCase):

#     @parsedoc
#     def test_simple_realize(self, entries, errors, options_map):
#         """
#           2013-05-01 open Assets:US:Checking:Sub   USD
#           2013-05-01 open Expenses:Stuff
#           2013-05-02 txn "Testing!"
#             Assets:US:Checking:Sub            100 USD
#             Expenses:Stuff           -100 USD
#         """
#         real_accounts = realization.realize(entries)
#         for real_account in real_accounts.values_recursively():
#             assert isinstance(real_account, realization.RealAccount)

#         real_accounts2 = realization.realize(entries)

#         for name in 'Assets:US:Checking:Sub', 'Expenses:Stuff':
#             lookup_account1 = real_accounts[name]
#             assert lookup_account1.fullname == name
#             lookup_account2 = real_accounts2[name]
#             assert lookup_account2.fullname == name





# Loader tests:

# Check that an account without an Open directive has one that gets
# automatically inserted for it.

# FIXME: please DO test the realization of a transaction that has multiple legs
# on the same account!

# Add a test for realizing with no entries.



__incomplete__ = True
