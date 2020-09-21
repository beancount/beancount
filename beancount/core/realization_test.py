"""Unit tests for realizations.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import copy
import datetime
import operator
import unittest

from beancount.core.number import D
from beancount.core.amount import A
from beancount.core.realization import RealAccount
from beancount.core import realization
from beancount.core import data
from beancount.core import inventory
from beancount.core import position
from beancount.core import account_types
from beancount.core import display_context
from beancount.utils import test_utils
from beancount import loader


def create_simple_account():
    ra0 = RealAccount('')
    ra0['Assets'] = RealAccount('Assets')
    ra0['Assets']['US'] = RealAccount('Assets:US')
    ra0['Assets']['US']['Bank'] = RealAccount('Assets:US:Bank')
    ra0['Assets']['US']['Bank']['Checking'] = RealAccount('Assets:US:Bank:Checking')
    return ra0


def create_real(account_value_pairs):
    real_root = RealAccount('')
    for account_name, value in account_value_pairs:
        real_account = realization.get_or_create(real_root, account_name)
        real_account.balance += inventory.from_string(value)
    return real_root


class TestRealAccount(unittest.TestCase):

    def test_ctor(self):
        ra0 = RealAccount('Assets:US:Bank:Checking')
        self.assertEqual(0, len(ra0))
        ra0 = RealAccount('Equity')
        ra0 = RealAccount('')
        with self.assertRaises(Exception):
            ra0 = RealAccount(None)

    def test_str(self):
        ra0 = RealAccount('Assets:US:Bank:Checking')
        self.assertEqual('{}', str(ra0))
        #self.assertEqual('RealAccount()', str(ra0))

        ra0 = create_simple_account()
        ra0_str = str(ra0)
        self.assertRegex(ra0_str, 'Assets')
        self.assertRegex(ra0_str, 'Bank')
        self.assertRegex(ra0_str, 'Checking')

    def test_equality(self):
        ra1 = RealAccount('Assets:US:Bank:Checking')
        ra1.balance.add_amount(A('100 USD'))
        ra1.txn_postings.extend(['a', 'b'])

        ra2 = RealAccount('Assets:US:Bank:Checking')
        ra2.balance.add_amount(A('100 USD'))
        ra2.txn_postings.extend(['a', 'b'])

        self.assertEqual(ra1, ra2)

        saved_balance = ra2.balance
        ra2.balance.add_amount(A('0.01 USD'))
        self.assertNotEqual(ra1, ra2)
        ra2.balance = saved_balance

        ra2.txn_postings.append('c')
        self.assertNotEqual(ra1, ra2)
        ra2.txn_postings.pop(-1)

        saved_account = ra2.account
        ra2.account += ':First'
        self.assertNotEqual(ra1, ra2)
        ra2.account = saved_account

    def test_getitem_setitem(self):
        ra0 = create_simple_account()
        self.assertTrue(isinstance(ra0['Assets'], RealAccount))
        self.assertTrue(isinstance(ra0['Assets']['US'], RealAccount))
        with self.assertRaises(KeyError):
            _ = ra0['Liabilities']

    def test_setitem_constraints(self):
        ra0 = RealAccount('')
        ra0['Assets'] = RealAccount('Assets')
        with self.assertRaises(KeyError):
            ra0['Assets'][42] = RealAccount('Assets:US')
        with self.assertRaises(ValueError):
            ra0['Assets']['US'] = 42
        with self.assertRaises(ValueError):
            ra0['Assets']['US'] = RealAccount('Assets:US:Checking')

    def test_clone(self):
        ra0 = RealAccount('')
        ra0['Assets'] = RealAccount('Assets')
        ra0.balance = 42
        ra0.txn_postings.append('posting1')
        ra0.txn_postings.append('posting2')

        ra0_clone = copy.copy(ra0)
        self.assertEqual(42, ra0_clone.balance)
        self.assertEqual(['posting1', 'posting2'], ra0_clone.txn_postings)
        self.assertEqual({'Assets'}, ra0_clone.keys())

        ra0_clone = ra0.copy()
        self.assertEqual(42, ra0_clone.balance)
        self.assertEqual(['posting1', 'posting2'], ra0_clone.txn_postings)
        self.assertEqual({'Assets'}, ra0_clone.keys())


class TestRealGetters(unittest.TestCase):

    def test_get(self):
        ra0 = create_simple_account()
        self.assertEqual('Assets',
                         realization.get(ra0, 'Assets').account)
        self.assertEqual('Assets:US:Bank',
                         realization.get(ra0, 'Assets:US:Bank').account)
        self.assertEqual('Assets:US:Bank:Checking',
                         realization.get(ra0, 'Assets:US:Bank:Checking').account)
        self.assertEqual(None, realization.get(ra0, 'Assets:US:Bank:Savings'))
        self.assertEqual(42, realization.get(ra0, 'Assets:US:Bank:Savings', 42))
        with self.assertRaises(ValueError):
            self.assertEqual(42, realization.get(ra0, None))
        self.assertEqual(None, realization.get(ra0, ''))

    def test_get_or_create(self):
        ra0 = RealAccount('')
        ra0_checking = realization.get_or_create(ra0, 'Assets:US:Bank:Checking')
        realization.get_or_create(ra0, 'Assets:US:Bank:Savings')
        self.assertEqual('Assets:US:Bank:Checking', ra0_checking.account)
        self.assertEqual({'Assets'}, ra0.keys())
        self.assertEqual({'Checking', 'Savings'}, ra0['Assets']['US']['Bank'].keys())

        ra0_assets = ra0['Assets']
        ra0_assets2 = realization.get_or_create(ra0, 'Assets')
        self.assertTrue(ra0_assets2 is ra0_assets)

    def test_contains(self):
        ra0 = RealAccount('')
        realization.get_or_create(ra0, 'Assets:US:Bank:Checking')
        realization.get_or_create(ra0, 'Assets:US:Bank:Savings')
        self.assertTrue(realization.contains(ra0, 'Assets:US:Bank:Checking'))
        self.assertTrue(realization.contains(ra0, 'Assets:US:Bank:Savings'))
        self.assertFalse(realization.contains(ra0, 'Assets:US:Cash'))

    def test_iter_children(self):
        ra0 = RealAccount('')
        for account_name in ['Assets:US:Bank:Checking',
                             'Assets:US:Bank:Savings',
                             'Assets:US:Cash',
                             'Assets:CA:Cash']:
            realization.get_or_create(ra0, account_name)

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
                         [ra.account for ra in realization.iter_children(ra0)])

        # Test enumerating leaves only.
        self.assertEqual(['Assets:CA:Cash',
                          'Assets:US:Bank:Checking',
                          'Assets:US:Bank:Savings',
                          'Assets:US:Cash'],
                         [ra.account for ra in realization.iter_children(ra0, True)])


class TestRealization(unittest.TestCase):

    @loader.load_doc()
    def test_postings_by_account(self, entries, errors, _):
        """
        option "plugin_processing_mode" "raw"

        2012-01-01 open Expenses:Restaurant
        2012-01-01 open Expenses:Movie
        2012-01-01 open Assets:Cash
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Equity:Opening-Balances

        2012-01-15 pad Liabilities:CreditCard Equity:Opening-Balances

        2012-03-01 * "Food"
          Expenses:Restaurant     100 CAD
          Assets:Cash            -100 CAD

        2012-03-10 * "Food again"
          Expenses:Restaurant      80 CAD
          Liabilities:CreditCard  -80 CAD

        ;; Two postings on the same account.
        2012-03-15 * "Two Movies"
          Expenses:Movie           10 CAD
          Expenses:Movie           10 CAD
          Liabilities:CreditCard  -20 CAD

        2012-03-20 note Liabilities:CreditCard "Called Amex, asked about 100 CAD dinner"

        2012-03-28 document Liabilities:CreditCard "march-statement.pdf"

        2013-04-01 balance Liabilities:CreditCard   204 CAD

        2014-01-01 close Liabilities:CreditCard
        """
        self.assertEqual(0, len(errors))

        txn_postings_map = realization.postings_by_account(entries)
        self.assertTrue(isinstance(txn_postings_map, dict))

        self.assertEqual([data.Open, data.TxnPosting],
                         list(map(type, txn_postings_map['Assets:Cash'])))

        self.assertEqual([data.Open, data.TxnPosting, data.TxnPosting],
                         list(map(type, txn_postings_map['Expenses:Restaurant'])))

        self.assertEqual([data.Open,
                          data.TxnPosting,
                          data.TxnPosting],
                         list(map(type, txn_postings_map['Expenses:Movie'])))

        self.assertEqual([data.Open,
                          data.Pad,
                          data.TxnPosting, data.TxnPosting, # data.TxnPosting,
                          data.Note,
                          data.Document,
                          data.Balance,
                          data.Close],
                         list(map(type, txn_postings_map['Liabilities:CreditCard'])))

        self.assertEqual([data.Open, data.Pad],
                         list(map(type, txn_postings_map['Equity:Opening-Balances'])))

    def test_realize_empty(self):
        real_account = realization.realize([])
        self.assertTrue(isinstance(real_account, realization.RealAccount))
        self.assertEqual(real_account.account, '')

    def test_realize_min_accoumts(self):
        real_account = realization.realize(
            [], account_types.DEFAULT_ACCOUNT_TYPES)
        self.assertTrue(isinstance(real_account, realization.RealAccount))
        self.assertEqual(real_account.account, '')
        self.assertEqual(len(real_account), 5)
        self.assertEqual(set(account_types.DEFAULT_ACCOUNT_TYPES),
                         real_account.keys())

    @loader.load_doc()
    def test_simple_realize(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:US:Checking:Sub   USD
          2013-05-01 open Expenses:Stuff
          2013-05-02 txn "Testing!"
            Assets:US:Checking:Sub            100 USD
            Expenses:Stuff           -100 USD
        """
        real_root = realization.realize(entries)
        for real_account in realization.iter_children(real_root):
            assert isinstance(real_account, realization.RealAccount)

        for account_name in ['Assets:US:Checking:Sub',
                             'Expenses:Stuff']:
            real_account = realization.get(real_root, account_name)
            self.assertEqual(account_name, real_account.account)

    def test_realize(self):
        input_string = """
        2012-01-01 open Expenses:Restaurant
        2012-01-01 open Expenses:Movie
        2012-01-01 open Assets:Cash
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Equity:Opening-Balances

        2012-01-15 pad Liabilities:CreditCard Equity:Opening-Balances

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

        2012-03-28 document Liabilities:CreditCard "{filename}"

        2013-04-01 balance Liabilities:CreditCard   204 CAD

        2014-01-01 close Liabilities:CreditCard
        """
        # 'filename' is because we need an existing filename.
        entries, errors, _ = loader.load_string(input_string.format(filename=__file__))

        real_account = realization.realize(entries)
        ra0_movie = realization.get(real_account, 'Expenses:Movie')
        self.assertEqual('Expenses:Movie', ra0_movie.account)
        expected_balance = inventory.Inventory()
        expected_balance.add_amount(A('20 CAD'))
        self.assertEqual(expected_balance, ra0_movie.balance)


class TestRealFilter(unittest.TestCase):

    def test_filter_to_empty(self):
        # Test filtering down to empty.
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Liabilities:Bank:CreditCard', '3 USD')])
        real_copy = realization.filter(real_root, lambda ra0: False)
        self.assertTrue(real_copy is None)

    def test_filter_almost_all(self):
        # Test filtering that culls leaves, to make sure that works.
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Liabilities:USBank:CreditCard', '3 USD'),
                                 ('Assets', '100 USD'),
                                 ('Liabilities:US:Bank', '101 USD')])
        def ge100(ra0):
            return ra0.balance.get_currency_units('USD').number >= 100
        real_copy = realization.filter(real_root, ge100)
        self.assertTrue(real_copy is not None)
        self.assertEqual({'Assets', 'Liabilities:US:Bank'},
                         set(ra0.account
                             for ra0 in realization.iter_children(real_copy, True)))

    def test_filter_with_leaves(self):
        # Test filtering that keeps some leaf nodes with some intermediate nodes
        # that would otherwise be eliminated.
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Liabilities:USBank:CreditCard', '3 USD')])
        def not_empty(ra0):
            return not ra0.balance.is_empty()
        real_copy = realization.filter(real_root, not_empty)
        self.assertTrue(real_copy is not None)
        self.assertEqual({'Assets:US:Bank:Checking',
                          'Assets:US:Bank:Savings',
                          'Liabilities:USBank:CreditCard'},
                         set(ra0.account
                             for ra0 in realization.iter_children(real_copy, True)))

    def test_filter_no_leaves(self):
        # Test filtering that drops leaf nodes but that keeps intermediate
        # nodes.
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Assets:US', '100 USD'),
                                 ('Assets', '100 USD')])
        def ge100(ra0):
            return ra0.balance.get_currency_units('USD').number >= 100
        real_copy = realization.filter(real_root, ge100)
        self.assertTrue(real_copy is not None)
        self.assertEqual({'Assets:US'},
                         set(ra0.account
                             for ra0 in realization.iter_children(real_copy, True)))

    def test_filter_misc(self):
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Assets:US:Cash', '3 USD'),
                                 ('Assets:CA:Cash', '4 USD'),
                                 ('Liabilities:Bank:CreditCard', '5 USD'),
                                 ('Expenses:Food:Grocery', '6 USD'),
                                 ('Expenses:Food:Restaurant', '7 USD'),
                                 ('Expenses:Food:Alcohol', '8 USD'),
                                 ('Expenses:Food', '10 USD')])
        def even(real_account):
            return (not real_account.balance.is_empty() and
                    real_account.balance.get_currency_units('NOK').number % 2 == 0)
        real_even = realization.filter(real_root, even)
        self.assertTrue(all(map(even, realization.iter_children(real_even, True))))


class TestRealOther(test_utils.TestCase):

    @loader.load_doc()
    def test_get_postings(self, entries, errors, _):
        """
        option "plugin_processing_mode" "raw"

        2012-01-01 open Assets:Bank:Checking
        2012-01-01 open Expenses:Restaurant
        2012-01-01 open Expenses:Movie
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Equity:Opening-Balances

        2012-01-15 pad Assets:Bank:Checking Equity:Opening-Balances

        2012-03-01 * "Food"
          Expenses:Restaurant     11.11 CAD
          Assets:Bank:Checking   -11.11 CAD

        2012-03-05 * "Food"
          Expenses:Movie         22.22 CAD
          Assets:Bank:Checking  -22.22 CAD

        2012-03-10 * "Paying off credit card"
          Assets:Bank:Checking     -33.33 CAD
          Liabilities:CreditCard    33.33 CAD

        2012-03-20 note Assets:Bank:Checking "Bla bla 444.44"

        2013-04-01 balance Assets:Bank:Checking   555.00 CAD

        2013-04-20 price CAD 0.91 USD

        2013-04-21 event "location" "Somewhere, USA"

        2013-04-22 custom "customentry" Assets:Bank:Checking
        2013-04-22 custom "customentry" "just a string, no account"

        2013-05-01 close Assets:Bank:Checking
        """
        real_account = realization.realize(entries)
        postings = list(realization.get_postings(real_account))

        for (exp_type, exp_account, exp_number), entpost in zip([
                (data.Open, 'Assets:Bank:Checking', None),
                (data.Open, 'Expenses:Restaurant', None),
                (data.Open, 'Expenses:Movie', None),
                (data.Open, 'Liabilities:CreditCard', None),
                (data.Open, 'Equity:Opening-Balances', None),
                (data.Pad, 'Assets:Bank:Checking', None),
                #(data.TxnPosting, 'Assets:Bank:Checking', '621.66'),
                (data.Pad, 'Assets:Bank:Checking', None),
                #(data.TxnPosting, 'Equity:Opening-Balances', '-621.66'),
                (data.TxnPosting, 'Assets:Bank:Checking', '-11.11'),
                (data.TxnPosting, 'Expenses:Restaurant', '11.11'),
                (data.TxnPosting, 'Assets:Bank:Checking', '-22.22'),
                (data.TxnPosting, 'Expenses:Movie', '22.22'),
                (data.TxnPosting, 'Assets:Bank:Checking', '-33.33'),
                (data.TxnPosting, 'Liabilities:CreditCard', '33.33'),
                (data.Note, 'Assets:Bank:Checking', None),
                (data.Balance, 'Assets:Bank:Checking', None),
                (data.Custom, None, None),
                (data.Close, 'Assets:Bank:Checking', None),
        ], postings):
            self.assertEqual(exp_type, type(entpost))
            if isinstance(entpost, data.TxnPosting):
                entpost = entpost.posting
            if exp_account:
                self.assertEqual(exp_account, entpost.account)
            if exp_number:
                self.assertEqual(D(exp_number), entpost.units.number)

    def test_compare_realizations(self):
        # Check that value comparison uses our balance comparison properly.
        map1 = {'Assets:US:Bank:Checking': inventory.Inventory()}
        map2 = {'Assets:US:Bank:Checking': inventory.Inventory()}
        map2['Assets:US:Bank:Checking'].add_amount(A('0.01 USD'))
        self.assertNotEqual(map1, map2)

        # Now check this with accounts.
        root1 = RealAccount('')
        ra1 = realization.get_or_create(root1, 'Assets:US:Bank:Checking')
        ra1.balance.add_amount(A('0.01 USD'))
        root2 = RealAccount('')
        ra2 = realization.get_or_create(root2, 'Assets:US:Bank:Checking')
        ra2.balance.add_amount(A('0.01 USD'))
        self.assertEqual(ra1, ra2)

        root3 = copy.deepcopy(root2)
        ra3 = realization.get(root3, 'Assets:US:Bank:Checking')
        ra3.account = 'Liabilities:US:CreditCard'
        self.assertNotEqual(root1, root3)

        root3 = copy.deepcopy(root2)
        ra3 = realization.get(root3, 'Assets:US:Bank:Checking')
        ra3.balance.add_amount(A('0.01 CAD'))
        self.assertNotEqual(root1, root3)

        root3 = copy.deepcopy(root2)
        ra3 = realization.get(root3, 'Assets:US:Bank:Checking')
        ra3.txn_postings.append('posting')
        self.assertNotEqual(root1, root3)

        root3 = copy.deepcopy(root2)
        ra3 = realization.get(root3, 'Assets:US:Bank:Checking')
        ra3['Sub'] = RealAccount('Assets:US:Bank:Checking:Sub')
        self.assertNotEqual(root1, root3)

    @loader.load_doc()
    def test_iterate_with_balance(self, entries, _, __):
        """
        2012-01-01 open Assets:Bank:Checking
        2012-01-01 open Expenses:Restaurant
        2012-01-01 open Equity:Opening-Balances

        2012-01-15 pad Assets:Bank:Checking Equity:Opening-Balances

        2012-01-20 balance Assets:Bank:Checking  20.00 USD

        2012-03-01 * "With a single entry"
          Expenses:Restaurant     11.11 CAD
          Assets:Bank:Checking

        2012-03-02 * "With two entries"
          Expenses:Restaurant     20.01 CAD
          Expenses:Restaurant     20.02 CAD
          Assets:Bank:Checking

        2012-03-02 note Expenses:Restaurant  "This was good"

        2012-04-01 balance Expenses:Restaurant  51.14 CAD
        """
        root_account = realization.realize(entries)
        real_account = realization.get(root_account, 'Expenses:Restaurant')

        def simplify_rtuple(rtuple):
            return [(type(entry), len(postings), str(change), str(balance))
                    for entry, postings, change, balance in rtuple]

        # Surprisingly enough, this covers all the legal cases that occur in
        # practice (checked for full coverage manually if you like).
        rtuple = realization.iterate_with_balance(real_account.txn_postings[:-2])
        self.assertEqual([
            (data.Open        , 0 , '()'          , '()')          ,
            (data.Transaction , 1 , '(11.11 CAD)' , '(11.11 CAD)') ,
            (data.Transaction , 2 , '(40.03 CAD)' , '(51.14 CAD)') ,
            ], simplify_rtuple(rtuple))

        # Test it again with the final balance entry.
        rtuple = realization.iterate_with_balance(real_account.txn_postings)
        self.assertEqual([
            (data.Open        , 0 , '()'          , '()')          ,
            (data.Transaction , 1 , '(11.11 CAD)' , '(11.11 CAD)') ,
            (data.Transaction , 2 , '(40.03 CAD)' , '(51.14 CAD)') ,
            (data.Note        , 0 , '()'          , '(51.14 CAD)') ,
            (data.Balance     , 0 , '()'          , '(51.14 CAD)') ,
            ], simplify_rtuple(rtuple))

        # Test it out with valid input but with entries for the same transaction
        # separated by another entry. Swap the balance entry with the last
        # posting entry to test this.
        postings = list(real_account.txn_postings)
        postings[-3], postings[-2] = postings[-2], postings[-3]
        rtuple = realization.iterate_with_balance(postings)
        self.assertEqual([
            (data.Open        , 0 , '()'          , '()')          ,
            (data.Transaction , 1 , '(11.11 CAD)' , '(11.11 CAD)') ,
            (data.Transaction , 2 , '(40.03 CAD)' , '(51.14 CAD)') ,
            (data.Note        , 0 , '()'          , '(51.14 CAD)') ,
            (data.Balance     , 0 , '()'          , '(51.14 CAD)') ,
            ], simplify_rtuple(rtuple))

        # Go one step further and test it out with invalid date ordering.
        postings = list(real_account.txn_postings)
        postings[-1], postings[-2] = postings[-2], postings[-1]
        with self.assertRaises(AssertionError):
            list(realization.iterate_with_balance(postings))

    def test_compute_balance(self):
        real_root = create_real([('Assets:US:Bank:Checking', '100 USD'),
                                 ('Assets:US:Bank:Savings', '200 USD'),
                                 ('Assets:US:Bank', '10 USD'),
                                 ('Liabilities:Bank:CreditCard', '-500 USD')])
        balance = realization.compute_balance(real_root)
        self.assertEqual(inventory.from_string('-190 USD'), balance)

        balance = realization.compute_balance(realization.get(real_root, 'Assets:US:Bank'))
        self.assertEqual(inventory.from_string('310 USD'), balance)

    @loader.load_doc()
    def test_dump(self, entries, _, __):
        """
        2012-01-01 open Assets:Bank1:Checking
        2012-01-01 open Assets:Bank1:Savings
        2012-01-01 open Assets:Bank2:Checking
        2012-01-01 open Expenses:Restaurant
        2012-01-01 open Expenses:Movie
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Equity:Opening-Balances
        """
        real_account = realization.realize(entries)
        lines = realization.dump(real_account)
        self.assertEqual([
            ('|-- Assets              ',
             '|   |                   '),
            ('|   |-- Bank1           ',
             '|   |   |               '),
            ('|   |   |-- Checking    ',
             '|   |   |               '),
            ('|   |   `-- Savings     ',
             '|   |                   '),
            ('|   `-- Bank2           ',
             '|       |               '),
            ('|       `-- Checking    ',
             '|                       '),
            ('|-- Equity              ',
             '|   |                   '),
            ('|   `-- Opening-Balances',
             '|                       '),
            ('|-- Expenses            ',
             '|   |                   '),
            ('|   |-- Movie           ',
             '|   |                   '),
            ('|   `-- Restaurant      ',
             '|                       '),
            ('`-- Liabilities         ',
             '    |                   '),
            ('    `-- CreditCard      ',
             '                        '),
            ], [(first_line, cont_line)
                for first_line, cont_line, _1 in lines])

    @loader.load_doc()
    def test_dump_balances(self, entries, _, options_map):
        """
        2012-01-01 open Expenses:Restaurant
        2012-01-01 open Liabilities:US:CreditCard
        2012-01-01 open Liabilities:CA:CreditCard

        2014-05-30 *
          Liabilities:CA:CreditCard   123.45 CAD
          Expenses:Restaurant

        2014-05-31 *
          Liabilities:US:CreditCard   123.45 USD
          Expenses:Restaurant

        """
        real_account = realization.realize(entries)
        dformat = options_map['dcontext'].build(alignment=display_context.Align.DOT,
                                                reserved=2)
        self.assertLines("""
            |-- Expenses
            |   `-- Restaurant          -123.45 CAD
            |                           -123.45 USD
            `-- Liabilities
                |-- CA
                |   `-- CreditCard       123.45 CAD
                `-- US
                    `-- CreditCard       123.45 USD
        """, realization.dump_balances(real_account, dformat))


class TestRealMisc(unittest.TestCase):

    def test_index_key(self):
        objects = [object() for _ in range(10)]
        index = realization.index_key(objects, objects[4], lambda x: x, operator.is_)
        self.assertEqual(4, index)


class TestFindLastActive(unittest.TestCase):

    @loader.load_doc()
    def test_find_last_active_posting(self, entries, _, __):
        """
        2012-01-01 open Assets:Target
        2012-01-01 open Equity:Other

        2014-02-01 *
          Assets:Target            123.45 CAD
          Equity:Other

        2014-03-01 U "This should get ignored because of the unrealized flag."
          Assets:Target            -23.45 CAD
          Equity:Other

        ;; This should get ignored because it's not one of the directives checked for
        ;; active.
        2014-03-02 event "location" "Somewhere, Somewhereland"
        """
        real_account = realization.realize(entries)
        txn_postings = realization.get(real_account, 'Assets:Target').txn_postings
        txn_posting = realization.find_last_active_posting(txn_postings)
        self.assertEqual(datetime.date(2014, 2, 1), txn_posting.txn.date)


class TestComputeBalance(unittest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_compute_postings_balance(self, entries, _, __):
        """
        2014-01-01 open Assets:Bank:Checking
        2014-01-01 open Assets:Bank:Savings
        2014-01-01 open Assets:Investing
        2014-01-01 open Assets:Other

        2014-05-26 note Assets:Investing "Buying some shares"

        2014-05-30 *
          Assets:Bank:Checking  111.23 USD
          Assets:Bank:Savings   222.74 USD
          Assets:Bank:Savings   17.23 CAD
          Assets:Investing      10000 EUR
          Assets:Investing      32 HOOL {45.203 USD}
          Assets:Other          1000 EUR @ 1.78 GBP
          Assets:Other          1000 EUR @@ 1780 GBP
        """
        postings = entries[:-1] + entries[-1].postings
        computed_balance = realization.compute_postings_balance(postings)

        expected_balance = inventory.Inventory()
        expected_balance.add_amount(A('333.97 USD'))
        expected_balance.add_amount(A('17.23 CAD'))
        expected_balance.add_amount(A('32 HOOL'),
                                    position.Cost(D('45.203'), 'USD',
                                                  datetime.date(2014, 5, 30), None))
        expected_balance.add_amount(A('12000 EUR'))
        self.assertEqual(expected_balance, computed_balance)


if __name__ == '__main__':
    unittest.main()
