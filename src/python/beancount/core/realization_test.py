"""Unit tests for realizations.
"""
import copy
import datetime
import io
import operator
import re
import unittest

from beancount.core.amount import D
from beancount.core.realization import RealAccount
from beancount.core import realization
from beancount.core import data
from beancount.core import inventory
from beancount.core import amount
from beancount.core import account_types
from beancount.parser import parsedoc
from beancount.utils import test_utils


def create_simple_account():
    ra = RealAccount('')
    ra['Assets'] = RealAccount('Assets')
    ra['Assets']['US'] = RealAccount('Assets:US')
    ra['Assets']['US']['Bank'] = RealAccount('Assets:US:Bank')
    ra['Assets']['US']['Bank']['Checking'] = RealAccount('Assets:US:Bank:Checking')
    return ra


def create_real(account_value_pairs):
    real_root = RealAccount('')
    for account_name, value in account_value_pairs:
        real_account = realization.get_or_create(real_root, account_name)
        real_account.balance += inventory.from_string(value)
    return real_root


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
        #self.assertEqual('RealAccount()', str(ra))

        ra = create_simple_account()
        ra_str = str(ra)
        self.assertTrue(re.search('Assets', ra_str))
        self.assertTrue(re.search('Bank', ra_str))
        self.assertTrue(re.search('Checking', ra_str))

    def test_equality(self):
        ra1 = RealAccount('Assets:US:Bank:Checking')
        ra1.balance.add_amount(amount.Amount('100', 'USD'))
        ra1.postings.extend(['a', 'b'])

        ra2 = RealAccount('Assets:US:Bank:Checking')
        ra2.balance.add_amount(amount.Amount('100', 'USD'))
        ra2.postings.extend(['a', 'b'])

        self.assertEqual(ra1, ra2)

        saved_balance = ra2.balance
        ra2.balance.add_amount(amount.Amount('0.01', 'USD'))
        self.assertNotEqual(ra1, ra2)
        ra2.balance = saved_balance

        ra2.postings.append('c')
        self.assertNotEqual(ra1, ra2)
        ra2.postings.pop(-1)

        saved_account = ra2.account
        ra2.account += ':First'
        self.assertNotEqual(ra1, ra2)
        ra2.account = saved_account

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

    def test_clone(self):
        ra = RealAccount('')
        ra_assets = ra['Assets'] = RealAccount('Assets')
        ra.balance = 42
        ra.postings.append('posting1')
        ra.postings.append('posting2')

        ra_clone = copy.copy(ra)
        self.assertEqual(42, ra_clone.balance)
        self.assertEqual(['posting1', 'posting2'], ra_clone.postings)
        self.assertEqual({'Assets'}, ra_clone.keys())

        ra_clone = ra.copy()
        self.assertEqual(42, ra_clone.balance)
        self.assertEqual(['posting1', 'posting2'], ra_clone.postings)
        self.assertEqual({'Assets'}, ra_clone.keys())


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

    @parsedoc
    def test_postings_by_account(self, entries, errors, _):
        """
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

        2012-03-28 document Liabilities:CreditCard "march-statement.pdf"

        2013-04-01 balance Liabilities:CreditCard   204 CAD

        2014-01-01 close Liabilities:CreditCard
        """
        self.assertEqual(0, len(errors))

        postings_map = realization.postings_by_account(entries)
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
                          data.Posting, data.Posting, # data.Posting,
                          data.Note,
                          data.Document,
                          data.Balance,
                          data.Close],
                         list(map(type, postings_map['Liabilities:CreditCard'])))

        self.assertEqual([data.Open, data.Pad],
                         list(map(type, postings_map['Equity:Opening-Balances'])))

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

    @parsedoc
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

    @parsedoc
    def test_realize(self, entries, errors, _):
        """
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

        2012-03-28 document Liabilities:CreditCard "march-statement.pdf"

        2013-04-01 balance Liabilities:CreditCard   204 CAD

        2014-01-01 close Liabilities:CreditCard
        """
        real_account = realization.realize(entries)
        ra_movie = realization.get(real_account, 'Expenses:Movie')
        self.assertEqual('Expenses:Movie', ra_movie.account)
        expected_balance = inventory.Inventory()
        expected_balance.add_amount(amount.Amount('20', 'CAD'))
        self.assertEqual(expected_balance, ra_movie.balance)


class TestRealFilter(unittest.TestCase):

    def test_filter_to_empty(self):
        # Test filtering down to empty.
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Liabilities:Bank:CreditCard', '3 USD')])
        real_copy = realization.filter(real_root, lambda ra: False)
        self.assertTrue(real_copy is None)

    def test_filter_almost_all(self):
        # Test filtering that culls leaves, to make sure that works.
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Liabilities:USBank:CreditCard', '3 USD'),
                                 ('Assets', '100 USD'),
                                 ('Liabilities:US:Bank', '101 USD')])
        def ge100(ra):
            return ra.balance.get_amount('USD').number >= 100
        real_copy = realization.filter(real_root, ge100)
        self.assertTrue(real_copy is not None)
        self.assertEqual({'Assets', 'Liabilities:US:Bank'},
                         set(ra.account
                             for ra in realization.iter_children(real_copy, True)))

    def test_filter_with_leaves(self):
        # Test filtering that keeps some leaf nodes with some intermediate nodes
        # that would otherwise be eliminated.
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Liabilities:USBank:CreditCard', '3 USD')])
        def not_empty(ra):
            return not ra.balance.is_empty()
        real_copy = realization.filter(real_root, not_empty)
        self.assertTrue(real_copy is not None)
        self.assertEqual({'Assets:US:Bank:Checking',
                          'Assets:US:Bank:Savings',
                          'Liabilities:USBank:CreditCard'},
                         set(ra.account
                             for ra in realization.iter_children(real_copy, True)))

    def test_filter_no_leaves(self):
        # Test filtering that drops leaf nodes but that keeps intermediate
        # nodes.
        real_root = create_real([('Assets:US:Bank:Checking', '1 USD'),
                                 ('Assets:US:Bank:Savings', '2 USD'),
                                 ('Assets:US', '100 USD'),
                                 ('Assets', '100 USD')])
        def ge100(ra):
            return ra.balance.get_amount('USD').number >= 100
        real_copy = realization.filter(real_root, ge100)
        self.assertTrue(real_copy is not None)
        self.assertEqual({'Assets:US'},
                         set(ra.account
                             for ra in realization.iter_children(real_copy, True)))

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
                    real_account.balance.get_amount('NOK').number % 2 == 0)
        real_even = realization.filter(real_root, even)
        self.assertTrue(all(map(even, realization.iter_children(real_even, True))))


class TestRealOther(test_utils.TestCase):

    @parsedoc
    def test_get_postings(self, entries, errors, _):
        """
        2012-01-01 open Assets:Bank:Checking
        2012-01-01 open Expenses:Restaurant
        2012-01-01 open Expenses:Movie
        2012-01-01 open Liabilities:CreditCard
        2012-01-01 open Equity:Opening-Balances

        2012-01-15 pad Assets:Bank:Checking Equity:Opening-Balances

        2012-03-01 * "Food"
          Expenses:Restaurant     11.11 CAD
          Assets:Bank:Checking

        2012-03-05 * "Food"
          Expenses:Movie         22.22 CAD
          Assets:Bank:Checking

        2012-03-10 * "Paying off credit card"
          Assets:Bank:Checking     -33.33 CAD
          Liabilities:CreditCard

        2012-03-20 note Assets:Bank:Checking "Bla bla 444.44"

        2013-04-01 balance Assets:Bank:Checking   555.00 CAD

        2013-04-20 price CAD 0.91 USD

        2013-04-21 event "location" "Somewhere, USA"

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
            #(data.Posting, 'Assets:Bank:Checking', '621.66'),
            (data.Pad, 'Assets:Bank:Checking', None),
            #(data.Posting, 'Equity:Opening-Balances', '-621.66'),
            (data.Posting, 'Assets:Bank:Checking', '-11.11'),
            (data.Posting, 'Expenses:Restaurant', '11.11'),
            (data.Posting, 'Assets:Bank:Checking', '-22.22'),
            (data.Posting, 'Expenses:Movie', '22.22'),
            (data.Posting, 'Assets:Bank:Checking', '-33.33'),
            (data.Posting, 'Liabilities:CreditCard', '33.33'),
            (data.Note, 'Assets:Bank:Checking', None),
            (data.Balance, 'Assets:Bank:Checking', None),
            (data.Close, 'Assets:Bank:Checking', None),
            ], postings):

            self.assertEqual(exp_type, type(entpost))
            if exp_account:
                self.assertEqual(exp_account, entpost.account)
            if exp_number:
                self.assertEqual(D(exp_number), entpost.position.number)

    def test_compare_realizations(self):
        # Check that value comparison uses our balance comparison properly.
        map1 = {'Assets:US:Bank:Checking': inventory.Inventory()}
        map2 = {'Assets:US:Bank:Checking': inventory.Inventory()}
        map2['Assets:US:Bank:Checking'].add_amount(amount.Amount('0.01', 'USD'))
        self.assertNotEqual(map1, map2)

        # Now check this with accounts.
        root1 = RealAccount('')
        ra1 = realization.get_or_create(root1, 'Assets:US:Bank:Checking')
        ra1.balance.add_amount(amount.Amount('0.01', 'USD'))
        root2 = RealAccount('')
        ra2 = realization.get_or_create(root2, 'Assets:US:Bank:Checking')
        ra2.balance.add_amount(amount.Amount('0.01', 'USD'))
        self.assertEqual(ra1, ra2)

        root3 = copy.deepcopy(root2)
        ra3 = realization.get(root3, 'Assets:US:Bank:Checking')
        ra3.account = 'Liabilities:US:CreditCard'
        self.assertNotEqual(root1, root3)

        root3 = copy.deepcopy(root2)
        ra3 = realization.get(root3, 'Assets:US:Bank:Checking')
        ra3.balance.add_amount(amount.Amount('0.01', 'CAD'))
        self.assertNotEqual(root1, root3)

        root3 = copy.deepcopy(root2)
        ra3 = realization.get(root3, 'Assets:US:Bank:Checking')
        ra3.postings.append('posting')
        self.assertNotEqual(root1, root3)

        root3 = copy.deepcopy(root2)
        ra3 = realization.get(root3, 'Assets:US:Bank:Checking')
        ra3['Sub'] = RealAccount('Assets:US:Bank:Checking:Sub')
        self.assertNotEqual(root1, root3)

    @parsedoc
    def test_iterate_with_balance(self, entries, _, __):
        """
        2012-01-01 open Assets:Bank:Checking
        2012-01-01 open Expenses:Restaurant

        2012-01-15 pad Assets:Bank:Checking Equity:Opening-Balances

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

        # Surprinsingly enough, this covers all the legal cases that occur in
        # practice (checked for full coverage manually if you like).
        # pylint: disable=bad-whitespace
        rtuple = realization.iterate_with_balance(real_account.postings[:-2])
        self.assertEqual([
            (data.Open        , 0 , 'Inventory()'          , 'Inventory()')          ,
            (data.Transaction , 1 , 'Inventory(11.11 CAD)' , 'Inventory(11.11 CAD)') ,
            (data.Transaction , 2 , 'Inventory(40.03 CAD)' , 'Inventory(51.14 CAD)') ,
            ], simplify_rtuple(rtuple))

        # Test it again with the final balance entry.
        rtuple = realization.iterate_with_balance(real_account.postings)
        self.assertEqual([
            (data.Open        , 0 , 'Inventory()'          , 'Inventory()')          ,
            (data.Transaction , 1 , 'Inventory(11.11 CAD)' , 'Inventory(11.11 CAD)') ,
            (data.Transaction , 2 , 'Inventory(40.03 CAD)' , 'Inventory(51.14 CAD)') ,
            (data.Note        , 0 , 'Inventory()'          , 'Inventory(51.14 CAD)') ,
            (data.Balance     , 0 , 'Inventory()'          , 'Inventory(51.14 CAD)') ,
            ], simplify_rtuple(rtuple))

        # Test it out with valid input but with entries for the same transaction
        # separated by another entry. Swap the balance entry with the last
        # posting entry to test this.
        postings = list(real_account.postings)
        postings[-3], postings[-2] = postings[-2], postings[-3]
        rtuple = realization.iterate_with_balance(postings)
        self.assertEqual([
            (data.Open        , 0 , 'Inventory()'          , 'Inventory()')          ,
            (data.Transaction , 1 , 'Inventory(11.11 CAD)' , 'Inventory(11.11 CAD)') ,
            (data.Transaction , 2 , 'Inventory(40.03 CAD)' , 'Inventory(51.14 CAD)') ,
            (data.Note        , 0 , 'Inventory()'          , 'Inventory(51.14 CAD)') ,
            (data.Balance     , 0 , 'Inventory()'          , 'Inventory(51.14 CAD)') ,
            ], simplify_rtuple(rtuple))

        # Go one step further and test it out with invalid date ordering.
        postings = list(real_account.postings)
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

    @parsedoc
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
        oss = io.StringIO()
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
                for first_line, cont_line, _ in lines])

    @parsedoc
    def test_dump_balances(self, entries, _, __):
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
        self.assertLines("""
            |-- Expenses
            |   `-- Restaurant          -123.45 CAD
            |                           -123.45 USD
            `-- Liabilities
                |-- CA
                |   `-- CreditCard       123.45 CAD
                `-- US
                    `-- CreditCard       123.45 USD
        """, realization.dump_balances(real_account))


class TestRealMisc(unittest.TestCase):

    def test_index_key(self):
        objects = [object() for _ in range(10)]
        index = realization.index_key(objects, objects[4], lambda x: x, operator.is_)
        self.assertEqual(4, index)


class TestFindLastActive(unittest.TestCase):

    @parsedoc
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
        2014-03-02 document Assets:Target  "/path/to/somewhere.txt"

        """
        real_account = realization.realize(entries)
        postings = realization.get(real_account, 'Assets:Target').postings
        posting = realization.find_last_active_posting(postings)
        self.assertEqual(datetime.date(2014, 2, 1), posting.entry.date)
