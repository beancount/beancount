__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import datetime
from collections import OrderedDict

from beancount.core import getters
from beancount.core import data
from beancount import loader


TEST_INPUT = """

2012-02-01 open Assets:US:Cash
2012-02-01 open Assets:US:Credit-Card
2012-02-01 open Expenses:Grocery
2012-02-01 open Expenses:Coffee
2012-02-01 open Expenses:Restaurant

2012-02-01 commodity HOOL
  name: "Hooli Corp."
  ticker: "NYSE:HOOLI"

2012-02-01 commodity PIPA
  name: "Pied Piper"

2012-05-18 * "Buying food" #dinner
  Expenses:Restaurant         100 USD
  Expenses:Grocery            200 USD
  Assets:US:Cash

2013-06-20 * "Whole Foods Market" "Buying books" #books #dinner ^ee89ada94a39
  Expenses:Restaurant         150 USD
  Assets:US:Credit-Card

2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
  Expenses:Coffee         5 USD
  Assets:US:Cash

2014-02-01 close Assets:US:Cash
2014-02-01 close Assets:US:Credit-Card

"""

class TestGetters(unittest.TestCase):

    def test_methods_coverage(self):
        for dispatcher in (getters.GetAccounts,):
            for klass in data.ALL_DIRECTIVES:
                self.assertTrue(hasattr(dispatcher, klass.__name__))

    def test_get_accounts_use_map(self):
        entries = loader.load_string(TEST_INPUT)[0]
        accounts_first, accounts_last = getters.get_accounts_use_map(entries)
        self.assertEqual({'Expenses:Coffee': datetime.date(2012, 2, 1),
                          'Expenses:Restaurant': datetime.date(2012, 2, 1),
                          'Assets:US:Cash': datetime.date(2012, 2, 1),
                          'Expenses:Grocery': datetime.date(2012, 2, 1),
                          'Assets:US:Credit-Card': datetime.date(2012, 2, 1)},
                         accounts_first)
        self.assertEqual({'Expenses:Coffee': datetime.date(2013, 6, 22),
                          'Expenses:Restaurant': datetime.date(2013, 6, 20),
                          'Assets:US:Cash': datetime.date(2014, 2, 1),
                          'Expenses:Grocery': datetime.date(2012, 5, 18),
                          'Assets:US:Credit-Card': datetime.date(2014, 2, 1)},
                         accounts_last)

    def test_get_accounts(self):
        entries = loader.load_string(TEST_INPUT)[0]
        accounts = getters.get_accounts(entries)
        self.assertEqual({'Assets:US:Cash',
                          'Assets:US:Credit-Card',
                          'Expenses:Grocery',
                          'Expenses:Coffee',
                          'Expenses:Restaurant'},
                         accounts)

    def test_get_entry_accounts(self):
        entries = loader.load_string(TEST_INPUT)[0]
        accounts = getters.get_entry_accounts(next(entry
                                                   for entry in entries
                                                   if isinstance(entry, data.Transaction)))
        self.assertEqual({'Assets:US:Cash',
                          'Expenses:Grocery',
                          'Expenses:Restaurant'},
                         accounts)

    def test_get_all_tags(self):
        entries = loader.load_string(TEST_INPUT)[0]
        tags = getters.get_all_tags(entries)
        self.assertEqual(['books', 'dinner'], tags)

    def test_get_all_payees(self):
        entries = loader.load_string(TEST_INPUT)[0]
        payees = getters.get_all_payees(entries)
        self.assertEqual(['La Colombe', 'Whole Foods Market'], payees)

    def test_get_all_links(self):
        entries = loader.load_string(TEST_INPUT)[0]
        links = getters.get_all_links(entries)
        self.assertEqual(['ee89ada94a39'], links)

    def test_get_leveln_parent_accounts(self):
        account_names = ['Assets:US:Cash',
                         'Assets:US:Credit-Card',
                         'Expenses:Grocery',
                         'Expenses:Coffee',
                         'Expenses:Restaurant']

        levels = getters.get_leveln_parent_accounts(account_names, 0, 0)
        self.assertEqual({'Assets', 'Expenses'}, set(levels))

        levels = getters.get_leveln_parent_accounts(account_names, 1, 0)
        self.assertEqual({'US', 'Grocery', 'Coffee', 'Restaurant'}, set(levels))

        levels = getters.get_leveln_parent_accounts(account_names, 2, 0)
        self.assertEqual({'Cash', 'Credit-Card'}, set(levels))

    def test_get_dict_accounts(self):
        account_names = ['Assets:US:Cash',
                         'Assets:US:Credit-Card',
                         'Expenses:Grocery',
                         'Expenses:Grocery:Bean',
                         'Expenses:Coffee',
                         'Expenses:Restaurant']

        LABEL = getters.get_dict_accounts.ACCOUNT_LABEL
        root = OrderedDict([(LABEL, True)])
        account_dict = OrderedDict([
            ('Assets', OrderedDict([
                ('US', OrderedDict([
                    ('Cash', root),
                    ('Credit-Card', root),
                ])),
            ])),
            ('Expenses', OrderedDict([
                ('Grocery', OrderedDict([
                    ('Bean', root), # Wrong order here
                    (LABEL, True),
                ])),
                ('Coffee', root),
                ('Restaurant', root),
            ])),
        ])
        self.assertNotEqual(
            getters.get_dict_accounts(account_names),
            account_dict
        )
        account_dict['Expenses']['Grocery'] = OrderedDict([
            (LABEL, True),
            ('Bean', root),
        ])

    def test_get_min_max_dates(self):
        entries = loader.load_string(TEST_INPUT)[0]
        mindate, maxdate = getters.get_min_max_dates(entries)
        self.assertEqual(datetime.date(2012, 2, 1), mindate)
        self.assertEqual(datetime.date(2014, 2, 1), maxdate)

    def test_get_active_years(self):
        entries = loader.load_string(TEST_INPUT)[0]
        years = list(getters.get_active_years(entries))
        self.assertEqual([2012, 2013, 2014], years)

    def test_get_account_open_close(self):
        entries = loader.load_string(TEST_INPUT)[0]
        ocmap = getters.get_account_open_close(entries)
        self.assertEqual(5, len(ocmap))

        def mapfound(account_name):
            open, close = ocmap[account_name]
            return (open is not None, close is not None)

        self.assertEqual(mapfound('Assets:US:Cash'), (True, True))
        self.assertEqual(mapfound('Assets:US:Credit-Card'), (True, True))
        self.assertEqual(mapfound('Expenses:Grocery'), (True, False))
        self.assertEqual(mapfound('Expenses:Coffee'), (True, False))
        self.assertEqual(mapfound('Expenses:Restaurant'), (True, False))

    @loader.load_doc(expect_errors=True)
    def test_get_account_open_close__duplicates(self, entries, _, __):
        """
        2014-01-01 open  Assets:Checking
        2014-01-02 open  Assets:Checking

        2014-01-28 close Assets:Checking
        2014-01-29 close Assets:Checking
        """
        open_close_map = getters.get_account_open_close(entries)
        self.assertEqual(1, len(open_close_map))
        open_entry, close_entry = open_close_map['Assets:Checking']
        self.assertEqual(datetime.date(2014, 1, 1), open_entry.date)
        self.assertEqual(datetime.date(2014, 1, 28), close_entry.date)

    def test_get_account_components(self):
        entries = loader.load_string(TEST_INPUT)[0]
        components = getters.get_account_components(entries)
        expected_components = {'US', 'Assets', 'Restaurant', 'Grocery',
                               'Cash', 'Coffee', 'Expenses', 'Credit-Card'}
        self.assertEqual(sorted(expected_components), components)

    def test_get_commodities_map(self):
        entries, _, options_map = loader.load_string(TEST_INPUT)
        commodity_map = getters.get_commodity_map(entries)
        self.assertEqual({'HOOL', 'PIPA', 'USD'}, commodity_map.keys())
        self.assertTrue(all(isinstance(value, data.Commodity)
                            for value in commodity_map.values()))
        self.assertEqual(commodity_map['HOOL'],
                         next(entry
                              for entry in entries
                              if isinstance(entry, data.Commodity)))

    def test_get_values_meta__single(self):
        entries, _, options_map = loader.load_string(TEST_INPUT)
        commodity_map = getters.get_commodity_map(entries)
        values = getters.get_values_meta(commodity_map, 'name', default='BLA')
        self.assertEqual({'USD': 'BLA',
                          'PIPA': 'Pied Piper',
                          'HOOL': 'Hooli Corp.'},
                         values)

    def test_get_values_meta__multi(self):
        entries, _, options_map = loader.load_string(TEST_INPUT)
        commodity_map = getters.get_commodity_map(entries)
        values = getters.get_values_meta(commodity_map, 'name', 'ticker')
        self.assertEqual({'HOOL': ('Hooli Corp.', 'NYSE:HOOLI'),
                          'PIPA': ('Pied Piper', None),
                          'USD': (None, None)},
                         values)
