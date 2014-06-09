import unittest
import datetime

from beancount.core import getters
from beancount.parser import parser


TEST_INPUT = """

2012-02-01 open Assets:US:Cash
2012-02-01 open Assets:US:Credit-Card
2012-02-01 open Expenses:Grocery
2012-02-01 open Expenses:Coffee
2012-02-01 open Expenses:Restaurant

2012-05-18 * "Buying food" #dinner
  Expenses:Restaurant         100 USD
  Expenses:Grocery            200 USD
  Assets:US:Cash

2013-06-20 * "Whole Foods Market" | "Buying books" #books #dinner ^ee89ada94a39
  Expenses:Restaurant         150 USD
  Assets:US:Credit-Card

2013-06-22 * "La Colombe" | "Buying coffee"  ^ee89ada94a39
  Expenses:Coffee         5 USD
  Assets:US:Cash

2014-02-01 close Assets:US:Cash
2014-02-01 close Assets:US:Credit-Card

"""

class TestGetters(unittest.TestCase):

    def test_get_accounts(self):
        entries = parser.parse_string(TEST_INPUT)[0]
        accounts = getters.get_accounts(entries)
        self.assertEqual({'Assets:US:Cash',
                          'Assets:US:Credit-Card',
                          'Expenses:Grocery',
                          'Expenses:Coffee',
                          'Expenses:Restaurant'},
                         accounts)

    def test_get_all_tags(self):
        entries = parser.parse_string(TEST_INPUT)[0]
        tags = getters.get_all_tags(entries)
        self.assertEqual({'books', 'dinner'}, tags)

    def test_get_all_payees(self):
        entries = parser.parse_string(TEST_INPUT)[0]
        payees = getters.get_all_payees(entries)
        self.assertEqual({'Whole Foods Market', 'La Colombe'}, payees)

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

    def test_get_min_max_dates(self):
        entries = parser.parse_string(TEST_INPUT)[0]
        mindate, maxdate = getters.get_min_max_dates(entries)
        self.assertEqual(datetime.date(2012, 2, 1), mindate)
        self.assertEqual(datetime.date(2014, 2, 1), maxdate)

    def test_get_active_years(self):
        entries = parser.parse_string(TEST_INPUT)[0]
        years = list(getters.get_active_years(entries))
        self.assertEqual([2012, 2013, 2014], years)

    def test_get_account_open_close(self):
        entries = parser.parse_string(TEST_INPUT)[0]
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

    def test_get_account_components(self):
        entries = parser.parse_string(TEST_INPUT)[0]
        components = getters.get_account_components(entries)
        expected_components = {'US', 'Assets', 'Restaurant', 'Grocery',
                               'Cash', 'Coffee', 'Expenses', 'Credit-Card'}
        self.assertEqual(expected_components, components)
