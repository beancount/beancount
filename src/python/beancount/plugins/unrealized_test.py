__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import re

from beancount.plugins import unrealized
from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core import data
from beancount.parser import options
from beancount.ops import validation
from beancount import loader


def get_entries_with_narration(entries, regexp):
    """Return the entries whose narration matches the regexp.

    Args:
      entries: A list of directives.
      regexp: A regular expression string, to be matched against the
        narration field of transactions.
    Returns:
      A list of directives.
    """
    return [entry
            for entry in entries
            if (isinstance(entry, data.Transaction) and
                re.search(regexp, entry.narration))]


class TestUnrealized(unittest.TestCase):

    def test_empty_entries(self):
        entries, _ = unrealized.add_unrealized_gains([], options.OPTIONS_DEFAULTS.copy())
        self.assertEqual([], entries)

    @loader.load_doc()
    def test_nothing_held_at_cost(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc           -1000 USD
          Assets:Account1

        2014-01-16 *
          Income:Misc           -1000 EUR
          Assets:Account2

        2014-02-01 price EUR  1.34 USD
        """
        new_entries, _ = unrealized.add_unrealized_gains(entries, options_map)
        self.assertEqual(new_entries, entries)
        self.assertEqual([],
                         unrealized.get_unrealized_entries(new_entries))

    @loader.load_doc()
    def test_normal_case(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc           -1000 USD
          Assets:Account1       10 HOUSE {100 USD}

        2014-01-16 *
          Income:Misc           -600 USD
          Assets:Account1       5 HOUSE {120 USD}

        2014-01-17 *
          Income:Misc           -1000 EUR
          Assets:Account2       5 MANSION {200 EUR}

        2014-01-18 * "Bought through a price conversion, not held at cost."
          Income:Misc           -1500 EUR
          Assets:Account2       5 HOTEL @ 300 EUR

        2014-02-01 price HOUSE    130 USD
        2014-02-01 price MANSION  180 EUR
        2014-02-01 price HOTEL    330 USD
        """
        new_entries, _ = unrealized.add_unrealized_gains(entries, options_map)

        self.assertEqual(2, len(unrealized.get_unrealized_entries(new_entries)))

        house = get_entries_with_narration(new_entries, "units of HOUSE")[0]
        self.assertEqual(2, len(house.postings))
        self.assertEqual(D('350'), house.postings[0].units.number)
        self.assertEqual('Assets:Account1', house.postings[0].account)
        self.assertEqual('Income:Account1', house.postings[1].account)

        mansion = get_entries_with_narration(new_entries, "units of MANSION")[0]
        self.assertEqual(2, len(mansion.postings))
        self.assertEqual(D('-100'), mansion.postings[0].units.number)

    @loader.load_doc()
    def test_no_price(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc           -1000 USD
          Assets:Account1       10 HOUSE {100 USD}

        2014-01-15 price HOUSE  100 USD
        """
        # Well... if there is a cost, there is at least one price, derived from
        # the cost entry. This should always work.
        new_entries, _ = unrealized.add_unrealized_gains(entries, options_map)
        unreal_entries = unrealized.get_unrealized_entries(new_entries)
        self.assertEqual(1, len(unreal_entries))
        self.assertEqual(ZERO, unreal_entries[0].postings[0].units.number)

    @loader.load_doc()
    def test_immediate_profit(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc           -1000 USD
          Assets:Account1       10 HOUSE {100 USD} @ 120 USD

        2014-01-15 price HOUSE  120 USD
        """
        # Well... if there is a cost, there is at least one price, derived from
        # the cost entry.
        new_entries, _ = unrealized.add_unrealized_gains(entries, options_map)
        unreal_entries = unrealized.get_unrealized_entries(new_entries)
        self.assertEqual(1, len(unreal_entries))
        self.assertEqual(D('200'),
                         unreal_entries[0].postings[0].units.number)

    @loader.load_doc()
    def test_conversions_only(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc           -780 USD
          Assets:Account1       600 EUR @ 1.3 USD
        """
        # Check to make sure values not held at cost are not included.
        new_entries, _ = unrealized.add_unrealized_gains(entries, options_map)
        self.assertEqual([], unrealized.get_unrealized_entries(new_entries))

    @loader.load_doc()
    def test_with_subaccount(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc
          Assets:Account1       10 HOUSE {100 USD}

        2014-01-15 price HOUSE  100 USD
        """
        entries, errors = unrealized.add_unrealized_gains(entries, options_map, '_invalid_')
        self.assertEqual([unrealized.UnrealizedError], list(map(type, errors)))

        new_entries, _ = unrealized.add_unrealized_gains(entries, options_map, 'Gains')
        entries = unrealized.get_unrealized_entries(new_entries)
        entry = entries[0]
        self.assertEqual('Assets:Account1:Gains', entry.postings[0].account)
        self.assertEqual('Income:Account1:Gains', entry.postings[1].account)

    @loader.load_doc()
    def test_not_assets(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Liabilities:Account1
        2014-01-01 open Equity:Account1
        2014-01-01 open Expenses:Account1
        2014-01-01 open Income:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc
          Assets:Account1      1 HOUSE {100 USD}
          Liabilities:Account1 2 HOUSE {101 USD}
          Equity:Account1      3 HOUSE {102 USD}
          Expenses:Account1    4 HOUSE {103 USD}
          Income:Account1      5 HOUSE {104 USD}

        2014-01-16 price HOUSE 110 USD
        """
        new_entries, _ = unrealized.add_unrealized_gains(entries, options_map, 'Gains')
        unreal_entries = unrealized.get_unrealized_entries(new_entries)

        entry = get_entries_with_narration(unreal_entries, '1 units')[0]
        self.assertEqual("Assets:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(D("10.00"), entry.postings[0].units.number)
        self.assertEqual(D("-10.00"), entry.postings[1].units.number)

        entry = get_entries_with_narration(unreal_entries, '2 units')[0]
        self.assertEqual("Liabilities:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(D("18.00"), entry.postings[0].units.number)
        self.assertEqual(D("-18.00"), entry.postings[1].units.number)

        entry = get_entries_with_narration(unreal_entries, '3 units')[0]
        self.assertEqual("Equity:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(D("24.00"), entry.postings[0].units.number)
        self.assertEqual(D("-24.00"), entry.postings[1].units.number)

        entry = get_entries_with_narration(unreal_entries, '4 units')[0]
        self.assertEqual("Expenses:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(D("28.00"), entry.postings[0].units.number)
        self.assertEqual(D("-28.00"), entry.postings[1].units.number)

        entry = get_entries_with_narration(unreal_entries, '5 units')[0]
        self.assertEqual("Income:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(D("30.00"), entry.postings[0].units.number)
        self.assertEqual(D("-30.00"), entry.postings[1].units.number)

    @loader.load_doc()
    def test_create_open_directive(self, entries, errors, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc
          Assets:Account1      1 HOUSE {100 USD}

        2014-01-16 price HOUSE 110 USD
        """
        # Test the creation of a new, undeclared income account, check that open
        # directives are present for accounts that have been created
        # automatically, because the resulting set of entries should validation
        # no matter what modifications.

        # Test it out without a subaccount, only an open directive should be
        # added for the income account.
        new_entries, errors = unrealized.add_unrealized_gains(entries, options_map)
        self.assertEqual({'Income:Misc',
                          'Assets:Account1',
                          'Income:Account1'},
                         {entry.account for entry in new_entries
                          if isinstance(entry, data.Open)})

        # Test it with a subaccount; we should observe new open directives for
        # th esubaccounts as well.
        new_entries, _ = unrealized.add_unrealized_gains(entries, options_map, 'Gains')

        self.assertEqual({'Income:Misc',
                          'Assets:Account1',
                          'Assets:Account1:Gains',
                          'Income:Account1:Gains'},
                         {entry.account for entry in new_entries
                          if isinstance(entry, data.Open)})

        # Validate the new entries; validation should pass.
        valid_errors = validation.validate(new_entries, options_map)
        self.assertFalse(valid_errors)

    @loader.load_doc()
    def test_no_units_but_leaked_cost_basis(self, entries, errors, options_map):
        """
        ;; This probable mistake triggers an error in the unrealized gains
        ;; calculation. This will occur if you use unstrict booking and leak
        ;; some cost basis, resulting in an holding of zero units but some
        ;; non-zero book value (which should be ignored).

        2009-08-17 open Assets:Cash
        2009-08-17 open Assets:Stocks  "NONE"
        2009-08-17 open Income:Stocks

        2009-08-18 * "Bought titles"
          Assets:Cash      -5000 EUR
          Assets:Stocks     5000 HOOL {1.0 EUR}

        2013-06-19 * "Sold with loss"
          Assets:Stocks    -5000 HOOL {1.1 EUR} ;; Incorrect
          Assets:Cash       3385 EUR
          Income:Stocks
        """
        self.assertFalse(errors)
        new_entries, new_errors = unrealized.add_unrealized_gains(entries, options_map)
        self.assertFalse(new_errors)
