import datetime
import unittest
import re
import textwrap

from beancount.core.amount import to_decimal, ZERO
from beancount.core import account_types
from beancount.core import data
from beancount.parser import parsedoc, parse_string
from beancount.parser import printer
from beancount.parser import options
from beancount.ops import unrealized


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
        entries = unrealized.add_unrealized_gains(
            [], account_types.DEFAULT_ACCOUNT_TYPES)
        self.assertEqual([], entries)

    @parsedoc
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
        new_entries = unrealized.add_unrealized_gains(
            entries, options.get_account_types(options_map))
        self.assertEqual(new_entries, entries)
        self.assertEqual([],
                         unrealized.get_unrealized_entries(new_entries))

    @parsedoc
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
        new_entries = unrealized.add_unrealized_gains(
            entries, options.get_account_types(options_map))

        self.assertEqual(2, len(unrealized.get_unrealized_entries(new_entries)))

        house = get_entries_with_narration(new_entries, "units of HOUSE")[0]
        self.assertEqual(2, len(house.postings))
        self.assertEqual(to_decimal('350'), house.postings[0].position.number)
        self.assertEqual('Assets:Account1', house.postings[0].account)
        self.assertEqual('Income:Account1', house.postings[1].account)

        mansion = get_entries_with_narration(new_entries, "units of MANSION")[0]
        self.assertEqual(2, len(mansion.postings))
        self.assertEqual(to_decimal('-100'), mansion.postings[0].position.number)

    @parsedoc
    def test_no_price(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc           -1000 USD
          Assets:Account1       10 HOUSE {100 USD}
        """
        # Well... if there is a cost, there is at least one price, derived from
        # the cost entry. This should always work.
        new_entries = unrealized.add_unrealized_gains(
            entries, options.get_account_types(options_map))
        unreal_entries = unrealized.get_unrealized_entries(new_entries)
        self.assertEqual(1, len(unreal_entries))
        self.assertEqual(ZERO, unreal_entries[0].postings[0].position.number)

    @parsedoc
    def test_immediate_profit(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc           -1000 USD
          Assets:Account1       10 HOUSE {100 USD} @ 120 USD
        """
        # Well... if there is a cost, there is at least one price, derived from
        # the cost entry.
        new_entries = unrealized.add_unrealized_gains(
            entries, options.get_account_types(options_map))
        unreal_entries = unrealized.get_unrealized_entries(new_entries)
        self.assertEqual(1, len(unreal_entries))
        self.assertEqual(to_decimal('200'),
                         unreal_entries[0].postings[0].position.number)


    @parsedoc
    def test_conversions_only(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc           -780
          Assets:Account1       600 EUR @ 1.3 USD
        """
        # Check to make sure values not held at cost are not included.
        new_entries = unrealized.add_unrealized_gains(
            entries, options.get_account_types(options_map))
        self.assertEqual([], unrealized.get_unrealized_entries(new_entries))


    @parsedoc
    def test_with_subaccount(self, entries, _, options_map):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Income:Misc
          Assets:Account1       10 HOUSE {100 USD}
        """
        with self.assertRaises(ValueError):
            unrealized.add_unrealized_gains(
                entries, options.get_account_types(options_map), '_invalid_')

        new_entries = unrealized.add_unrealized_gains(
            entries, options.get_account_types(options_map), 'Gains')
        entry = unrealized.get_unrealized_entries(new_entries)[0]
        self.assertEqual('Assets:Account1:Gains', entry.postings[0].account)
        self.assertEqual('Income:Account1:Gains', entry.postings[1].account)

    @parsedoc
    def test_not_assets(self, entries, _, options_map):
        """
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
        new_entries = unrealized.add_unrealized_gains(entries,
                                                      options.get_account_types(options_map), 'Gains')
        unreal_entries = unrealized.get_unrealized_entries(new_entries)

        entry = get_entries_with_narration(unreal_entries, '1 units')[0]
        self.assertEqual("Assets:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(to_decimal("10.00"), entry.postings[0].position.number)
        self.assertEqual(to_decimal("-10.00"), entry.postings[1].position.number)

        entry = get_entries_with_narration(unreal_entries, '2 units')[0]
        self.assertEqual("Liabilities:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(to_decimal("18.00"), entry.postings[0].position.number)
        self.assertEqual(to_decimal("-18.00"), entry.postings[1].position.number)

        entry = get_entries_with_narration(unreal_entries, '3 units')[0]
        self.assertEqual("Equity:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(to_decimal("24.00"), entry.postings[0].position.number)
        self.assertEqual(to_decimal("-24.00"), entry.postings[1].position.number)

        entry = get_entries_with_narration(unreal_entries, '4 units')[0]
        self.assertEqual("Expenses:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(to_decimal("28.00"), entry.postings[0].position.number)
        self.assertEqual(to_decimal("-28.00"), entry.postings[1].position.number)

        entry = get_entries_with_narration(unreal_entries, '5 units')[0]
        self.assertEqual("Income:Account1:Gains", entry.postings[0].account)
        self.assertEqual("Income:Account1:Gains", entry.postings[1].account)
        self.assertEqual(to_decimal("30.00"), entry.postings[0].position.number)
        self.assertEqual(to_decimal("-30.00"), entry.postings[1].position.number)

    @parsedoc
    def test_create_open_directive(self, entries, _, options_map):
        """
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
        # Test the creation of a new, undeclared income account, check that open
        # directives are present for accounts that have been created
        # automatically, because the resulting set of entries should validation
        # no matter what modifications.


        # new_entries = unrealized.add_unrealized_gains( entries,
        #                                                options.get_account_types(options_map), 'Gains')
        # unreal_entries = unrealized.get_unrealized_entries(new_entries)
        pass



# Create a decorator for each of these methods that automatmically calls the function
__incomplete__ = True
