__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core.number import D
from beancount.core import data
from beancount.parser import cmptest
from beancount.plugins import implicit_prices
from beancount import loader


class TestImplicitPrices(cmptest.TestCase):

    @loader.load_doc()
    def test_add_implicit_prices__all_cases(self, entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        ;; An explicit price directive.
        2013-02-01 price  USD  1.10 CAD

        2013-04-01 * "A transaction with a price conversion."
          Assets:Account1             150 USD @ 1.12 CAD
          Assets:Other

        ;; This should book at price at the cost.
        2013-04-02 * "A transaction with a cost."
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other

        ;; This one should be IGNORED because it books against the above.
        2013-04-03 * "A transaction with a cost that reduces an existing position"
          Assets:Account1            -500 HOOL {520 USD}
          Assets:Other

        ;; This one should generate the price, even if it is reducing.
        2013-04-04 * "A transaction with a cost that reduces existing position, with price"
          Assets:Account1            -100 HOOL {520 USD} @ 530 USD
          Assets:Other

        ;; This is not reducing and should also book a price at cost.
        2013-04-05 * "A transaction with another cost that is not reducing."
          Assets:Account1             500 HOOL {540 USD}
          Assets:Other

        ;; The price here overrides the cost and should create an entry.
        2013-04-06 * "A transaction with a cost and a price."
          Assets:Account1             500 HOOL {540 USD} @ 560 USD
          Assets:Other
        """
        self.assertEqual(10, len(entries))
        new_entries, _ = implicit_prices.add_implicit_prices(entries, options_map)
        price_entries = list(filter(lambda entry: isinstance(entry, data.Price),
                                    new_entries))

        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-02-01 price USD 1.10 CAD

        2013-04-01 * "A transaction with a price conversion."
          Assets:Account1                        150 USD                        @ 1.12 CAD
          Assets:Other                          -168.00 CAD

        2013-04-01 price USD 1.12 CAD

        2013-04-02 * "A transaction with a cost."
          Assets:Account1                      1500 HOOL     {520 USD}
          Assets:Other                       -780000 USD

        2013-04-02 price HOOL 520 USD

        2013-04-03 * "A transaction with a cost that reduces an existing position"
          Assets:Account1                      -500 HOOL     {520 USD}
          Assets:Other                        260000 USD

        2013-04-04 * "A transaction with a cost that reduces existing position, with price"
          Assets:Account1                      -100 HOOL     {520 USD}     @ 530 USD
          Assets:Other                         52000 USD

        2013-04-04 price HOOL 530 USD

        2013-04-05 * "A transaction with another cost that is not reducing."
          Assets:Account1                       500 HOOL     {540 USD}
          Assets:Other                       -270000 USD

        2013-04-05 price HOOL 540 USD

        2013-04-06 * "A transaction with a cost and a price."
          Assets:Account1                       500 HOOL     {540 USD}     @ 560 USD
          Assets:Other                       -270000 USD

        2013-04-06 price HOOL 560 USD
        """, new_entries)

        self.assertEqual(6, len(price_entries))
        expected_values = [(x[0], x[1], D(x[2])) for x in [
            ('USD', 'CAD', '1.10'),
            ('USD', 'CAD', '1.12'),
            ('HOOL', 'USD', '520.00'),
            ('HOOL', 'USD', '530.00'),
            ('HOOL', 'USD', '540.00'),
            ('HOOL', 'USD', '560.00')
            ]]
        for expected, price in zip(expected_values, price_entries):
            actual = (price.currency, price.amount.currency, price.amount.number)
            self.assertEqual(expected, actual)

    @loader.load_doc()
    def test_add_implicit_prices__other_account(self, entries, errors, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2  "NONE"
        2013-01-01 open Assets:Other

        2013-04-01 *
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other

        2013-04-02 *
          Assets:Account2             1500 HOOL {530 USD}
          Assets:Other

        2013-04-10 * "Reduces existing position in account 1"
          Assets:Account1            -100 HOOL {520 USD}
          Assets:Other              52000 USD

        2013-04-11 * "Does not find an existing position in account 2"
          Assets:Account2            -200 HOOL {531 USD}
          Assets:Other             106200 USD

        """
        new_entries, _ = implicit_prices.add_implicit_prices(entries, options_map)
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2  "NONE"
        2013-01-01 open Assets:Other

        2013-04-01 *
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other             -780000 USD

        2013-04-02 *
          Assets:Account2             1500 HOOL {530 USD}
          Assets:Other             -795000 USD

        2013-04-01 price HOOL 520 USD

        2013-04-02 price HOOL 530 USD

        2013-04-10 * "Reduces existing position in account 1"
          Assets:Account1                       -100 HOOL {520 USD}
          Assets:Other                         52000 USD

        2013-04-11 * "Does not find an existing position in account 2"
          Assets:Account2                       -200 HOOL {531 USD}
          Assets:Other                        106200 USD

        ;; Because a match was not found against the inventory, a price will be added.
        2013-04-11 price HOOL 531 USD

        """, new_entries)

    @loader.load_doc()
    def test_add_implicit_prices__duplicates_on_same_transaction(self,
                                                                 entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed because of same price"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Account2             1500 HOOL {520 USD}
          Assets:Other

        2013-04-02 * "Second one is disallowed because of different price"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Account2             1500 HOOL {530 USD}
          Assets:Other

        """
        new_entries, errors = implicit_prices.add_implicit_prices(entries, options_map)
        self.assertEqual([], [type(error) for error in errors])
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed because of same price"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Account2             1500 HOOL {520 USD}
          Assets:Other            -1560000 USD

        2013-04-01 price HOOL 520 USD

        2013-04-02 * "Second one is disallowed because of different price"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Account2             1500 HOOL {530 USD}
          Assets:Other            -1575000 USD

        2013-04-02 price HOOL 520 USD
        2013-04-02 price HOOL 530 USD  ;; Allowed for now.

        """, new_entries)

    @loader.load_doc()
    def test_add_implicit_prices__duplicates_on_different_transactions(self,
                                                                       entries, _,
                                                                       options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed because of same price #1"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other

        2013-04-01 * "Allowed because of same price #2"
          Assets:Account2             1500 HOOL {520 USD}
          Assets:Other

        2013-04-02 * "Second one is disallowed because of different price #1"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other

        2013-04-02 * "Second one is disallowed because of different price #2"
          Assets:Account2             1500 HOOL {530 USD}
          Assets:Other

        """
        new_entries, errors = implicit_prices.add_implicit_prices(entries, options_map)
        self.assertEqual([], [type(error) for error in errors])
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed because of same price #1"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other             -780000 USD

        2013-04-01 * "Allowed because of same price #2"
          Assets:Account2             1500 HOOL {520 USD}
          Assets:Other             -780000 USD

        2013-04-01 price HOOL 520 USD

        2013-04-02 * "Second one is disallowed because of different price #1"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other             -780000 USD

        2013-04-02 * "Second one is disallowed because of different price #2"
          Assets:Account2             1500 HOOL {530 USD}
          Assets:Other             -795000 USD

        2013-04-02 price HOOL 520 USD
        2013-04-02 price HOOL 530 USD  ;; Allowed for now.

        """, new_entries)

    @loader.load_doc()
    def test_add_implicit_prices__duplicates_overloaded(self, entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed, sets the price for that day"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other

        2013-04-01 * "Will be ignored, price for the day already set"
          Assets:Account1             1500 HOOL {530 USD}
          Assets:Other

        2013-04-01 * "Should be ignored too, price for the day already set"
          Assets:Account1             1500 HOOL {530 USD}
          Assets:Other
        """
        new_entries, errors = implicit_prices.add_implicit_prices(entries, options_map)
        self.assertEqual([], [type(error) for error in errors])
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed, sets the price for that day"
          Assets:Account1             1500 HOOL {520 USD}
          Assets:Other             -780000 USD

        2013-04-01 * "Will be ignored, price for the day already set"
          Assets:Account1             1500 HOOL {530 USD}
          Assets:Other             -795000 USD

        2013-04-01 * "Should be ignored too, price for the day already set"
          Assets:Account1             1500 HOOL {530 USD}
          Assets:Other             -795000 USD

        2013-04-01 price HOOL 520 USD
        2013-04-01 price HOOL 530 USD

        """, new_entries)


if __name__ == '__main__':
    unittest.main()
