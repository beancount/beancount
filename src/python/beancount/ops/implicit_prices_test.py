import unittest
import datetime
import re

from beancount.core.amount import to_decimal as D
from beancount.core import amount
from beancount.core import data
from beancount.ops import implicit_prices
from beancount.parser import parsedoc
from beancount.parser import cmptest
from beancount.parser import printer


class TestImplicitPrices(cmptest.TestCase):

    @parsedoc
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
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        ;; This one should be IGNORED because it books against the above.
        2013-04-03 * "A transaction with a cost that reduces an existing position"
          Assets:Account1            -500 GOOG {520 USD}
          Assets:Other

        ;; This one should generate the price, even if it is reducing.
        2013-04-04 * "A transaction with a cost that reduces existing position, with price"
          Assets:Account1            -100 GOOG {520 USD} @ 530 USD
          Assets:Other

        ;; This is not reducing and should also book a price at cost.
        2013-04-05 * "A transaction with another cost that is not reducing."
          Assets:Account1             500 GOOG {540 USD}
          Assets:Other

        ;; The price here overrides the cost and should create an entry.
        2013-04-06 * "A transaction with a cost and a price."
          Assets:Account1             500 GOOG {540 USD} @ 560 USD
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
          Assets:Account1                        150.00 USD                        @ 1.12 CAD
          Assets:Other                          -168.00 CAD

        2013-04-01 price USD 1.12 CAD

        2013-04-02 * "A transaction with a cost."
          Assets:Account1                      1500.00 GOOG     {520.00 USD}
          Assets:Other                       -780000.00 USD

        2013-04-02 price GOOG 520.00 USD

        2013-04-03 * "A transaction with a cost that reduces an existing position"
          Assets:Account1                      -500.00 GOOG     {520.00 USD}
          Assets:Other                        260000.00 USD

        2013-04-04 * "A transaction with a cost that reduces existing position, with price"
          Assets:Account1                      -100.00 GOOG     {520.00 USD}     @ 530.00 USD
          Assets:Other                         52000.00 USD

        2013-04-04 price GOOG 530.00 USD

        2013-04-05 * "A transaction with another cost that is not reducing."
          Assets:Account1                       500.00 GOOG     {540.00 USD}
          Assets:Other                       -270000.00 USD

        2013-04-05 price GOOG 540.00 USD

        2013-04-06 * "A transaction with a cost and a price."
          Assets:Account1                       500.00 GOOG     {540.00 USD}     @ 560.00 USD
          Assets:Other                       -270000.00 USD

        2013-04-06 price GOOG 560.00 USD
        """, new_entries)

        self.assertEqual(6, len(price_entries))
        expected_values = [(x[0], x[1], D(x[2])) for x in [
            ('USD', 'CAD', '1.10'),
            ('USD', 'CAD', '1.12'),
            ('GOOG', 'USD', '520.00'),
            ('GOOG', 'USD', '530.00'),
            ('GOOG', 'USD', '540.00'),
            ('GOOG', 'USD', '560.00')
            ]]
        for expected, price in zip(expected_values, price_entries):
            actual = (price.currency, price.amount.currency, price.amount.number)
            self.assertEqual(expected, actual)

    @parsedoc
    def test_add_implicit_prices__other_account(self, entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 *
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        2013-04-02 *
          Assets:Account2             1500 GOOG {530 USD}
          Assets:Other

        2013-04-10 * "Reduces existing position in account 1"
          Assets:Account1            -100 GOOG {520 USD}
          Assets:Other

        2013-04-11 * "Does not find an existing position in account 2"
          Assets:Account2            -200 GOOG {520 USD}
          Assets:Other

        """
        new_entries, _ = implicit_prices.add_implicit_prices(entries, options_map)
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 *
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        2013-04-02 *
          Assets:Account2             1500 GOOG {530 USD}
          Assets:Other

        2013-04-01 price GOOG 520.00 USD

        2013-04-02 price GOOG 530.00 USD

        2013-04-10 * "Reduces existing position in account 1"
          Assets:Account1                       -100.00 GOOG     {520.00 USD}
          Assets:Other                          52000.00 USD

        2013-04-11 * "Does not find an existing position in account 2"
          Assets:Account2                       -200.00 GOOG     {520.00 USD}
          Assets:Other                         104000.00 USD

        2013-04-11 price GOOG 520.00 USD

        """, new_entries)

    @parsedoc
    def test_add_implicit_prices__duplicates_on_same_transaction(self, entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed because of same price"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Account2             1500 GOOG {520 USD}
          Assets:Other

        2013-04-02 * "Second one is disallowed because of different price"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Account2             1500 GOOG {530 USD}
          Assets:Other

        """
        new_entries, errors = implicit_prices.add_implicit_prices(entries, options_map)
        self.assertEqual([], [type(error) for error in errors])
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed because of same price"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Account2             1500 GOOG {520 USD}
          Assets:Other

        2013-04-01 price GOOG 520.00 USD

        2013-04-02 * "Second one is disallowed because of different price"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Account2             1500 GOOG {530 USD}
          Assets:Other

        2013-04-02 price GOOG 520.00 USD
        2013-04-02 price GOOG 530.00 USD  ;; Allowed for now.

        """, new_entries)

    @parsedoc
    def test_add_implicit_prices__duplicates_on_different_transactions(self, entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed because of same price #1"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        2013-04-01 * "Allowed because of same price #2"
          Assets:Account2             1500 GOOG {520 USD}
          Assets:Other

        2013-04-02 * "Second one is disallowed because of different price #1"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        2013-04-02 * "Second one is disallowed because of different price #2"
          Assets:Account2             1500 GOOG {530 USD}
          Assets:Other

        """
        new_entries, errors = implicit_prices.add_implicit_prices(entries, options_map)
        self.assertEqual([], [type(error) for error in errors])
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed because of same price #1"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        2013-04-01 * "Allowed because of same price #2"
          Assets:Account2             1500 GOOG {520 USD}
          Assets:Other

        2013-04-01 price GOOG 520.00 USD

        2013-04-02 * "Second one is disallowed because of different price #1"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        2013-04-02 * "Second one is disallowed because of different price #2"
          Assets:Account2             1500 GOOG {530 USD}
          Assets:Other

        2013-04-02 price GOOG 520.00 USD
        2013-04-02 price GOOG 530.00 USD  ;; Allowed for now.

        """, new_entries)

    @parsedoc
    def test_add_implicit_prices__duplicates_overloaded(self, entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed, sets the price for that day"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        2013-04-01 * "Will be ignored, price for the day already set"
          Assets:Account1             1500 GOOG {530 USD}
          Assets:Other

        2013-04-01 * "Should be ignored too, price for the day already set"
          Assets:Account1             1500 GOOG {530 USD}
          Assets:Other
        """
        new_entries, errors = implicit_prices.add_implicit_prices(entries, options_map)
        self.assertEqual([], [type(error) for error in errors])
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Other

        2013-04-01 * "Allowed, sets the price for that day"
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        2013-04-01 * "Will be ignored, price for the day already set"
          Assets:Account1             1500 GOOG {530 USD}
          Assets:Other

        2013-04-01 * "Should be ignored too, price for the day already set"
          Assets:Account1             1500 GOOG {530 USD}
          Assets:Other

        2013-04-01 price GOOG 520.00 USD
        2013-04-01 price GOOG 530.00 USD

        """, new_entries)
