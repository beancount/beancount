import unittest
import pprint

from beancount.core.amount import to_decimal
from beancount.ops import prices
from beancount.parser import parsedoc


class TestPriceEntries(unittest.TestCase):

    @parsedoc
    def test_get_price_entries(self, entries, _, __):
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
        2013-04-01 * "A transaction with a cost."
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Other

        ;; This one should be IGNORED because it books against the above.
        2013-04-01 * "A transaction with a cost"
          Assets:Account1            -500 GOOG {520 USD}
          Assets:Other

        ;; This is not reducing and should also book a price at cost.
        2013-04-02 * "A transaction with another cost that is not reducing."
          Assets:Account1             500 GOOG {540 USD}
          Assets:Other

        ;; The price here overrides the cost and should create an entry.
        2013-04-03 * "A transaction with a cost and a price."
          Assets:Account1             500 GOOG {540 USD} @ 560 USD
          Assets:Other
        """
        self.assertEqual(9, len(entries))
        price_entries = prices.get_price_entries(entries)
        self.assertEqual(5, len(price_entries))
        expected_values = [(x[0], x[1], to_decimal(x[2])) for x in [
            ('USD', 'CAD', '1.10'),
            ('USD', 'CAD', '1.12'),
            ('GOOG', 'USD', '520.00'),
            ('GOOG', 'USD', '540.00'),
            ('GOOG', 'USD', '560.00')
            ]]
        for expected, price in zip(expected_values, price_entries):
            actual = (price.currency, price.amount.currency, price.amount.number)
            self.assertEqual(expected, actual)


class TestPriceMap(unittest.TestCase):

    @parsedoc
    def test_build_price_map(self, entries, _, __):
        """
        2013-06-01 price  USD  1.10 CAD
        2013-06-02 price  USD  1.11 CAD
        2013-06-02 price  USD  1.12 CAD
        2013-06-03 price  USD  1.13 CAD
        2013-06-03 price  USD  1.14 CAD
        2013-06-03 price  USD  1.15 CAD
        2013-06-04 price  USD  1.16 CAD
        """
        price_map = prices.build_price_map(entries)
        print()
        pprint.pprint(price_map)

    def test_get_all_prices(self):
        pass

    def test_get_latest_price(self):
        pass

__incomplete__ = True
