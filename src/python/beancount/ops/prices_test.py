import unittest
import datetime
import re

from beancount.core.amount import to_decimal as D
from beancount.core import amount
from beancount.core import data
from beancount.ops import prices
from beancount.parser import parsedoc
from beancount.parser import cmptest


class TestPriceEntries(cmptest.TestCase):

    @parsedoc
    def test_add_implicit_prices(self, entries, _, options_map):
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
        2013-04-01 * "A transaction with a cost that reduces an existing position"
          Assets:Account1            -500 GOOG {520 USD}
          Assets:Other

        ;; This one should generate the price, even if it is reducing.
        2013-04-01 * "A transaction with a cost that reduces existing position, with price"
          Assets:Account1            -100 GOOG {520 USD} @ 530 USD
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
        self.assertEqual(10, len(entries))
        new_entries, _ = prices.add_implicit_prices(entries, options_map)
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

        2013-04-01 * "A transaction with a cost."
          Assets:Account1                      1500.00 GOOG     {520.00 USD}
          Assets:Other                       -780000.00 USD

        2013-04-01 price GOOG 520.00 USD

        2013-04-01 * "A transaction with a cost that reduces an existing position"
          Assets:Account1                      -500.00 GOOG     {520.00 USD}
          Assets:Other                        260000.00 USD

        2013-04-01 * "A transaction with a cost that reduces existing position, with price"
          Assets:Account1                      -100.00 GOOG     {520.00 USD}     @ 530.00 USD
          Assets:Other                         52000.00 USD

        2013-04-01 price GOOG 530.00 USD

        2013-04-02 * "A transaction with another cost that is not reducing."
          Assets:Account1                       500.00 GOOG     {540.00 USD}
          Assets:Other                       -270000.00 USD

        2013-04-02 price GOOG 540.00 USD

        2013-04-03 * "A transaction with a cost and a price."
          Assets:Account1                       500.00 GOOG     {540.00 USD}     @ 560.00 USD
          Assets:Other                       -270000.00 USD

        2013-04-03 price GOOG 560.00 USD
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
    def test_add_implicit_prices_other_account(self, entries, _, options_map):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 *
          Assets:Account1             1500 GOOG {520 USD}
          Assets:Account2             1500 GOOG {530 USD}
          Assets:Other

        2013-04-10 * "Reduces existing position in account 1"
          Assets:Account1            -100 GOOG {520 USD}
          Assets:Other

        2013-04-10 * "Does not existing position in account 2"
          Assets:Account2            -200 GOOG {520 USD}
          Assets:Other

        """
        new_entries, _ = prices.add_implicit_prices(entries, options_map)
        self.assertEqualEntries("""

        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Other

        2013-04-01 *
          Assets:Account1                       1500.00 GOOG     {520.00 USD}
          Assets:Account2                       1500.00 GOOG     {530.00 USD}
          Assets:Other                       -1575000.00 USD

        2013-04-01 price GOOG 520.00 USD

        2013-04-01 price GOOG 530.00 USD

        2013-04-10 * "Reduces existing position in account 1"
          Assets:Account1                       -100.00 GOOG     {520.00 USD}
          Assets:Other                          52000.00 USD

        2013-04-10 * "Does not existing position in account 2"
          Assets:Account2                       -200.00 GOOG     {520.00 USD}
          Assets:Other                         104000.00 USD

        2013-04-10 price GOOG 520.00 USD

        """, new_entries)

    @parsedoc
    def test_get_last_price_entries(self, entries, _, __):
        """
        2013-01-01 price  USD  1.01 CAD
        2013-02-01 price  USD  1.02 CAD
        2013-03-01 price  USD  1.03 CAD
        2013-04-01 price  USD  1.04 CAD
        2013-05-01 price  USD  1.05 CAD
        2013-06-01 price  USD  1.06 CAD
        2013-07-01 price  USD  1.07 CAD
        """
        self.assertEqualEntries("""
        2013-04-01 price  USD  1.04 CAD
        """, prices.get_last_price_entries(entries, datetime.date(2013, 5, 1)))

        self.assertEqualEntries("""
        2013-05-01 price  USD  1.05 CAD
        """, prices.get_last_price_entries(entries, datetime.date(2013, 5, 2)))

        self.assertEqualEntries("""
        2013-07-01 price  USD  1.07 CAD
        """, prices.get_last_price_entries(entries, datetime.date(2014, 1, 1)))

        self.assertEqualEntries("""
        """, prices.get_last_price_entries(entries, datetime.date(2012, 1, 1)))


class TestPriceMap(unittest.TestCase):

    def test_normalize_base_quote(self):
        self.assertEqual(('USD', 'CAD'),
                         prices.normalize_base_quote(('USD', 'CAD')))
        self.assertEqual(('USD', 'CAD'),
                         prices.normalize_base_quote(('USD/CAD')))
        with self.assertRaises(AssertionError):
            self.assertEqual(('USD', 'CAD'),
                             prices.normalize_base_quote(('GOOG/USD/CAD')))

    @parsedoc
    def test_build_price_map(self, entries, _, __):
        """
        2013-06-01 price  USD  1.10 CAD

        ;; Try some prices at the same date.
        2013-06-02 price  USD  1.11 CAD
        2013-06-02 price  USD  1.12 CAD
        2013-06-02 price  USD  1.13 CAD

        ;; One after too.
        2013-06-03 price  USD  1.14 CAD

        ;; Try a few inverse prices.
        2013-06-05 price  CAD  0.86956 USD
        2013-06-06 price  CAD  0.86207 USD
        """
        price_map = prices.build_price_map(entries)

        self.assertEqual(2, len(price_map))
        self.assertEqual(set([('USD', 'CAD'), ('CAD', 'USD')]),
                         set(price_map.keys()))

        values = price_map[('USD', 'CAD')]
        expected = [(datetime.date(2013, 6, 1), D('1.10')),
                    (datetime.date(2013, 6, 2), D('1.13')),
                    (datetime.date(2013, 6, 3), D('1.14')),
                    (datetime.date(2013, 6, 5), D('1.15')),
                    (datetime.date(2013, 6, 6), D('1.16'))]
        for (exp_date, exp_value), (act_date, act_value) in zip(expected, values):
            self.assertEqual(exp_date, act_date)
            self.assertEqual(exp_value, act_value.quantize(D('0.01')))

        self.assertEqual(5, len(price_map[('CAD', 'USD')]))

    @parsedoc
    def test_lookup_price_and_inverse(self, entries, _, __):
        """
        2013-06-01 price  USD  1.01 CAD
        """
        price_map = prices.build_price_map(entries)

        # Ensure that the forward exception includes the forward detail.
        try:
            prices._lookup_price_and_inverse(price_map, ('EUR', 'USD'))
            self.fail("Exception not raised.")
        except KeyError as exc:
            self.assertTrue(re.search("('EUR', 'USD')", str(exc)))

    @parsedoc
    def test_get_all_prices(self, entries, _, __):
        """
        2013-06-01 price  USD  1.01 CAD
        2013-06-03 price  USD  1.03 CAD
        2013-06-05 price  USD  1.05 CAD
        2013-06-07 price  USD  1.07 CAD
        2013-06-09 price  USD  1.09 CAD
        2013-06-11 price  USD  1.11 CAD
        """
        price_map = prices.build_price_map(entries)
        price_list = prices.get_all_prices(price_map, ('USD', 'CAD'))
        expected = [(datetime.date(2013, 6, 1), D('1.01')),
                    (datetime.date(2013, 6, 3), D('1.03')),
                    (datetime.date(2013, 6, 5), D('1.05')),
                    (datetime.date(2013, 6, 7), D('1.07')),
                    (datetime.date(2013, 6, 9), D('1.09')),
                    (datetime.date(2013, 6, 11), D('1.11'))]
        self.assertEqual(expected, price_list)

        inv_price_list = prices.get_all_prices(price_map, ('CAD', 'USD'))
        self.assertEqual(len(price_list), len(inv_price_list))

    @parsedoc
    def test_get_latest_price(self, entries, _, __):
        """
        2013-06-01 price  USD  1.01 CAD
        2013-06-09 price  USD  1.09 CAD
        2013-06-11 price  USD  1.11 CAD
        """
        price_map = prices.build_price_map(entries)
        price_list = prices.get_latest_price(price_map, ('USD', 'CAD'))
        expected = (datetime.date(2013, 6, 11), D('1.11'))
        self.assertEqual(expected, price_list)

    @parsedoc
    def test_get_price(self, entries, _, __):
        """
        2013-06-01 price  USD  1.00 CAD
        2013-06-10 price  USD  1.50 CAD
        2013-07-01 price  USD  2.00 CAD
        """
        price_map = prices.build_price_map(entries)

        date, price = prices.get_price(price_map, 'USD/CAD', datetime.date(2013, 5, 15))
        self.assertEqual(None, price)

        date, price = prices.get_price(price_map, 'USD/CAD', datetime.date(2013, 6, 1))
        self.assertEqual(D('1.00'), price)

        date, price = prices.get_price(price_map, 'USD/CAD', datetime.date(2013, 6, 5))
        self.assertEqual(D('1.00'), price)

        date, price = prices.get_price(price_map, 'USD/CAD', datetime.date(2013, 6, 10))
        self.assertEqual(D('1.50'), price)

        date, price = prices.get_price(price_map, 'USD/CAD', datetime.date(2013, 6, 20))
        self.assertEqual(D('1.50'), price)

        date, price = prices.get_price(price_map, 'USD/CAD', datetime.date(2013, 7, 1))
        self.assertEqual(D('2.00'), price)

        date, price = prices.get_price(price_map, 'USD/CAD', datetime.date(2013, 7, 15))
        self.assertEqual(D('2.00'), price)

        # With no date, should devolved to get_latest_price().
        date, price = prices.get_price(price_map, 'USD/CAD', None)
        self.assertEqual(D('2.00'), price)

    @parsedoc
    def test_convert_amount(self, entries, _, __):
        """
        2013-07-01 price  USD  1.20 CAD
        """
        price_map = prices.build_price_map(entries)
        self.assertEqual(amount.Amount('120', 'CAD'),
                         prices.convert_amount(price_map, 'CAD',
                                               amount.Amount('100', 'USD')))
        self.assertEqual(amount.Amount('100', 'CAD'),
                         prices.convert_amount(price_map, 'CAD',
                                               amount.Amount('100', 'CAD')))
        self.assertEqual(None,
                         prices.convert_amount(price_map, 'EUR',
                                               amount.Amount('100', 'USD')))
