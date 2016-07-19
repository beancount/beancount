__author__ = "Martin Blais <blais@furius.ca>"

import collections
import re
import textwrap

from beancount.parser import parser
from beancount.parser import cmptest
from beancount.parser import booking
from beancount.parser import booking_simple
from beancount.parser import printer
from beancount import loader


class TestInvalidAmountsErrors(cmptest.TestCase):

    @parser.parse_doc()
    def test_zero_amount(self, entries, errors, options_map):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      0 MSFT {200.00 USD}
            Assets:Investments:Cash      0 USD
        """
        booked_entries, booking_errors = booking.book(entries, options_map)
        self.assertEqual(1, len(booking_errors))
        self.assertRegex(booking_errors[0].message, 'Amount is zero')

    @parser.parse_doc()
    def test_cost_zero(self, entries, errors, options_map):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      -10 MSFT {0.00 USD}
            Assets:Investments:Cash  2000.00 USD
        """
        booked_entries, booking_errors = booking.book(entries, options_map)
        self.assertFalse(booking_errors)

    @parser.parse_doc()
    def test_cost_negative(self, entries, errors, options_map):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      -10 MSFT {-200.00 USD}
            Assets:Investments:Cash  2000.00 USD
        """
        booked_entries, booking_errors = booking.book(entries, options_map)
        self.assertEqual(1, len(booking_errors))
        self.assertRegex(booking_errors[0].message, 'Cost is negative')


class TestBookingValidation(cmptest.TestCase):

    def setUp(self):
        self.input_str = textwrap.dedent("""

        2014-01-01 open Assets:Investments:Cash
        2014-01-01 open Assets:Investments:Stock

        2014-06-22 * "Add some positive units"
          Assets:Investments:Stock    1 HOOL {500 USD}
          Assets:Investments:Cash  -500 USD

        2014-06-23 * "Down to zero"
          Assets:Investments:Stock   -1 HOOL {500 USD}
          Assets:Investments:Cash   500 USD

        2014-06-24 * "Go negative from zero"
          Assets:Investments:Stock   -1 HOOL {500 USD}
          Assets:Investments:Cash   500 USD

        2014-06-25 * "Go positive much"
          Assets:Investments:Stock    11 HOOL {500 USD}
          Assets:Investments:Cash  -5500 USD

        2014-06-26 * "Cross to negative from above zero"
          Assets:Investments:Stock  -15 HOOL {500 USD}
          Assets:Investments:Cash  7500 USD

        """)

    BM = collections.defaultdict(lambda: Booking.STRICT)

    def convert_and_validate(self, entries, options_map):
        entries, _ = booking_simple.convert_lot_specs_to_lots(entries)
        return booking.validate_inventory_booking(entries, options_map, self.BM)

    def do_validate_inventory_booking(self, input_str):
        entries, errors, options_map = parser.parse_string(input_str)
        validation_errors = self.convert_and_validate(entries, options_map)
        self.assertEqual([], list(map(type, validation_errors)))

    def test_validate_inventory_booking(self):
        self.do_validate_inventory_booking(self.input_str)

    def test_validate_inventory_booking__same_day(self):
        input_str = re.sub(r'\b2\d\b', '22', self.input_str)
        self.do_validate_inventory_booking(input_str)

    @parser.parse_doc()
    def test_simple_negative_lots(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                -1 HOOL {501 USD}
            Equity:Opening-Balances             501 USD
        """
        validation_errors = self.convert_and_validate(entries, options_map)
        self.assertEqual([], list(map(type, validation_errors)))

    @parser.parse_doc()
    def test_mixed_lots_in_single_transaction(self, entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {501 USD}
            Assets:Bank:Investing                -1 HOOL {502 USD}
            Equity:Opening-Balances           -2003 USD
        """
        validation_errors = self.convert_and_validate(entries, options_map)
        self.assertEqual([booking.BookingError], list(map(type, validation_errors)))

    @parser.parse_doc()
    def test_mixed_lots_in_multiple_transactions_augmenting(self,
                                                            entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {501 USD}
            Equity:Opening-Balances            -501 USD

          2013-05-03 *
            Assets:Bank:Investing                -1 HOOL {502 USD}
            Equity:Opening-Balances             502 USD
        """
        validation_errors = self.convert_and_validate(entries, options_map)
        self.assertEqual([booking.BookingError], list(map(type, validation_errors)))

    @parser.parse_doc()
    def test_mixed_lots_in_multiple_transactions_reducing(self,
                                                          entries, errors, options_map):
        """
          2013-05-01 open Assets:Bank:Investing
          2013-05-01 open Equity:Opening-Balances

          2013-05-02 *
            Assets:Bank:Investing                 5 HOOL {501 USD}
            Assets:Bank:Investing                 5 HOOL {502 USD}
            Equity:Opening-Balances           -5015 USD

          2013-05-03 *
            Assets:Bank:Investing                -6 HOOL {502 USD}
            Equity:Opening-Balances            3012 USD
        """
        validation_errors = self.convert_and_validate(entries, options_map)
        self.assertEqual([booking.BookingError], list(map(type, validation_errors)))


class TestMissingEliminated(cmptest.TestCase):

    @loader.load_doc(expect_errors=True)
    def test_missing_data(self, entries, errors, options_map):
        """
          option "experiment_booking_algorithm" "SIMPLE"

          2013-05-01 open Assets:Test
          2013-05-01 open Expenses:Test

          2016-06-10 * "" ""
            Expenses:Test       10.00
            Assets:Test
        """
        self.assertEqual(1, len(errors))
        self.assertTrue(all(re.search('Transaction has incomplete elements', error.message)
                            for error in errors))
