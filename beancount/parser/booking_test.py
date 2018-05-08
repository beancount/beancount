__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import re
import textwrap

from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.core.amount import Amount
from beancount.core.data import Booking
from beancount.core.data import Transaction
from beancount.core.position import Cost
from beancount.parser import parser
from beancount.parser import cmptest
from beancount.parser import booking


BookingTestError = collections.namedtuple('BookingTestError', 'source message entry')


def convert_lot_specs_to_lots(entries):
    """For all the entries, convert the posting's position's CostSpec to Cost
    instances. In the simple method, the data provided in the CostSpec must
    unambiguously provide a way to compute the cost amount.

    This essentially replicates the way the old parser used to work, but
    allowing positions to have the fuzzy lot specifications instead of the
    resolved ones. We used to simply compute the costs locally, and this gets
    rid of the CostSpec to produce the Cost without fuzzy matching. This is only
    there for the sake of transition to the new matching logic.

    Args:
      entries: A list of incomplete directives as per the parser.
    Returns:
      A list of entries whose postings's position costs have been converted to
      Cost instances but that may still be incomplete.
    Raises:
      ValueError: If there's a unacceptable number.
    """
    new_entries = []
    errors = []
    for entry in entries:
        if not isinstance(entry, Transaction):
            new_entries.append(entry)
            continue

        new_postings = []
        for posting in entry.postings:
            try:
                units = posting.units
                cost_spec = posting.cost
                cost = convert_spec_to_cost(units, cost_spec)
                if cost_spec is not None and cost is None:
                    errors.append(
                        BookingTestError(entry.meta,
                                         "Cost syntax not supported; cost spec ignored",
                                         None))

                if cost and isinstance(units, Amount):
                    # If there is a cost, we don't allow either a cost value of
                    # zero, nor a zero number of units. Note that we allow a price
                    # of zero as the only special case (for conversion entries), but
                    # never for costs.
                    if units.number == ZERO:
                        raise ValueError('Amount is zero: "{}"'.format(units))
                    if cost.number is not None and cost.number < ZERO:
                        raise ValueError('Cost is negative: "{}"'.format(cost))
            except ValueError as exc:
                errors.append(BookingTestError(entry.meta, str(exc), None))
                cost = None
            new_postings.append(posting._replace(cost=cost))
        new_entries.append(entry._replace(postings=new_postings))
    return new_entries, errors


def convert_spec_to_cost(units, cost_spec):
    """Convert a posting's CostSpec instance to a Cost.

    Args:
      units: An instance of Amount.
      cost_spec: An instance of CostSpec.
    Returns:
      An instance of Cost.
    """
    cost = cost_spec
    errors = []
    if isinstance(units, Amount):
        currency = units.currency
        if cost_spec is not None:
            number_per, number_total, cost_currency, date, label, merge = cost_spec

            # Compute the cost.
            if number_per is not MISSING or number_total is not None:
                if number_total is not None:
                    # Compute the per-unit cost if there is some total cost
                    # component involved.
                    units_num = units.number
                    cost_total = number_total
                    if number_per is not MISSING:
                        cost_total += number_per * units_num
                    unit_cost = cost_total / abs(units_num)
                else:
                    unit_cost = number_per
                cost = Cost(unit_cost, cost_currency, date, label)
            else:
                cost = None
    return cost


class TestInvalidAmountsErrors(cmptest.TestCase):

    @parser.parse_doc()
    def test_zero_amount(self, entries, errors, options_map):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      0 MSFT
            Assets:Investments:Cash      0 USD
        """
        booked_entries, booking_errors = booking.book(entries, options_map)
        self.assertEqual(0, len(booking_errors))

    @parser.parse_doc()
    def test_zero_amount__with_cost(self, entries, errors, options_map):
        """
          2013-05-18 * ""
            Assets:Investments:MSFT      0 MSFT {200.00 USD}
            Assets:Investments:Cash    1 USD
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
        self.assertEqual(1, len(entries))
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

    BOOKMETH = collections.defaultdict(lambda: Booking.STRICT)

    def convert_and_validate(self, entries, options_map):
        entries, _ = convert_lot_specs_to_lots(entries)
        return booking.validate_inventory_booking(entries, options_map, self.BOOKMETH)

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
