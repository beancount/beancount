"""Tests for individual booking methods.

Covers direct calls to booking_method dispatch and edge cases.
"""

__copyright__ = "Copyright (C) 2017, 2019, 2024-2026  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount.core import data
from beancount.core.amount import Amount
from beancount.core.number import D
from beancount.core.position import Cost
from beancount.core.position import Position
from beancount.parser import booking_method as bm


class TestBookingMethods(unittest.TestCase):
    def _make_entry_and_posting(self, units="-5"):
        posting = data.Posting(
            "Assets:Investments",
            Amount(D(units), "HOOL"),
            None,
            None,
            None,
            None,
        )
        entry = data.Transaction(
            data.new_metadata("<test>", 1),
            datetime.date(2024, 1, 10),
            "*",
            None,
            "Sell",
            data.EMPTY_SET,
            data.EMPTY_SET,
            [posting],
        )
        return entry, posting

    def test_booking_method_none_returns_four_tuple(self):
        entry, posting = self._make_entry_and_posting()
        match = Position(
            Amount(D("10"), "HOOL"),
            Cost(D("100"), "USD", datetime.date(2024, 1, 1), None),
        )
        booked_reductions, booked_matches, errors = bm.handle_ambiguous_matches(
            entry, posting, [match], data.Booking.NONE
        )
        self.assertEqual([posting], booked_reductions)
        self.assertEqual([], booked_matches)
        self.assertEqual([], errors)

    def test_xifo_ignores_uncosted_positions(self):
        entry, posting = self._make_entry_and_posting("-5")
        uncosted = Position(Amount(D("3"), "HOOL"), None)
        lot_old = Position(
            Amount(D("5"), "HOOL"),
            Cost(D("100"), "USD", datetime.date(2024, 1, 1), None),
        )
        lot_new = Position(
            Amount(D("5"), "HOOL"),
            Cost(D("120"), "USD", datetime.date(2024, 1, 5), None),
        )
        booked_reductions, booked_matches, errors = bm.handle_ambiguous_matches(
            entry, posting, [uncosted, lot_new, lot_old], data.Booking.FIFO
        )
        self.assertEqual([], errors)
        self.assertEqual([lot_old], booked_matches)
        self.assertEqual(lot_old.cost, booked_reductions[0].cost)

        booked_reductions, booked_matches, errors = bm.handle_ambiguous_matches(
            entry, posting, [uncosted, lot_old, lot_new], data.Booking.HIFO
        )
        self.assertEqual([], errors)
        self.assertEqual([lot_new], booked_matches)
        self.assertEqual(lot_new.cost, booked_reductions[0].cost)


if __name__ == "__main__":
    unittest.main()
