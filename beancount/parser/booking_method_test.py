"""Tests for individual booking methods.

Covers direct calls to booking_method dispatch and edge cases.
"""

__copyright__ = "Copyright (C) 2017, 2019, 2024-2026  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest

from beancount import loader
from beancount.core import data
from beancount.core.amount import Amount
from beancount.core.number import D
from beancount.core.position import Cost
from beancount.core.position import CostSpec
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

    def test_lofo_orders_lowest_cost_first_and_end_to_end(self):
        ledger = """
        2024-01-01 open Assets:Cash
        2024-01-01 open Assets:Investments "LOFO"
        2024-01-01 open Income:Gains

        2024-01-02 * "Buy high"
          Assets:Investments   10 HOOL {120.00 USD}
          Assets:Cash       -1200.00 USD

        2024-01-03 * "Buy low"
          Assets:Investments   10 HOOL {100.00 USD}
          Assets:Cash       -1000.00 USD

        2024-01-10 * "Sell 5"
          Assets:Investments   -5 HOOL {} @ 150.00 USD
          Assets:Cash         750.00 USD
          Income:Gains
        """
        entries, errors, _ = loader.load_string(ledger, dedent=True)
        self.assertEqual([], errors)
        sell_txn = [e for e in entries if isinstance(e, data.Transaction)][-1]
        self.assertEqual(D("100.00"), sell_txn.postings[0].cost.number)
        self.assertEqual(D("-250.00"), sell_txn.postings[2].units.number)

    def test_xifo_rejects_mixed_cost_currencies(self):
        entry, posting = self._make_entry_and_posting("-5")
        lot_usd = Position(
            Amount(D("5"), "HOOL"),
            Cost(D("100"), "USD", datetime.date(2024, 1, 1), None),
        )
        lot_cad = Position(
            Amount(D("5"), "HOOL"),
            Cost(D("120"), "CAD", datetime.date(2024, 1, 2), None),
        )
        for method in (data.Booking.HIFO, data.Booking.LOFO):
            _, _, errors = bm.handle_ambiguous_matches(
                entry, posting, [lot_usd, lot_cad], method
            )
            self.assertEqual(1, len(errors))
            self.assertRegex(errors[0].message, "multiple cost currencies")

    def test_average_full_liquidation_exact_zero_drift(self):
        # 1 share @ 100 + 2 shares @ 101 = 302 total across 3 shares (repeating 100.6666...)
        # Full liquidation retires lots directly without division rounding drift.
        entry, posting = self._make_entry_and_posting("-3")
        lot1 = Position(
            Amount(D("1"), "HOOL"),
            Cost(D("100"), "USD", datetime.date(2024, 1, 1), None),
        )
        lot2 = Position(
            Amount(D("2"), "HOOL"),
            Cost(D("101"), "USD", datetime.date(2024, 1, 2), None),
        )
        booked_reductions, booked_matches, errors = bm.handle_ambiguous_matches(
            entry, posting, [lot1, lot2], data.Booking.AVERAGE
        )
        self.assertEqual([], errors)
        self.assertEqual(2, len(booked_reductions))
        self.assertEqual([lot1, lot2], booked_matches)
        total_weight = sum(p.units.number * p.cost.number for p in booked_reductions)
        self.assertEqual(D("-302"), total_weight)

    def test_average_end_to_end_capital_gains(self):
        ledger = """
        2024-01-01 open Assets:Cash
        2024-01-01 open Assets:Investments "AVERAGE"
        2024-01-01 open Income:Gains

        2024-01-02 * "Buy 1"
          Assets:Investments   60 HOOL {100.00 USD}
          Assets:Cash       -6000.00 USD

        2024-01-03 * "Buy 2"
          Assets:Investments   40 HOOL {110.00 USD}
          Assets:Cash       -4400.00 USD

        2024-01-10 * "Sell 20 at 120"
          Assets:Investments  -20 HOOL {} @ 120.00 USD
          Assets:Cash        2400.00 USD
          Income:Gains
        """
        entries, errors, _ = loader.load_string(ledger, dedent=True)
        self.assertEqual([], errors)
        sell_txn = [e for e in entries if isinstance(e, data.Transaction)][-1]
        gains_posting = [p for p in sell_txn.postings if p.account == "Income:Gains"][0]
        # Average cost = (6000 + 4400) / 100 = 104.00; sold 20 @ 120 => gain = 20 * 16 = 320.00
        self.assertEqual(D("-320.00"), gains_posting.units.number)

    def test_average_explicit_cost_rejected(self):
        entry, posting = self._make_entry_and_posting("-5")
        posting = posting._replace(
            cost=CostSpec(D("100"), None, "USD", None, None, False)
        )
        lot = Position(
            Amount(D("10"), "HOOL"),
            Cost(D("100"), "USD", datetime.date(2024, 1, 1), None),
        )
        _, _, errors = bm.handle_ambiguous_matches(
            entry, posting, [lot], data.Booking.AVERAGE
        )
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Explicit cost reductions")


if __name__ == "__main__":
    unittest.main()
