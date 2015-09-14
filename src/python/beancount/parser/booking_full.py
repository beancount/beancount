"""Full (new) booking implementation.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import os
import logging
import sys

from beancount.parser import grammar
from beancount.parser import booking_simple
from beancount.core.data import Transaction
from beancount.core.position import Position
from beancount.core.amount import Amount
from beancount.core.number import ZERO
from beancount.core import interpolate
from beancount.core import data
from beancount.core import inventory
from beancount.core import realization


FullBookingError = collections.namedtuple('FullBookingError', 'source message entry')


class BookingStats:

    def __init__(self):
        self.num_transactions = 0
        self.num_postings = 0
        self.num_interp_amount = 0
        self.num_interp_units = 0
        self.num_lots = 0
        self.num_lots_atcost = 0
        self.num_interp_price = 0

    def __str__(self):
        return '; '.join(["transactions: {s.num_transactions}",
                          "postings: {s.num_postings}",
                          "interp_amount: {s.num_interp_amount}",
                          "interp_units: {s.num_interp_units}",
                          "lots: {s.num_lots}",
                          "lots_atcost: {s.num_lots_atcost}",
                          "interp_price: {s.num_interp_price}"]).format(s=self)


def book(entries, options_map):
    """Interpolate missing data from the entries using the full historical algorithm.

    Args:
      incomplete_entries: A list of directives, with some postings possibly left
        with incomplete amounts as produced by the parser.
      options_map: An options dict as produced by the parser.
    Returns:
      A pair of
        entries: A list of interpolated entries with all their postings completed.
        errors: New errors produced during interpolation.
    """
    print()

    balances = collections.defaultdict(inventory.Inventory)
    stats = BookingStats()
    errors = []
    for entry in entries:
        if isinstance(entry, Transaction):
            stats.num_transactions += 1

            # Group postings by currency.
            groups, free_postings = group_postings_by_currency(entry.postings)

            for posting in entry.postings:
                stats.num_postings += 1
                pos = posting.position
                if pos is None:
                    stats.num_interp_amount += 1

                elif pos.units.number is None:
                    stats.num_interp_units += 1

                elif posting.price and (posting.price.number is None or
                                        posting.price.currency is None):
                    stats.num_interp_price += 1

                if 1:
                    stats.num_lots += 1

                    # FIXME: This is broken; rewrite this.

                    # compound_cost = lot.cost
                    # if compound_cost is not None:
                    #     if compound_cost.number_total is not None:
                    #         # Compute the per-unit cost if there is some total cost
                    #         # component involved.
                    #         units = pos.units.number
                    #         cost_total = compound_cost.number_total
                    #         if compound_cost.number_per is not None:
                    #             cost_total += compound_cost.number_per * units
                    #         unit_cost = cost_total / abs(units)
                    #     else:
                    #         unit_cost = compound_cost.number_per
                    #     cost = Amount(unit_cost, compound_cost.currency)

                    #     # print(cost, 'against', balances[posting.account])

                    #     lot = Lot(lot_spec.currency, cost)
                    #     stats.num_lots_atcost += 1
                    #     # print(posting.position)
                    # else:
                    #     # As per the parser, this cannot happen.
                    #     assert (lot_spec.lot_date is None and
                    #             lot_spec.label is None and
                    #             lot_spec.merge is None), "Internal error"

                    #     # This is a simple position.
                    #     lot = Lot(lot_spec.currency, None, None)

                balance = balances[posting.account]
                balance.add_position(posting.position)

    logging.info("Interpolation Stats: %s", stats)

    return entries, errors


def group_postings_by_currency(postings, balances):
    """Group the postings by the currency they declare.

    This is used to prepare the postings for interpolation. Interpolation is
    carried out separately on each currency group. We also have to deal with
    postings that don't have a declared currency.

    Args:
      postings: A list of incomplete postings.
      balanaces: A dict of currency to inventory contents.
    Returns:
      A dict of currency (string) to a list of postings. If there were
      auto-postings - postings without a currency - they were duplicated for
      each group that required them.
    """
    print()

    groups = collections.defaultdict(list)
    unknown = []
    for posting in postings:
        pos = posting.position
        print('=====', pos)

        # If the posting if unspecified, go to the free list.
        if pos is None:
            unknown.append(posting)
            continue

        # Categorize based on the lot spec.
        cost_spec = pos.cost
        if cost_spec is not None:
            if cost_spec.currency:
                groups[cost_spec.currency].append(posting)
            else:
                unknown.append(posting)
        else:
            groups[pos.units.currency].append(posting)

    # FIMXE: Maybe duplicate free postings here for each group so you don't have
    # to return a list of free postings? That would make sense.

    return groups
