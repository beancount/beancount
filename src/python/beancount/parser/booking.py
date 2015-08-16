"""Algorithms for 'booking' inventory, that is, the process of finding a
matching lot when reducing the content of an inventory.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import os
import logging
import sys

from beancount.parser import grammar
from beancount.core.data import Transaction
from beancount.core.position import Position
from beancount.core.position import Lot
from beancount.core.amount import Amount
from beancount.core.number import ZERO
from beancount.core import interpolate
from beancount.core import data
from beancount.core import inventory


__sanity_checks__ = False


# An error booking a lot reduction to an existing lot.
BookingError = collections.namedtuple('BookingError', 'source message entry')


def book(incomplete_entries, options_map):
    """Book inventory lots and complete all positions with incomplete numbers.

    Args:
      incomplete_entries: A list of directives, with some postings possibly left
        with incomplete amounts as produced by the parser.
      options_map: An options dict as produced by the parser.
    Returns:
      A pair of
        entries: A list of completed entries with all their postings completed.
        errors: New errors produced during interpolation.
    """
    if os.getenv("OLD_BOOKING"):
        # Old-school local-only interpolation overrides the new one for now.
        entries, interpolation_errors = simple_booking(incomplete_entries, options_map)
    else:
        entries, interpolation_errors = full_booking(incomplete_entries, options_map)

    validation_errors = validate_inventory_booking(entries, options_map)
    return entries, (interpolation_errors + validation_errors)


class BookingStats:

    def __init__(self):
        self.num_transactions = 0
        self.num_postings = 0
        self.num_interp_amount = 0
        self.num_interp_units = 0
        self.num_unbooked_lots = 0
        self.num_interp_price = 0

    def __str__(self):
        return '; '.join(["transactions: {s.num_transactions}",
                          "postings: {s.num_postings}",
                          "interp_amount: {s.num_interp_amount}",
                          "interp_units: {s.num_interp_units}",
                          "unbooked_lots: {s.num_unbooked_lots}",
                          "interp_price: {s.num_interp_price}"]).format(s=self)


def full_booking(entries, options_map):
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
    stats = BookingStats()
    errors = []
    for entry in entries:
        if isinstance(entry, Transaction):
            stats.num_transactions += 1
            for posting in entry.postings:
                stats.num_postings += 1
                if posting.position is None:
                    stats.num_interp_amount += 1

                elif posting.position.number is None:
                    stats.num_interp_units += 1

                elif posting.price and (posting.price.number is None or
                                        posting.price.currency is None):
                    stats.num_interp_price += 1

                elif isinstance(posting.position.lot, grammar.LotSpec):
                    stats.num_unbooked_lots += 1

            # if any((posting.position is None or
            #         posting.position.number is None)
            #        for posting in entry.postings):
            #     #printer.print_entry(entry)
            #     num_interpolated += 1

    logging.info("Interpolation Stats: %s", stats)

    return entries, errors


def convert_lot_specs_to_lots(entries, unused_options_map):
    """For all the entries, convert the posting's position's LotSpec to Lot instances.

    This essentially replicates the way the old parser used to work, but
    allowing positions to have the fuzzy lot specifications instead of the
    resolved ones. We used to simply compute the costs locally, and this gets
    rid of the LotSpec to produce the Lot without fuzzy matching. This is only
    there for the sake of transition to the new matching logic.

    Args:
      entries: A list of incomplte directives as per the parser.
      options_map: An options dict from the parser.
    Returns:
      A list of entries whose postings's positions have been converted to Lot
      instances but that may still be incomplete.
    """
    new_entries = []
    errors = []
    for entry in entries:
        if not isinstance(entry, Transaction):
            new_entries.append(entry)
            continue

        new_postings = []
        for posting in entry.postings:
            pos = posting.position
            if pos is not None:
                currency, compound_cost, lot_date, label, merge = pos.lot

                # Compute the cost.
                if compound_cost is not None:
                    if compound_cost.number_total is not None:
                        # Compute the per-unit cost if there is some total cost
                        # component involved.
                        units = pos.number
                        cost_total = compound_cost.number_total
                        if compound_cost.number_per is not None:
                            cost_total += compound_cost.number_per * units
                        unit_cost = cost_total / abs(units)
                    else:
                        unit_cost = compound_cost.number_per
                    cost = Amount(unit_cost, compound_cost.currency)
                else:
                    cost = None

                # If there is a cost, we don't allow either a cost value of
                # zero, nor a zero number of units. Note that we allow a price
                # of zero as the only special case (for conversion entries), but
                # never for costs.
                if cost is not None:
                    if pos.number == ZERO:
                        errors.append(
                            BookingError(entry.meta,
                                         'Amount is zero: "{}"'.format(pos), None))

                    if cost.number is not None and cost.number < ZERO:
                        errors.append(
                            BookingError(entry.meta,
                                         'Cost is negative: "{}"'.format(cost), None))

                lot = Lot(currency, cost, lot_date)
                posting = posting._replace(position=Position(lot, pos.number))

            new_postings.append(posting)
        new_entries.append(entry._replace(postings=new_postings))
    return new_entries, errors


def simple_booking(entries, options_map):
    """Run a local interpolation on a list of incomplete entries from the parser.

    Note: this does not take previous positions into account.

    !WARNING!!! This destructively modifies some of the Transaction entries directly.

    Args:
      incomplete_entries: A list of directives, with some postings possibly left
        with incomplete amounts as produced by the parser.
      options_map: An options dict as produced by the parser.
    Returns:
      A pair of
        entries: A list of interpolated entries with all their postings completed.
        errors: New errors produced during interpolation.
    """
    entries_with_lots, errors = convert_lot_specs_to_lots(entries, options_map)

    for entry in entries_with_lots:
        if not isinstance(entry, Transaction):
            continue
        # Balance incomplete auto-postings and set the parent link to this
        # entry as well.
        balance_errors = interpolate.balance_incomplete_postings(entry, options_map)
        if balance_errors:
            errors.extend(balance_errors)

        # Check that the balance actually is empty.
        if __sanity_checks__:
            residual = interpolate.compute_residual(entry.postings)
            tolerances = interpolate.infer_tolerances(entry.postings, options_map)
            assert residual.is_small(tolerances, options_map['default_tolerance']), (
                "Invalid residual {}".format(residual))

    return entries_with_lots, errors


def validate_inventory_booking(entries, unused_options_map):
    """Validate that no position at cost is allowed to go negative.

    This routine checks that when a posting reduces a position, existing or not,
    that the subsequent inventory does not result in a position with a negative
    number of units. A negative number of units would only be required for short
    trades of trading spreads on futures, and right now this is not supported.
    It would not be difficult to support this, however, but we want to be strict
    about it, because being pedantic about this is otherwise a great way to
    detect user data entry mistakes.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of errors.
    """
    errors = []

    # A mapping of account name to booking method, accumulated in the main loop.
    booking_methods = {}

    balances = collections.defaultdict(inventory.Inventory)
    for entry in entries:
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                # Update the balance of each posting on its respective account
                # without allowing booking to a negative position, and if an error
                # is encountered, catch it and return it.
                running_balance = balances[posting.account]
                position_, _ = running_balance.add_position(posting.position)

                # Skip this check if the booking method is set to ignore it.
                if booking_methods.get(posting.account, None) == 'NONE':
                    continue

                # Check if the resulting inventory is mixed, which is not
                # allowed under the STRICT method.
                if running_balance.is_mixed():
                    errors.append(
                        BookingError(
                            entry.meta,
                            ("Reducing position results in inventory with positive "
                             "and negative lots: {}").format(position_),
                            entry))

        elif isinstance(entry, data.Open):
            # These Open directives should always appear beforehand as per the
            # assumptions on the list of entries, so should never be a problem
            # finding them. If not, move this loop to a dedicated before.
            booking_methods[entry.account] = entry.booking

    return errors
