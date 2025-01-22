"""Algorithms for 'booking' inventory, that is, the process of finding a
matching lot when reducing the content of an inventory.
"""

__copyright__ = "Copyright (C) 2015-2018, 2020-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections
from typing import NamedTuple

from beancount.core import amount
from beancount.core import data
from beancount.core import inventory
from beancount.core import position
from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.parser import booking_full


class BookingError(NamedTuple):
    source: data.Meta
    message: str
    entry: data.Transaction


def book(incomplete_entries, options_map, initial_balances=None):
    """Book inventory lots and complete all positions with incomplete numbers.

    Args:
      incomplete_entries: A list of directives, with some postings possibly left
        with incomplete amounts as produced by the parser.
      options_map: An options dict as produced by the parser.
      initial_balances: A dict of (account, inventory) pairs to start booking from.
        This is useful when attempting to book on top of an existing state.
    Returns:
      A pair of
        entries: A list of completed entries with all their postings completed.
        errors: New errors produced during interpolation.
    """
    # Get the list of booking methods for each account.
    booking_methods = collections.defaultdict(lambda: options_map["booking_method"])
    for entry in incomplete_entries:
        if isinstance(entry, data.Open) and entry.booking:
            booking_methods[entry.account] = entry.booking

    # Do the booking here!
    entries, booking_errors = booking_full.book(
        incomplete_entries, options_map, booking_methods, initial_balances
    )

    # Check for MISSING elements remaining.
    missing_errors = validate_missing_eliminated(entries, options_map)

    return entries, (booking_errors + missing_errors)


def validate_missing_eliminated(entries, unused_options_map):
    """Validate that all the missing bits of postings have been eliminated.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of errors.
    """
    errors = []
    for entry in entries:
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                units = posting.units
                cost = posting.cost
                if (
                    MISSING in (units.number, units.currency)
                    or cost is not None
                    and MISSING in (cost.number, cost.currency, cost.date, cost.label)
                ):
                    errors.append(
                        BookingError(
                            entry.meta, "Transaction has incomplete elements", entry
                        )
                    )
                    break
    return errors


# FIXME: This goes away. Maybe moves to a pedantic plugin.
def validate_inventory_booking(entries, unused_options_map, booking_methods):
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
      booking_methods: A mapping of account name to booking method, accumulated
        in the main loop.
    Returns:
      A list of errors.

    """
    errors = []
    balances = collections.defaultdict(inventory.Inventory)
    for entry in entries:
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                # Update the balance of each posting on its respective account
                # without allowing booking to a negative position, and if an error
                # is encountered, catch it and return it.
                running_balance = balances[posting.account]
                position_, _ = running_balance.add_position(posting)

                # Skip this check if the booking method is set to ignore it.
                if booking_methods.get(posting.account, None) == data.Booking.NONE:
                    continue

                # Check if the resulting inventory is mixed, which is not
                # allowed under the STRICT method.
                if running_balance.is_mixed():
                    errors.append(
                        BookingError(
                            entry.meta,
                            (
                                "Reducing position results in inventory with positive "
                                "and negative lots: {}"
                            ).format(position_),
                            entry,
                        )
                    )

    return errors


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
        if not isinstance(entry, data.Transaction):
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
                        BookingError(
                            entry.meta, "Cost syntax not supported; cost spec ignored", None
                        )
                    )

                if cost and isinstance(units, amount.Amount):
                    # If there is a cost, we don't allow either a cost value of
                    # zero, nor a zero number of units. Note that we allow a price
                    # of zero as the only special case (for conversion entries), but
                    # never for costs.
                    if units.number == ZERO:
                        raise ValueError('Amount is zero: "{}"'.format(units))
                    if cost.number is not None and cost.number < ZERO:
                        raise ValueError('Cost is negative: "{}"'.format(cost))
            except ValueError as exc:
                errors.append(BookingError(entry.meta, str(exc), None))
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
    if isinstance(units, amount.Amount):
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
                cost = position.Cost(unit_cost, cost_currency, date, label)
            else:
                cost = None
    return cost
