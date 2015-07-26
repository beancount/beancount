"""Algorithms for 'booking' inventory, that is, the process of finding a
matching lot when reducing the content of an inventory.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import os
import logging

from beancount.core.data import Transaction
from beancount.core import interpolate
from beancount.core import data
from beancount.core import inventory
from beancount.parser import printer



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
    if os.getenv("BEANCOUNT_BOOKING"):
        entries, interpolation_errors = full_interpolation(incomplete_entries, options_map)
    else:
        # Old-school local-only interpolation.
        entries, interpolation_errors = simple_interpolation(incomplete_entries, options_map)

    validation_errors = validate_inventory_booking(entries, options_map)
    return entries, (interpolation_errors + validation_errors)


def full_interpolation(entries, options_map):
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
    num_transactions = 0
    num_interpolated = 0
    errors = []
    for entry in entries:
        if isinstance(entry, Transaction):
            if any((posting.position is None or
                    posting.position.number is None)
                   for posting in entry.postings):
                #printer.print_entry(entry)
                num_interpolated += 1
            num_transactions += 1

        # FIXME: TODO

    logging.info("Num interpolated: %d (%.2f%%)",
                 num_interpolated,
                 num_interpolated/num_transactions * 100)
    return entries, errors


def simple_interpolation(entries, options_map):
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
    errors = []
    for entry in entries:
        if isinstance(entry, Transaction):
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

    return entries, errors


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
