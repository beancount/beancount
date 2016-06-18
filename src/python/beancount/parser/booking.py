"""Algorithms for 'booking' inventory, that is, the process of finding a
matching lot when reducing the content of an inventory.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections

from beancount.core.number import MISSING
from beancount.parser import booking_simple
from beancount.parser import booking_full
from beancount.core import data
from beancount.core import inventory


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
    booking_algorithms = {
        'SIMPLE': booking_simple.book,
        'FULL': booking_full.book,
    }
    method_name = options_map['experiment_booking_algorithm']
    errors = []
    try:
        booking_fun = booking_algorithms[method_name]
    except KeyError:
        meta = data.new_metadata(options_map['filename'], 1)
        booking_fun = booking_simple.book
        errors.append(
            BookingError(meta, ("Unsupported booking algorithm: '{}'; "
                                "falling back on SIMPLE method".format(method_name)), None))

    # Get the list of booking methods for each account.
    booking_methods = collections.defaultdict(lambda: options_map["booking_method"])
    for entry in incomplete_entries:
        if isinstance(entry, data.Open) and entry.booking:
            booking_methods[entry.account] = entry.booking

    # Do the booking here!
    entries, booking_errors = booking_fun(incomplete_entries, options_map,
                                          booking_methods)

    if method_name == 'SIMPLE':
        # Check that the inventory reductions are normal-looking.
        validation_errors = validate_inventory_booking(entries, options_map,
                                                       booking_methods)
    else:
        validation_errors = []

    # Check for MISSING elements remaining.
    missing_errors = validate_missing_eliminated(entries, options_map)

    return entries, (errors + booking_errors + validation_errors + missing_errors)


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
                if (MISSING in (units.number, units.currency) or
                    cost is not None and MISSING in (cost.number, cost.currency,
                                                     cost.date, cost.label)):
                    errors.append(
                        BookingError(entry.meta,
                                     "Transaction has incomplete elements",
                                     entry))
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
                            ("Reducing position results in inventory with positive "
                             "and negative lots: {}").format(position_),
                            entry))

    return errors
