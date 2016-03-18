"""Algorithms for 'booking' inventory, that is, the process of finding a
matching lot when reducing the content of an inventory.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections

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
    booking_methods = {
        'SIMPLE': booking_simple.book,
        'FULL': booking_full.book,
    }
    method_name = options_map['booking_method']
    booking_errors = []
    try:
        booking_fun = booking_methods[method_name]
    except KeyError:
        meta = data.new_metadata(options_map['filename'], 1)
        booking_fun = booking_simple.book
        booking_errors.append(
            BookingError(meta, ("Unsupported booking method: {}; "
                                "falling back on SIMPLE method".format(method_name)), None))

    entries, interpolation_errors = booking_fun(incomplete_entries, options_map)
    validation_errors = validate_inventory_booking(entries, options_map)
    return entries, (booking_errors + interpolation_errors + validation_errors)


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
                position_, _ = running_balance.add_position(posting)

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
