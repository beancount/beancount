"""Implementations of all the particular booking methods.
This code is used by the full booking algorithm.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core.inventory import Inventory
from beancount.core.number import ZERO
from beancount.core.data import Booking, Posting
from beancount.core.amount import Amount
from beancount.core import flags
from beancount.core import position
from beancount.core import inventory


# An error raised if we failed to reduce the inventory balance unambiguously.
AmbiguousMatchError = collections.namedtuple('AmbiguousMatchError', 'source message entry')


def handle_ambiguous_matches(entry, posting, matches, method):
    """Handle ambiguous matches by dispatching to a particular method.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
        Those positions are known to already match the 'posting' spec.
      methods: A mapping of account name to their corresponding booking
        method.
    Returns:
      A pair of
        booked_reductions: A list of matched Posting instances, whose 'cost'
          attributes are ensured to be of type Cost.
        errors: A list of errors to be generated.
    """
    assert isinstance(method, Booking), (
        "Invalid type: {}".format(method))
    assert matches, "Internal error: Invalid call with no matches"

    #method = globals()['booking_method_{}'.format(method.name)]
    method = _BOOKING_METHODS[method]
    (booked_reductions,
     booked_matches, errors, insufficient) = method(entry, posting, matches)
    if insufficient:
        errors.append(
            AmbiguousMatchError(entry.meta,
                                'Not enough lots to reduce "{}": {}'.format(
                                    position.to_string(posting),
                                    ', '.join(position.to_string(match_posting)
                                              for match_posting in matches)),
                                entry))

    return booked_reductions, booked_matches, errors


def booking_method_STRICT(entry, posting, matches):
    """Strict booking method.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
        Those positions are known to already match the 'posting' spec.
    Returns:
      A triple of
        booked_reductions: A list of matched Posting instances, whose 'cost'
          attributes are ensured to be of type Cost.
        errors: A list of errors to be generated.
        insufficient: A boolean, true if we could not find enough matches
          to fulfill the reduction.
    """
    booked_reductions = []
    booked_matches = []
    errors = []
    insufficient = False
    # In strict mode, we require at most a single matching posting.
    if len(matches) > 1:
        # If the total requested to reduce matches the sum of all the
        # ambiguous postings, match against all of them.
        sum_matches = sum(p.units.number for p in matches)
        if sum_matches == -posting.units.number:
            booked_reductions.extend(
                posting._replace(units=-match.units, cost=match.cost)
                for match in matches)
        else:
            errors.append(
                AmbiguousMatchError(entry.meta,
                                    'Ambiguous matches for "{}": {}'.format(
                                        position.to_string(posting),
                                        ', '.join(position.to_string(match_posting)
                                                  for match_posting in matches)),
                                    entry))
    else:
        # Replace the posting's units and cost values.
        match = matches[0]
        sign = -1 if posting.units.number < ZERO else 1
        number = min(abs(match.units.number), abs(posting.units.number))
        match_units = Amount(number * sign, match.units.currency)
        booked_reductions.append(posting._replace(units=match_units, cost=match.cost))
        booked_matches.append(match)
        insufficient = (match_units.number != posting.units.number)

    return booked_reductions, booked_matches, errors, insufficient


def booking_method_FIFO(entry, posting, matches):
    """FIFO booking method implementation."""
    return _booking_method_xifo(entry, posting, matches, False)


def booking_method_LIFO(entry, posting, matches):
    """LIFO booking method implementation."""
    return _booking_method_xifo(entry, posting, matches, True)


def _booking_method_xifo(entry, posting, matches, reverse_order):
    """FIFO and LIFO booking method implementations."""
    booked_reductions = []
    booked_matches = []
    errors = []
    insufficient = False

    # Each up the positions.
    sign = -1 if posting.units.number < ZERO else 1
    remaining = abs(posting.units.number)
    for match in sorted(matches, key=lambda p: p.cost and p.cost.date,
                        reverse=reverse_order):
        if remaining <= ZERO:
            break

        # If the inventory somehow ended up with mixed lots, skip this one.
        if match.units.number * sign > ZERO:
            continue

        # Compute the amount of units we can reduce from this leg.
        size = min(abs(match.units.number), remaining)
        booked_reductions.append(
            posting._replace(units=Amount(size * sign, match.units.currency),
                             cost=match.cost))
        booked_matches.append(match)
        remaining -= size

    # If we couldn't eat up all the requested reduction, return an error.
    insufficient = (remaining > ZERO)

    return booked_reductions, booked_matches, errors, insufficient


def booking_method_NONE(entry, posting, matches):
    """NONE booking method implementation."""

    # This never needs to match against any existing positions... we
    # disregard the matches, there's never any error. Note that this never
    # gets called in practice, we want to treat NONE postings as
    # augmentations. Default behaviour is to return them with their original
    # CostSpec, and the augmentation code will handle signaling an error if
    # there is insufficient detail to carry out the conversion to an
    # instance of Cost.

    # Note that it's an interesting question whether a reduction on an
    # account with NONE method which happens to match a single position
    # ought to be matched against it. We don't allow it for now.

    return [posting], [], False


def rebook_inventory_at_average_cost(full_inventory: Inventory,
                                     account,
                                     unit_currency,
                                     cost_currency):
    """ Returns a set of postings that rebook an inventory at average cost.

    An inventory can be held in multiple cost currencies, so we treat units
    held in differing cost currencies completely separately.

    Args:
        full_inventory: The ante-inventory that we want to rebook at average cost.
          The inventory passed in will be updated.
        account: The account name this inventory is held for.
        unit_currency: The unit currency (commodity) that we want to rebook our
          holdings for.
        cost_currency: The cost currency to filter the inventory's positions,
          since we treat units held in differing cost currencies completely
          indepdendently.

    Returns: a list of postings that will negate the inventory and rebook it
    as an average position. If the inventory is already a single position for
    the specified cost_currency, an empty list is returned as no rebooking is
    necessary.
    """
    filtered_inv = inventory.Inventory()
    for inv_position in full_inventory:
        if inv_position.units.currency == unit_currency and \
        inv_position.cost.currency == cost_currency:
            filtered_inv.add_position(inv_position)

    if len(filtered_inv) <= 1:
        return []

    booked_postings = []
    avg_position = filtered_inv.average().get_only_position()
    # Negate existing inventory.
    for inv_position in filtered_inv:
        full_inventory.add_amount(-inv_position.units, inv_position.cost)
        booked_postings.append(Posting(
                account=account,
                units=-inv_position.units,
                cost=inv_position.cost,
                price=None,
                flag=flags.FLAG_MERGING,
                meta=None,
            ))
    # Now rebook at average cost.
    full_inventory.add_amount(avg_position.units, avg_position.cost)
    booked_postings.append(
        Posting(
            account=account,
            units=avg_position.units,
            cost=avg_position.cost,
            price=None,
            flag=flags.FLAG_MERGING,
            meta=None,
        )
    )
    return booked_postings



def booking_method_AVERAGE(entry, posting, matches):
    """AVERAGE lot selector.

    Rebook at average already happens before these booking methods are called upon
    to disambiguate multiple potential matching lots. Therefore, matches should always
    be len 1 and we can simply delegate to STRICT booking.

    In the future we could allow the AVERAGE booking method to skip lot matching and receive
    all lots for the matching commodity. That would make our handling of user-specified
    cost basis more user-friendly: we can see the user's intent to specify the expected
    average basis in their reduction and throw an error if it isn't close to our computed
    average. This is similar to what `plugins/check_average_cost.py` does.
    """
    return booking_method_STRICT(entry, posting, matches)


_BOOKING_METHODS = {
    Booking.STRICT : booking_method_STRICT,
    Booking.FIFO   : booking_method_FIFO,
    Booking.LIFO   : booking_method_LIFO,
    Booking.NONE   : booking_method_NONE,
    Booking.AVERAGE: booking_method_AVERAGE,
}
