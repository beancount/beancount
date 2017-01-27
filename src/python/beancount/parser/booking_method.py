"""Implementations of all the particular booking methods.
This code is used by the full booking algorithm.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core.number import ZERO
from beancount.core.number import Decimal
from beancount.core.data import Booking
from beancount.core.amount import Amount
from beancount.core.position import Cost
from beancount.core import flags
from beancount.core import position
from beancount.core import inventory
from beancount.core import convert


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
        booked_postings: A list of matched Posting instances, whose 'cost'
          attributes are ensured to be of type Cost.
        errors: A list of errors to be generated.
    """
    assert isinstance(method, Booking), (
        "Invalid type: {}".format(method))
    assert matches, "Internal error: Invalid call with no matches"

    #method = globals()['booking_method_{}'.format(method.name)]
    method = _BOOKING_METHODS[method]
    postings, errors, insufficient = method(entry, posting, matches)
    if insufficient:
        errors.append(
            AmbiguousMatchError(entry.meta,
                           'Not enough lots to reduce "{}": {}'.format(
                               position.to_string(posting),
                               ', '.join(position.to_string(match_posting)
                                         for match_posting in matches)),
                           entry))

    return postings, errors


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
        booked_postings: A list of matched Posting instances, whose 'cost'
          attributes are ensured to be of type Cost.
        errors: A list of errors to be generated.
        insufficient: A boolean, true if we could not find enough matches
          to fulfill the reduction.
    """
    postings = []
    errors = []
    insufficient = False
    # In strict mode, we require at most a single matching posting.
    if len(matches) > 1:
        # If the total requested to reduce matches the sum of all the
        # ambiguous postings, match against all of them.
        sum_matches = sum(p.units.number for p in matches)
        if sum_matches == -posting.units.number:
            postings.extend(
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
        postings.append(posting._replace(units=match_units, cost=match.cost))
        insufficient = (match_units.number != posting.units.number)

    return postings, errors, insufficient


def booking_method_FIFO(entry, posting, matches):
    """FIFO booking method implementation."""
    return _booking_method_xifo(entry, posting, matches, False)


def booking_method_LIFO(entry, posting, matches):
    """LIFO booking method implementation."""
    return _booking_method_xifo(entry, posting, matches, True)


def _booking_method_xifo(entry, posting, matches, reverse_order):
    """FIFO and LIFO booking method implementations."""
    postings = []
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
        postings.append(
            posting._replace(units=Amount(size * sign, match.units.currency),
                             cost=match.cost))
        remaining -= size

    # If we couldn't eat up all the requested reduction, return an error.
    insufficient = (remaining > ZERO)

    return postings, errors, insufficient


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


def booking_method_AVERAGE(entry, posting, matches):
    """AVERAGE booking method implementation."""
    postings = []
    errors = [AmbiguousMatchError(entry.meta, "AVERAGE method is not supported", entry)]
    return postings, errors, False

    # FIXME: Future implementation here.
    # pylint: disable=unreachable
    if False: # pylint: disable=using-constant-test
        # DISABLED - This is the code for AVERAGE, which is currently disabled.

        # If there is more than a single match we need to ultimately merge the
        # postings. Also, if the reducing posting provides a specific cost, we
        # need to update the cost basis as well. Both of these cases are carried
        # out by removing all the matches and readding them later on.
        if len(matches) == 1 and (
                not isinstance(posting.cost.number_per, Decimal) and
                not isinstance(posting.cost.number_total, Decimal)):
            # There is no cost. Just reduce the one leg. This should be the
            # normal case if we always merge augmentations and the user lets
            # Beancount deal with the cost.
            match = matches[0]
            sign = -1 if posting.units.number < ZERO else 1
            number = min(abs(match.units.number), abs(posting.units.number))
            match_units = Amount(number * sign, match.units.currency)
            postings.append(posting._replace(units=match_units, cost=match.cost))
            insufficient = (match_units.number != posting.units.number)
        else:
            # Merge the matching postings to a single one.
            merged_units = inventory.Inventory()
            merged_cost = inventory.Inventory()
            for match in matches:
                merged_units.add_amount(match.units)
                merged_cost.add_amount(convert.get_weight(match))
            if len(merged_units) != 1 or len(merged_cost) != 1:
                errors.append(
                    AmbiguousMatchError(
                        entry.meta,
                        'Cannot merge positions in multiple currencies: {}'.format(
                            ', '.join(position.to_string(match_posting)
                                      for match_posting in matches)), entry))
            else:
                if (isinstance(posting.cost.number_per, Decimal) or
                    isinstance(posting.cost.number_total, Decimal)):
                    errors.append(
                        AmbiguousMatchError(
                            entry.meta,
                            "Explicit cost reductions aren't supported yet: {}".format(
                                position.to_string(posting)), entry))
                else:
                    # Insert postings to remove all the matches.
                    postings.extend(posting._replace(units=-match.units, cost=match.cost,
                                                     flag=flags.FLAG_MERGING)
                                    for match in matches)
                    units = merged_units[0].units
                    date = matches[0].cost.date  ## FIXME: Select which one,
                                                 ## oldest or latest.
                    cost_units = merged_cost[0].units
                    cost = Cost(cost_units.number/units.number, cost_units.currency,
                                date, None)

                    # Insert a posting to refill those with a replacement match.
                    postings.append(posting._replace(units=units, cost=cost,
                                                     flag=flags.FLAG_MERGING))

                    # Now, match the reducing request against this lot.
                    postings.append(posting._replace(units=posting.units, cost=cost))
                    insufficient = abs(posting.units.number) > abs(units.number)


_BOOKING_METHODS = {
    Booking.STRICT : booking_method_STRICT,
    Booking.FIFO   : booking_method_FIFO,
    Booking.LIFO   : booking_method_LIFO,
    Booking.NONE   : booking_method_NONE,
    Booking.AVERAGE: booking_method_AVERAGE,
}
