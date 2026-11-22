"""Implementations of all the particular booking methods.
This code is used by the full booking algorithm.
"""

__copyright__ = "Copyright (C) 2015-2017, 2019-2022, 2024-2026  Martin Blais"
__license__ = "GNU GPLv2"

from decimal import Decimal
from typing import NamedTuple

from beancount.core import convert
from beancount.core import flags
from beancount.core import inventory
from beancount.core import position
from beancount.core.amount import Amount
from beancount.core.data import Booking
from beancount.core.data import Directive
from beancount.core.data import Meta
from beancount.core.number import ZERO
from beancount.core.position import Cost


class AmbiguousMatchError(NamedTuple):
    """An error raised if we failed to reduce the inventory balance unambiguously."""

    source: Meta
    message: str
    entry: Directive


def handle_ambiguous_matches(entry, posting, matches, method):
    """Handle ambiguous matches by dispatching to a particular method.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
        Those positions are known to already match the 'posting' spec.
      method: The booking method to use to disambiguate.
    Returns:
      A triple of
        booked_reductions: A list of matched Posting instances, whose 'cost'
          attributes are ensured to be of type Cost.
        booked_matches: A list of matching positions that were used to reduce.
        errors: A list of errors to be generated.
    """
    assert isinstance(method, Booking), "Invalid type: {}".format(method)
    assert matches, "Internal error: Invalid call with no matches"

    # method = globals()['booking_method_{}'.format(method.name)]
    method = _BOOKING_METHODS[method]
    (booked_reductions, booked_matches, errors, insufficient) = method(
        entry, posting, matches
    )
    if insufficient:
        errors.append(
            AmbiguousMatchError(
                entry.meta,
                'Not enough lots to reduce "{}": {}'.format(
                    position.to_string(posting),
                    ", ".join(
                        position.to_string(match_posting) for match_posting in matches
                    ),
                ),
                entry,
            )
        )

    return booked_reductions, booked_matches, errors


def booking_method_STRICT(entry, posting, matches):
    """Strict booking method. This method fails if there are ambiguous matches.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
    Returns:
      A tuple of (booked_reductions, booked_matches, errors, insufficient).
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
                posting._replace(units=-match.units, cost=match.cost) for match in matches
            )
        else:
            errors.append(
                AmbiguousMatchError(
                    entry.meta,
                    'Ambiguous matches for "{}": {}'.format(
                        position.to_string(posting),
                        ", ".join(
                            position.to_string(match_posting) for match_posting in matches
                        ),
                    ),
                    entry,
                )
            )
    else:
        # Replace the posting's units and cost values.
        match = matches[0]
        sign = -1 if posting.units.number < ZERO else 1
        number = min(abs(match.units.number), abs(posting.units.number))
        match_units = Amount(number * sign, match.units.currency)
        booked_reductions.append(posting._replace(units=match_units, cost=match.cost))
        booked_matches.append(match)
        insufficient = match_units.number != posting.units.number

    return booked_reductions, booked_matches, errors, insufficient


def booking_method_STRICT_WITH_SIZE(entry, posting, matches):
    """Strict booking method, but disambiguate further with sizes.

    This booking method applies the same algorithm as the STRICT method, but if
    only one of the ambiguous lots matches the desired size, select that one
    automatically.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
    Returns:
      A tuple of (booked_reductions, booked_matches, errors, insufficient).
    """
    (booked_reductions, booked_matches, errors, insufficient) = booking_method_STRICT(
        entry, posting, matches
    )

    # If we couldn't match strictly, attempt to find a match with the same
    # number of units. If there is one or more of these, accept the oldest lot.
    if errors and len(matches) > 1:
        number = -posting.units.number
        matching_units = [match for match in matches if number == match.units.number]
        if matching_units:
            matching_units.sort(key=lambda match: match.cost.date)

            # Replace the posting's units and cost values.
            match = matching_units[0]
            booked_reductions.append(posting._replace(units=-match.units, cost=match.cost))
            booked_matches.append(match)
            insufficient = False
            errors = []

    return booked_reductions, booked_matches, errors, insufficient


def booking_method_FIFO(entry, posting, matches):
    """FIFO booking method implementation.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
    Returns:
      A tuple of (booked_reductions, booked_matches, errors, insufficient).
    """
    return _booking_method_xifo(entry, posting, matches, "date", False)


def booking_method_LIFO(entry, posting, matches):
    """LIFO booking method implementation.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
    Returns:
      A tuple of (booked_reductions, booked_matches, errors, insufficient).
    """
    return _booking_method_xifo(entry, posting, matches, "date", True)


def booking_method_HIFO(entry, posting, matches):
    """HIFO booking method implementation.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
    Returns:
      A tuple of (booked_reductions, booked_matches, errors, insufficient).
    """
    return _booking_method_xifo(entry, posting, matches, "number", True)


def booking_method_LOFO(entry, posting, matches):
    """LOFO (Lowest-In First-Out) booking method implementation.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
    Returns:
      A tuple of (booked_reductions, booked_matches, errors, insufficient).
    """
    return _booking_method_xifo(entry, posting, matches, "number", False)


def _booking_method_xifo(entry, posting, matches, sortattr, reverse_order):
    """FIFO, LIFO, HIFO, and LOFO booking method implementations.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
      sortattr: A string, the attribute of Cost to sort by.
      reverse_order: A boolean, whether to reverse the sort order.
    Returns:
      A tuple of (booked_reductions, booked_matches, errors, insufficient).
    """
    booked_reductions = []
    booked_matches = []
    errors = []
    insufficient = False

    # Each up the positions.
    sign = -1 if posting.units.number < ZERO else 1
    remaining = abs(posting.units.number)
    costed_matches = [p for p in matches if p.cost is not None]
    if sortattr == "number":
        cost_currencies = {p.cost.currency for p in costed_matches}
        if len(cost_currencies) > 1:
            errors.append(
                AmbiguousMatchError(
                    entry.meta,
                    "Cannot order positions by cost across multiple cost currencies: {}".format(
                        ", ".join(sorted(cost_currencies))
                    ),
                    entry,
                )
            )
            return booked_reductions, booked_matches, errors, False
    for match in sorted(
        costed_matches,
        key=lambda p: getattr(p.cost, sortattr),
        reverse=reverse_order,
    ):
        if remaining <= ZERO:
            break

        # If the inventory somehow ended up with mixed lots, skip this one.
        if match.units.number * sign > ZERO:
            continue

        # Compute the amount of units we can reduce from this leg.
        size = min(abs(match.units.number), remaining)
        booked_reductions.append(
            posting._replace(
                units=Amount(size * sign, match.units.currency), cost=match.cost
            )
        )
        booked_matches.append(match)
        remaining -= size

    # If we couldn't eat up all the requested reduction, return an error.
    insufficient = remaining > ZERO

    return booked_reductions, booked_matches, errors, insufficient


def booking_method_NONE(entry, posting, matches):
    """NONE booking method implementation.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
    Returns:
      A tuple of (booked_reductions, booked_matches, insufficient).
    """

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

    return [posting], [], [], False


def booking_method_AVERAGE(entry, posting, matches):
    """AVERAGE booking method implementation.

    Merges all matching lots in the same (units.currency, cost.currency) pool
    into a single weighted-average cost lot before applying the reduction.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
    Returns:
      A tuple of (booked_reductions, booked_matches, errors, insufficient).
    """
    booked_reductions = []
    booked_matches = []
    errors = []

    has_explicit_cost = posting.cost is not None and (
        isinstance(posting.cost.number_per, Decimal)
        or isinstance(posting.cost.number_total, Decimal)
    )
    if has_explicit_cost:
        errors.append(
            AmbiguousMatchError(
                entry.meta,
                "Explicit cost reductions aren't supported yet: {}".format(
                    position.to_string(posting)
                ),
                entry,
            )
        )
        return booked_reductions, booked_matches, errors, False

    sign = -1 if posting.units.number < ZERO else 1
    eligible = [
        m for m in matches if m.cost is not None and m.units.number * sign < ZERO
    ]
    if not eligible:
        return booked_reductions, booked_matches, errors, True

    # Single matching lot: reduce directly without emitting synthetic merge postings.
    if len(eligible) == 1:
        match = eligible[0]
        number = min(abs(match.units.number), abs(posting.units.number))
        match_units = Amount(number * sign, match.units.currency)
        booked_reductions.append(posting._replace(units=match_units, cost=match.cost))
        booked_matches.append(match)
        insufficient = match_units.number != posting.units.number
        return booked_reductions, booked_matches, errors, insufficient

    # Multiple matching lots: ensure single unit currency and single cost currency.
    unit_currencies = {m.units.currency for m in eligible}
    cost_currencies = {m.cost.currency for m in eligible}
    if len(unit_currencies) != 1 or len(cost_currencies) != 1:
        errors.append(
            AmbiguousMatchError(
                entry.meta,
                "Cannot merge positions in multiple currencies: {}".format(
                    ", ".join(position.to_string(m) for m in eligible)
                ),
                entry,
            )
        )
        return booked_reductions, booked_matches, errors, False

    total_units = sum((m.units.number for m in eligible), ZERO)
    req_units = abs(posting.units.number)
    avail_units = abs(total_units)

    if req_units > avail_units:
        return booked_reductions, booked_matches, errors, True

    # Full liquidation: retire every lot directly for zero rounding drift.
    if req_units == avail_units:
        for match in eligible:
            booked_reductions.append(
                posting._replace(units=-match.units, cost=match.cost)
            )
            booked_matches.append(match)
        return booked_reductions, booked_matches, errors, False

    # Partial reduction: compute weighted-average cost and merge matching lots.
    total_cost = sum((m.units.number * m.cost.number for m in eligible), ZERO)
    unit_currency = next(iter(unit_currencies))
    cost_currency = next(iter(cost_currencies))
    merge_date = min(m.cost.date for m in eligible)
    avg_cost = Cost(total_cost / total_units, cost_currency, merge_date, None)

    for match in eligible:
        booked_reductions.append(
            posting._replace(
                units=-match.units, cost=match.cost, flag=flags.FLAG_MERGING
            )
        )
        booked_matches.append(match)

    booked_reductions.append(
        posting._replace(
            units=Amount(total_units, unit_currency),
            cost=avg_cost,
            flag=flags.FLAG_MERGING,
        )
    )
    booked_reductions.append(posting._replace(units=posting.units, cost=avg_cost))
    return booked_reductions, booked_matches, errors, False


_BOOKING_METHODS = {
    Booking.STRICT: booking_method_STRICT,
    Booking.STRICT_WITH_SIZE: booking_method_STRICT_WITH_SIZE,
    Booking.FIFO: booking_method_FIFO,
    Booking.LIFO: booking_method_LIFO,
    Booking.HIFO: booking_method_HIFO,
    Booking.LOFO: booking_method_LOFO,
    Booking.NONE: booking_method_NONE,
    Booking.AVERAGE: booking_method_AVERAGE,
}
