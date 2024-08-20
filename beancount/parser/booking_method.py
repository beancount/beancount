"""Implementations of all the particular booking methods.
This code is used by the full booking algorithm.
"""

__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections
from datetime import timedelta
from decimal import Decimal

from beancount.core.number import ZERO
from beancount.core.data import Booking
from beancount.core.amount import Amount
from beancount.core.position import Cost, Position
from beancount.core import flags
from beancount.core import position
from beancount.core import inventory
from beancount.core import convert


# An error raised if we failed to reduce the inventory balance unambiguously.
AmbiguousMatchError = collections.namedtuple("AmbiguousMatchError", "source message entry")


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
      A triple of
        booked_reductions: A list of matched Posting instances, whose 'cost'
          attributes are ensured to be of type Cost.
        errors: A list of errors to be generated.
        insufficient: A boolean, true if we could not find enough matches to
        cover the entire position.
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
    """Strict booking method. This method fails if there are ambiguous matches."""
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
    """FIFO booking method implementation."""
    return _booking_method_xifo(entry, posting, matches,
                                lambda m: m.cost and getattr(m.cost, "date"),
                                reverse_order=False)


def booking_method_LIFO(entry, posting, matches):
    """LIFO booking method implementation."""
    return _booking_method_xifo(entry, posting, matches,
                                lambda m: m.cost and getattr(m.cost, "date"),
                                reverse_order=True)


def booking_method_HIFO(entry, posting, matches):
    """HIFO booking method implementation."""
    return _booking_method_xifo(entry, posting, matches,
                                lambda m: m.cost and getattr(m.cost, "number"),
                                reverse_order=True)

def booking_method_LowIFO(entry, posting, matches):
    """LowIFO booking method implementation.  Used internally in obscure cases
    for transfers, see LTFO"""
    return _booking_method_xifo(entry, posting, matches,
                                lambda m: m.cost and getattr(m.cost, "number"),
                                reverse_order=False)


def booking_method_LTFO(entry, posting, matches):
    """LTFO (least tax first out, ish) booking method implementation.
    
    This will attempt to minimize the tax liability of the sale by considering
    both gain/loss and the long/short term holding period (including preferring
    losses over gains, i.e. tax loss harvesting).  It will also use some
    heuristics on transfers.  (TODO: What heuristics?)

    This is a bit special cased for crypto.
    """
    # US tax rules
    lt_rate = Decimal("0.2")
    st_rate = Decimal("0.4")
    def lt_thresh(match: Position):
        return match.cost.date.replace(year=match.cost.date.year + 1)

    # If we have a price on the posting, then we can compute the tax liability
    # for each match and select the one with the lowest tax liability.

    if posting.price:
        def us_tax_liability(match: Position):
            """Compute the US tax liability (per unit) of a given match."""
            gain_per_unit = posting.price.number - match.cost.number
            lt_threshold = lt_thresh(match)
            tax_rate = lt_rate if entry.date >= lt_threshold else st_rate
            return gain_per_unit * tax_rate

        return _booking_method_xifo(entry, posting, matches,
                                    us_tax_liability,
                                    reverse_order=False)

    # If there's no price, it's probably a transfer rather than a sale.  The
    # posting we get is the reduction (donating account).
    #
    # If the posting is on a wallet account, it's probably a transfer to a
    # trading account in preparation for sale, so we would want to pick the
    # lot with minimal tax liability.
    #
    # If the posting is on a trading account, then it's probably a transfer to a
    # wallet and we want the inverse -- to stash away the lots least desirable
    # to sell.
    #
    # Now, how do we estimate the tax liability without a price?  Well, if
    # all matches have the same short/long term status, we can just use the
    # cost basis (highest, or lowest, depending on the account).

    sale_prep = "Wallet" in posting.account  # Total hack

    def is_lt(match: Position):
        return entry.date > lt_thresh(match)

    if all(is_lt(m) for m in matches) or all(not is_lt(m) for m in matches):
        if sale_prep:
            return booking_method_HIFO(entry, posting, matches)
        else:
            return booking_method_LowIFO(entry, posting, matches)

    # OK, this is the hard case, it's a transfer, we don't know the price,
    # and we have a mix of long and short term lots.  For now, do the same
    # as when there's no short/long mix.  TODO: be smarter?
    if sale_prep:
        return booking_method_HIFO(entry, posting, matches)
    else:
        return booking_method_LowIFO(entry, posting, matches)

def _booking_method_xifo(entry, posting, matches, key, reverse_order):
    """FIFO and LIFO booking method implementations."""
    booked_reductions = []
    booked_matches = []
    errors = []
    insufficient = False

    # Each up the positions.
    sign = -1 if posting.units.number < ZERO else 1
    remaining = abs(posting.units.number)
    for match in sorted(matches, key=key, reverse=reverse_order):
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
    booked_reductions = []
    booked_matches = []
    errors = [AmbiguousMatchError(entry.meta, "AVERAGE method is not supported", entry)]
    return booked_reductions, booked_matches, errors, False

    # FIXME: Future implementation here.

    if False:
        # DISABLED - This is the code for AVERAGE, which is currently disabled.

        # If there is more than a single match we need to ultimately merge the
        # postings. Also, if the reducing posting provides a specific cost, we
        # need to update the cost basis as well. Both of these cases are carried
        # out by removing all the matches and readding them later on.
        if len(matches) == 1 and (
            not isinstance(posting.cost.number_per, Decimal)
            and not isinstance(posting.cost.number_total, Decimal)
        ):
            # There is no cost. Just reduce the one leg. This should be the
            # normal case if we always merge augmentations and the user lets
            # Beancount deal with the cost.
            match = matches[0]
            sign = -1 if posting.units.number < ZERO else 1
            number = min(abs(match.units.number), abs(posting.units.number))
            match_units = Amount(number * sign, match.units.currency)
            booked_reductions.append(posting._replace(units=match_units, cost=match.cost))
            _insufficient = match_units.number != posting.units.number
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
                        "Cannot merge positions in multiple currencies: {}".format(
                            ", ".join(
                                position.to_string(match_posting)
                                for match_posting in matches
                            )
                        ),
                        entry,
                    )
                )
            else:
                if isinstance(posting.cost.number_per, Decimal) or isinstance(
                    posting.cost.number_total, Decimal
                ):
                    errors.append(
                        AmbiguousMatchError(
                            entry.meta,
                            "Explicit cost reductions aren't supported yet: {}".format(
                                position.to_string(posting)
                            ),
                            entry,
                        )
                    )
                else:
                    # Insert postings to remove all the matches.
                    booked_reductions.extend(
                        posting._replace(
                            units=-match.units, cost=match.cost, flag=flags.FLAG_MERGING
                        )
                        for match in matches
                    )
                    units = merged_units[0].units
                    date = matches[0].cost.date  ## FIXME: Select which one,
                    ## oldest or latest.
                    cost_units = merged_cost[0].units
                    cost = Cost(
                        cost_units.number / units.number, cost_units.currency, date, None
                    )

                    # Insert a posting to refill those with a replacement match.
                    booked_reductions.append(
                        posting._replace(units=units, cost=cost, flag=flags.FLAG_MERGING)
                    )

                    # Now, match the reducing request against this lot.
                    booked_reductions.append(
                        posting._replace(units=posting.units, cost=cost)
                    )
                    _insufficient = abs(posting.units.number) > abs(units.number)

def booking_method_HIFO_XFER_AWARE(entry, posting, matches):
    # If there's no price, it's probably a transfer rather than a sale.  The
    # posting we get is the reduction (donating account).
    #
    # If the posting is on a wallet account, it's probably a transfer to a
    # trading account in preparation for sale, so we would want to pick the
    # lot with minimal tax liability.
    #
    # If the posting is on a trading account, then it's probably a transfer to a
    # wallet and we want the inverse -- to stash away the lots least desirable
    # to sell.
    sale_prep = "Wallet" in posting.account  # Total hack
    if sale_prep:
        return booking_method_HIFO(entry, posting, matches)
    else:
        return booking_method_LowIFO(entry, posting, matches)

_BOOKING_METHODS = {
    Booking.STRICT: booking_method_STRICT,
    Booking.STRICT_WITH_SIZE: booking_method_STRICT_WITH_SIZE,
    Booking.FIFO: booking_method_FIFO,
    Booking.LIFO: booking_method_LIFO,
    Booking.HIFO: booking_method_HIFO,
    Booking.LTFO: booking_method_LTFO,
    Booking.HIFO_XFER_AWARE: booking_method_HIFO_XFER_AWARE,
    Booking.NONE: booking_method_NONE,
    Booking.AVERAGE: booking_method_AVERAGE,
}
