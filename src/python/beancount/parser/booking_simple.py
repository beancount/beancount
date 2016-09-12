"""Algorithms for 'booking' inventory, that is, the process of finding a
matching lot when reducing the content of an inventory.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections

from beancount.core.data import Transaction
from beancount.core.position import Cost
from beancount.core.amount import Amount
from beancount.core.number import ZERO
from beancount.core.number import MISSING
from beancount.core import interpolate


__sanity_checks__ = False


SimpleBookingError = collections.namedtuple('SimpleBookingError', 'source message entry')


def book(entries, options_map, unused_booking_methods):
    """Run a local interpolation on a list of incomplete entries from the parser.

    Note: this does not take previous positions into account.

    !WARNING!!! This destructively modifies some of the Transaction entries directly.

    Args:
      incomplete_entries: A list of directives, with some postings possibly left
        with incomplete amounts as produced by the parser.
      options_map: An options dict as produced by the parser.
      unused_booking_methods: (Unused.)
    Returns:
      A pair of
        entries: A list of interpolated entries with all their postings completed.
        errors: New errors produced during interpolation.
    """
    # Perform simple booking, that is convert the CostSpec instances to Cost,
    # not even looking at inventory contents.
    entries_with_lots, errors = convert_lot_specs_to_lots(entries)

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
            assert residual.is_small(tolerances,
                                     options_map['inferred_tolerance_default']), (
                                         "Invalid residual {}".format(residual))

    return entries_with_lots, errors


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
        if not isinstance(entry, Transaction):
            new_entries.append(entry)
            continue

        new_postings = []
        for posting in entry.postings:
            try:
                units = posting.units
                cost_spec = posting.cost
                cost = convert_spec_to_cost(units, cost_spec)
                if cost and isinstance(units, Amount):
                    # If there is a cost, we don't allow either a cost value of
                    # zero, nor a zero number of units. Note that we allow a price
                    # of zero as the only special case (for conversion entries), but
                    # never for costs.
                    if units.number == ZERO:
                        raise ValueError('Amount is zero: "{}"'.format(units))
                    if cost.number is not None and cost.number < ZERO:
                        raise ValueError('Cost is negative: "{}"'.format(cost))
            except ValueError as exc:
                errors.append(SimpleBookingError(entry.meta, str(exc), None))
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
    errors = []
    if isinstance(units, Amount):
        currency = units.currency
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
                cost = Cost(unit_cost, cost_currency, date, label)
            else:
                cost = None
    return cost
