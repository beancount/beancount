"""Algorithms for 'booking' inventory, that is, the process of finding a
matching lot when reducing the content of an inventory.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import copy

from beancount.core.amount import Amount
from beancount.core.data import Posting
from beancount.core.data import Transaction
from beancount.core.inventory import Inventory
from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.core.position import Cost
from beancount.core.position import Position
from beancount.core import convert

from beancount.core import account
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

    new_entries = []
    for entry in entries_with_lots:
        if isinstance(entry, Transaction):

            # Check that incompleteness may only occur at the posting level; reject
            # otherwise. These cases are handled by the FULL booking algorithm but
            # not so well by the SIMPLE algorithm. The new parsing is more liberal
            # than this old code can handle, so explicitly reject cases where it
            # would fail.
            skip = False
            for posting in entry.postings:
                units = posting.units
                if units is not MISSING:
                    if units.number is MISSING or units.currency is MISSING:
                        errors.append(SimpleBookingError(
                            entry.meta,
                            "Missing number or currency on units not handled",
                            None))
                        skip = True
                        break

                price = posting.price
                if price is not None:
                    if price.number is MISSING or price.currency is MISSING:
                        errors.append(SimpleBookingError(
                            entry.meta,
                            "Missing number or currency on price not handled",
                            None))
                        skip = True
                        break
            if skip:
                continue

            # Balance incomplete auto-postings and set the parent link to this
            # entry as well.
            balance_errors = balance_incomplete_postings(entry, options_map)
            if balance_errors:
                errors.extend(balance_errors)

            # Check that the balance actually is empty.
            if __sanity_checks__:
                residual = interpolate.compute_residual(entry.postings)
                tolerances = interpolate.infer_tolerances(entry.postings, options_map)
                assert residual.is_small(tolerances), "Invalid residual {}".format(residual)

        new_entries.append(entry)

    return new_entries, errors


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
                if cost_spec is not None and cost is None:
                    errors.append(
                        SimpleBookingError(entry.meta,
                                           "Cost syntax not supported; cost spec ignored",
                                           None))

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


def get_incomplete_postings(entry, options_map):
    """Balance an entry with auto-postings and return an updated list of completed postings.

    Returns a new list of balanced postings, with the incomplete postings
    replaced with completed ones. This is probably the only place where there
    is a bit of non-trivial logic in this entire project (and the rewrite was
    to make sure it was *that* simple.)

    Note that inferred postings are tagged via metatada with an '__automatic__'
    field added to them with a true boolean value.

    Note: The 'postings' parameter may be modified or destroyed for performance
    reasons; don't reuse it.

    Args:
      entry: An instance of a valid directive.
      options_map: A dict of options, as produced by the parser.
    Returns:
      A tuple of:
        postings: a list of new postings to replace the entry's unbalanced
          postings.
        inserted: A boolean set to true if we've inserted new postings.
        errors: A list of balance errors generated during the balancing process.
        residual: A Inventory instance, the residual amounts after balancing
          the postings.
        tolerances: The tolerances inferred in the process, using the postings
          provided.
    """
    # Make a copy of the original list of postings.
    postings = list(entry.postings)

    # Errors during balancing.
    balance_errors = []

    # The list of postings without and with an explicit position.
    auto_postings_indices = []

    # Currencies seen in complete postings.
    currencies = set()

    # An inventory to accumulate the residual balance.
    residual = Inventory()

    # A dict of values for default tolerances.
    tolerances = interpolate.infer_tolerances(postings, options_map)

    # Process all the postings.
    has_nonzero_amount = False
    has_regular_postings = False
    for i, posting in enumerate(postings):
        units = posting.units
        if units is MISSING or units is None:
            # This posting will have to get auto-completed.
            auto_postings_indices.append(i)
        else:
            currencies.add(units.currency)

            # Compute the amount to balance and update the inventory.
            weight = convert.get_weight(posting)
            residual.add_amount(weight)

            has_regular_postings = True
            if weight:
                has_nonzero_amount = True

    # If there are auto-postings, fill them in.
    has_inserted = False
    if auto_postings_indices:
        # If there are too many such postings, we can't do anything, barf.
        if len(auto_postings_indices) > 1:
            balance_errors.append(
                SimpleBookingError(entry.meta,
                                   "Too many auto-postings; cannot fill in",
                                   entry))
            # Delete the redundant auto-postings.
            for index in sorted(auto_postings_indices[1:], reverse=1):
                del postings[index]

        index = auto_postings_indices[0]
        old_posting = postings[index]
        assert old_posting.price is None

        residual_positions = residual.get_positions()

        # If there are no residual positions, we want to still insert a posting
        # but with a zero position for each currency, so that the posting shows
        # up anyhow. We insert one such posting for each currency seen in the
        # complete postings. Note: if all the non-auto postings are zero, we
        # want to avoid sending a warning; the input text clearly implies the
        # author knows this would be useless.
        new_postings = []
        if not residual_positions and ((has_regular_postings and has_nonzero_amount) or
                                       not has_regular_postings):
            balance_errors.append(
                SimpleBookingError(entry.meta,
                                   "Useless auto-posting: {}".format(residual), entry))
            for currency in currencies:
                units = Amount(ZERO, currency)
                meta = copy.copy(old_posting.meta) if old_posting.meta else {}
                meta[interpolate.AUTOMATIC_META] = True
                new_postings.append(
                    Posting(old_posting.account, units, None,
                            None, old_posting.flag, old_posting.meta))
                has_inserted = True
        else:
            # Convert all the residual positions in inventory into a posting for
            # each position.
            for pos in residual_positions:
                pos = -pos

                units = pos.units
                new_units = Amount(
                    interpolate.quantize_with_tolerance(tolerances,
                                                        units.currency,
                                                        units.number),
                    units.currency)

                meta = copy.copy(old_posting.meta) if old_posting.meta else {}
                meta[interpolate.AUTOMATIC_META] = True
                new_postings.append(
                    Posting(old_posting.account, new_units, pos.cost,
                            None, old_posting.flag, meta))
                has_inserted = True

                # Note/FIXME: This is dumb; refactor cost computation so we can
                # reuse it directly.
                new_pos = Position(new_units, pos.cost)

                # Update the residuals inventory.
                weight = convert.get_cost(new_pos)
                residual.add_amount(weight)

        postings[index:index+1] = new_postings

    else:
        # Checking for unbalancing transactions has been moved to the validation
        # stage, so although we already have the input transaction's residuals
        # conveniently precomputed here, we are postponing the check to allow
        # plugins to "fixup" unbalancing transactions. We want to allow users to
        # be able to input unbalancing transactions as long as the final
        # transactions objects that appear on the stream (after processing the
        # plugins) are balanced. See {9e6c14b51a59}.
        pass

    return (postings, has_inserted, balance_errors, residual, tolerances)


def balance_incomplete_postings(entry, options_map):
    """Balance an entry with incomplete postings, modifying the
    empty postings on the entry itself. This sets the parent of
    all the postings to this entry. Futhermore, it stores the dict
    of inferred tolerances as metadata.

    WARNING: This destructively modifies entry itself!

    Args:
      entry: An instance of a valid directive. This entry is modified by
        having new postings inserted to it.
      options_map: A dict of options, as produced by the parser.
    Returns:
      A list of errors, or None, if none occurred.
    """
    # No postings... nothing to do.
    if not entry.postings:
        return None

    # Get the list of corrected postings.
    (postings, unused_inserted, errors,
     residual, tolerances) = get_incomplete_postings(entry, options_map)

    # If we need to accumulate rounding error to accumulate the residual, add
    # suitable postings here.
    if not residual.is_empty():
        rounding_subaccount = options_map["account_rounding"]
        if rounding_subaccount:
            account_rounding = account.join(options_map['name_equity'], rounding_subaccount)
            rounding_postings = interpolate.get_residual_postings(residual,
                                                                  account_rounding)
            postings.extend(rounding_postings)

    # If we could make this faster to avoid the unnecessary copying, it would
    # make parsing substantially faster.
    entry.postings.clear()
    for posting in postings:
        entry.postings.append(posting)

    if entry.meta is None:
        entry.meta = {}
    entry.meta[interpolate.AUTOMATIC_TOLERANCES] = tolerances

    return errors or None
