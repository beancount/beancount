"""Algorithms for booking inventory.
"""
__author__ = "Martin Blais <blais@furius.ca>"

from beancount.core.data import Transaction
from beancount.core.interpolate import balance_incomplete_postings
from beancount.core.interpolate import compute_residual
from beancount.core.interpolate import infer_tolerances
from beancount.ops import validation  ## FIXME: Pull this in later.


__sanity_checks__ = False


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
    entries, interpolation_errors = simple_interpolation(incomplete_entries, options_map)
    validation_errors = validation.validate_inventory_booking(entries, options_map)
    return entries, (interpolation_errors + validation_errors)


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
            balance_errors = balance_incomplete_postings(entry, options_map)
            if balance_errors:
                errors.extend(balance_errors)

            # Check that the balance actually is empty.
            if __sanity_checks__:
                residual = compute_residual(entry.postings)
                tolerances = infer_tolerances(entry.postings, options_map)
                assert residual.is_small(tolerances, options_map['default_tolerance']), (
                    "Invalid residual {}".format(residual))

    return entries, errors
