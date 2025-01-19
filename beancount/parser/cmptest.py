"""Support utilities for testing scripts."""

__copyright__ = "Copyright (C) 2014-2018, 2020-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import io
import textwrap
import unittest

from beancount.core import amount
from beancount.core import compare
from beancount.core import data
from beancount.core import position
from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.parser import booking
from beancount.parser import parser
from beancount.parser import printer


class TestError(Exception):
    """Errors within the test implementation itself. These should never occur."""


def read_string_or_entries(entries_or_str, allow_incomplete=False):
    """Read a string of entries or just entries.

    Args:
      entries_or_str: Either a list of directives, or a string containing directives.
      allow_incomplete: A boolean, true if we allow incomplete inputs and perform
        light-weight booking.
    Returns:
      A list of directives.
    """
    if isinstance(entries_or_str, str):
        entries, errors, options_map = parser.parse_string(textwrap.dedent(entries_or_str))

        if allow_incomplete:
            # Do a simplistic local conversion in order to call the comparison.
            entries = [_local_booking(entry) for entry in entries]
        else:
            # Don't accept incomplete entries either.
            if any(parser.is_entry_incomplete(entry) for entry in entries):
                raise TestError("Entries in assertions may not use interpolation.")

            entries, booking_errors = booking.book(entries, options_map)
            errors = errors + booking_errors

        # Don't tolerate errors.
        if errors:
            oss = io.StringIO()
            printer.print_errors(errors, file=oss)
            raise TestError("Unexpected errors in expected: {}".format(oss.getvalue()))

    else:
        assert isinstance(entries_or_str, list), "Expecting list: {}".format(entries_or_str)
        entries = entries_or_str

    return entries


def _local_booking(entry):
    """Transform incomplete entries as booked.

    This method converts incomplete entries with positions that sport CostSpec
    instances and missing portions to using Cost and replacing MISSING instances
    to None. No booking is carried out on account inventories. The purpose of
    this degenerate booking is simply to transform parsed transactions for the
    purpose of comparison in tests.

    Args:
      entry: An instance of a directive.
    Returns:
      A transformed list of directives.
    """
    if not isinstance(entry, data.Transaction):
        return entry

    new_postings = []
    for posting in entry.postings:
        orig_posting = posting

        # Fixup units.
        if posting.units is MISSING:
            posting = posting._replace(units=None)
        elif posting.units:
            posting = posting._replace(units=_transform_incomplete_amount(posting.units))

        # Fixup cost.
        cost = posting.cost
        if isinstance(cost, position.CostSpec):
            if cost.number_per is MISSING:
                cost = cost._replace(number_per=None)
            if cost.number_total is MISSING:
                cost = cost._replace(number_total=None)
            if cost.currency is MISSING:
                cost = cost._replace(currency=None)
            if cost.date is MISSING:
                cost = cost._replace(date=None)
            if cost.label is MISSING:
                cost = cost._replace(label=None)
            if cost.number_total not in (None, MISSING):
                if not isinstance(posting.units, amount.Amount):
                    raise ValueError(
                        "Cannot convert posting without units: {}".format(orig_posting)
                    )
                number = posting.units.number
                total = (cost.number_per or ZERO) * number + (cost.number_total or ZERO)
                cost_number = (total / number) or None
            else:
                cost_number = None if cost.number_per is MISSING else cost.number_per
            posting = posting._replace(
                cost=position.Cost(cost_number, cost.currency, cost.date, cost.label)
            )
            assert cost.date is not MISSING
            assert cost.label is not MISSING

        # Fixup price.
        if posting.price is MISSING:
            posting = posting._replace(price=None)
        elif posting.price:
            posting = posting._replace(price=_transform_incomplete_amount(posting.price))

        new_postings.append(posting)
    return entry._replace(postings=new_postings)


def _transform_incomplete_amount(amt):
    """Remove MISSING parts from an incomplete amount."""
    if amt.number is MISSING:
        amt = amt._replace(number=None)
    if amt.currency is MISSING:
        amt = amt._replace(currency=None)
    return amt


class TestCase(unittest.TestCase):
    def assertEqualEntries(self, expected_entries, actual_entries, allow_incomplete=False):
        """Check that two lists of entries are equal.

        Entries can be provided either as a list of directives or as a
        string.  In the latter case, the string is parsed with
        beancount.parser.parse_string() and the resulting directives
        list is used. If allow_incomplete is True, light-weight
        booking is performed before comparing the directive lists,
        allowing to compare transactions with incomplete postings.

        Args:
          expected_entries: Expected entries.
          actual_entries: Actual entries.
          allow_incomplete: Perform booking before comparison.

        Raises:
          AssertionError: If the exception fails.

        """
        expected_entries = read_string_or_entries(expected_entries, allow_incomplete)
        actual_entries = read_string_or_entries(actual_entries, allow_incomplete)

        same, expected_missing, actual_missing = compare.compare_entries(
            expected_entries, actual_entries
        )
        if not same:
            assert expected_missing or actual_missing, "Missing is missing: {}, {}".format(
                expected_missing, actual_missing
            )
            oss = io.StringIO()
            if expected_missing:
                oss.write("Present in expected set and not in actual set:\n\n")
                for entry in expected_missing:
                    oss.write(printer.format_entry(entry))
                    oss.write("\n")
            if actual_missing:
                oss.write("Present in actual set and not in expected set:\n\n")
                for entry in actual_missing:
                    oss.write(printer.format_entry(entry))
                    oss.write("\n")
            self.fail(oss.getvalue())

    def assertIncludesEntries(self, subset_entries, entries, allow_incomplete=False):
        """Check that subset_entries is included in entries.

        Entries can be provided either as a list of directives or as a
        string.  In the latter case, the string is parsed with
        beancount.parser.parse_string() and the resulting directives
        list is used. If allow_incomplete is True, light-weight
        booking is performed before comparing the directive lists,
        allowing to compare transactions with incomplete postings.

        Args:
          subset_entries: Subset entries.
          entries: Entries.
          allow_incomplete: Perform booking before comparison.

        Raises:
          AssertionError: If the exception fails.

        """
        subset_entries = read_string_or_entries(subset_entries, allow_incomplete)
        entries = read_string_or_entries(entries)

        includes, missing = compare.includes_entries(subset_entries, entries)
        if not includes:
            assert missing, "Missing is empty: {}".format(missing)
            oss = io.StringIO()
            if missing:
                oss.write("Missing from from expected set:\n\n")
                for entry in missing:
                    oss.write(printer.format_entry(entry))
                    oss.write("\n")
            self.fail(oss.getvalue())

    def assertExcludesEntries(self, subset_entries, entries, allow_incomplete=False):
        """Check that subset_entries is not included in entries.

        Entries can be provided either as a list of directives or as a
        string. In the latter case, the string is parsed with
        beancount.parser.parse_string() and the resulting directives
        list is used. If allow_incomplete is True, light-weight
        booking is performed before comparing the directive lists,
        allowing to compare transactions with incomplete postings.

        Args:
          subset_entries: Subset entries.
          entries: Entries.
          allow_incomplete: Perform booking before comparison.

        Raises:
          AssertionError: If the exception fails.

        """
        subset_entries = read_string_or_entries(subset_entries, allow_incomplete)
        entries = read_string_or_entries(entries)

        excludes, extra = compare.excludes_entries(subset_entries, entries)
        if not excludes:
            assert extra, "Extra is empty: {}".format(extra)
            oss = io.StringIO()
            if extra:
                oss.write("Extra from from first/excluded set:\n\n")
                for entry in extra:
                    oss.write(printer.format_entry(entry))
                    oss.write("\n")
            self.fail(oss.getvalue())
