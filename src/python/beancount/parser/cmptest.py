"""Support utillities for testing scripts.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import io
import textwrap

from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import booking
from beancount.core import compare


class TestError(Exception):
    """Errors within the test implementation itself. These should never occur."""


def read_string_or_entries(entries_or_str):
    """Read a string of entries or just entries.

    Args:
      entries_or_str: Either a list of directives, or a string containing directives.
    Returns:
      A list of directives.
    """
    if isinstance(entries_or_str, str):
        entries, parse_errors, options_map = parser.parse_string(
            textwrap.dedent(entries_or_str))

        # Don't accept incomplete entries either.
        if any(parser.is_entry_incomplete(entry) for entry in entries):
            raise TestError("Entries in assertions may not use interpolation.")

        entries, booking_errors = booking.book(entries, options_map)
        errors = parse_errors + booking_errors

        # Don't tolerate errors.
        if errors:
            oss = io.StringIO()
            printer.print_errors(errors, file=oss)
            raise TestError("Unexpected errors in expected: {}".format(oss.getvalue()))

    else:
        assert isinstance(entries_or_str, list), "Expecting list: {}".format(entries_or_str)
        entries = entries_or_str

    return entries


class TestCase(unittest.TestCase):

    def assertEqualEntries(self, expected_entries, actual_entries):
        """Compare two lists of entries exactly and print missing entries verbosely if
        they occur.

        Args:
          expected_entries: Either a list of directives or a string, in which case the
            string is run through beancount.parser.parse_string() and the resulting
            list is used.
          actual_entries: Same treatment as expected_entries, the other list of
            directives to compare to.
        Raises:
          AssertionError: If the exception fails.
        """
        expected_entries = read_string_or_entries(expected_entries)
        actual_entries = read_string_or_entries(actual_entries)

        same, expected_missing, actual_missing = compare.compare_entries(expected_entries,
                                                                         actual_entries)
        if not same:
            assert expected_missing or actual_missing, "Missing is missing: {}, {}".format(
                expected_missing, actual_missing)
            oss = io.StringIO()
            if expected_missing:
                oss.write("Present in expected set and not in actual set:\n\n")
                for entry in expected_missing:
                    oss.write(printer.format_entry(entry))
                    oss.write('\n')
            if actual_missing:
                oss.write("Present in actual set and not in expected set:\n\n")
                for entry in actual_missing:
                    oss.write(printer.format_entry(entry))
                    oss.write('\n')
            self.fail(oss.getvalue())

    def assertIncludesEntries(self, subset_entries, entries):
        """Check that subset_entries is included in entries and print missing entries.

        Args:
          subset_entries: Either a list of directives or a string, in which case the
            string is run through beancount.parser.parse_string() and the resulting
            list is used.
          entries: Same treatment as subset_entries, the other list of
            directives to compare to.
        Raises:
          AssertionError: If the exception fails.
        """
        subset_entries = read_string_or_entries(subset_entries)
        entries = read_string_or_entries(entries)

        includes, missing = compare.includes_entries(subset_entries, entries)
        if not includes:
            assert missing, "Missing is empty: {}".format(missing)
            oss = io.StringIO()
            if missing:
                oss.write("Missing from from expected set:\n\n")
                for entry in missing:
                    oss.write(printer.format_entry(entry))
                    oss.write('\n')
            self.fail(oss.getvalue())

    def assertExcludesEntries(self, subset_entries, entries):
        """Check that subset_entries is not included in entries and print extra entries.

        Args:
          subset_entries: Either a list of directives or a string, in which case the
            string is run through beancount.parser.parse_string() and the resulting
            list is used.
          entries: Same treatment as subset_entries, the other list of
            directives to compare to.
        Raises:
          AssertionError: If the exception fails.
        """
        subset_entries = read_string_or_entries(subset_entries)
        entries = read_string_or_entries(entries)

        excludes, extra = compare.excludes_entries(subset_entries, entries)
        if not excludes:
            assert extra, "Extra is empty: {}".format(extra)
            oss = io.StringIO()
            if extra:
                oss.write("Extra from from first/excluded set:\n\n")
                for entry in extra:
                    oss.write(printer.format_entry(entry))
                    oss.write('\n')
            self.fail(oss.getvalue())
