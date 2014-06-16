"""Support utillities for testing scripts.
"""
import unittest
import io

from beancount.parser import parser
from beancount.parser import printer
from beancount.core import compare


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
        if isinstance(expected_entries, str):
            expected_entries, _, __ = parser.parse_string(expected_entries)
        if isinstance(actual_entries, str):
            actual_entries, _, __ = parser.parse_string(actual_entries)
        same, expected_missing, actual_missing = compare.compare_entries(expected_entries,
                                                                         actual_entries)
        if not same:
            assert expected_missing or actual_missing
            oss = io.StringIO()
            if expected_missing:
                oss.write("Missing from from first/expected set:\n\n")
                for entry in expected_missing:
                    oss.write(printer.format_entry(entry))
                    oss.write('\n')
            if actual_missing:
                oss.write("Missing from from actual:\n\n")
                for entry in actual_missing:
                    oss.write(printer.format_entry(entry))
                    oss.write('\n')
            self.fail(oss.getvalue())

    def assertIncludesEntries(self, expected_entries, actual_entries):
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
        if isinstance(expected_entries, str):
            expected_entries, _, __ = parser.parse_string(expected_entries)
        if isinstance(actual_entries, str):
            actual_entries, _, __ = parser.parse_string(actual_entries)
        includes, missing = compare.includes_entries(expected_entries, actual_entries)
        if not includes:
            assert missing
            oss = io.StringIO()
            if missing:
                oss.write("Missing from from first/expected set:\n\n")
                for entry in missing:
                    oss.write(printer.format_entry(entry))
                    oss.write('\n')
            self.fail(oss.getvalue())
