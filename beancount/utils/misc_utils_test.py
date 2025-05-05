"""
Tests for general utils.
"""

__copyright__ = "Copyright (C) 2013-2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import operator
import sys
import time
import unittest
from unittest import mock

from beancount.utils import misc_utils
from beancount.utils import test_utils


def raise_import_error(*args, **kw):
    """Raises an ImportError. This is patched in a test.

    Raises:
      ImportError, unconditionally.
    """
    raise ImportError("Could not import module")


class TestMiscUtils(unittest.TestCase):
    def test_log_time(self):
        with test_utils.capture() as stdout:
            with misc_utils.log_time("test-op", None):
                time.sleep(0.1)
        self.assertEqual("", stdout.getvalue())

        with test_utils.capture() as stdout:
            with misc_utils.log_time("test-op", sys.stdout.write):
                time.sleep(0.1)
        self.assertRegex(stdout.getvalue(), "Operation")
        self.assertRegex(stdout.getvalue(), "Time")

    def test_groupby(self):
        data = [("a", 1), ("b", 2), ("c", 3), ("d", 4)]
        grouped = misc_utils.groupby(lambda x: x[0], data)
        self.assertEqual(set(["a", "b", "c", "d"]), grouped.keys())
        self.assertEqual(
            [[("a", 1)], [("b", 2)], [("c", 3)], [("d", 4)]], sorted(grouped.values())
        )

    def test_filter_type(self):
        class A:
            pass

        class B:
            pass

        class C:
            pass

        data = [x() for x in [A, B, A, A, C, B, C, A]]
        self.assertEqual([A, A, A, A], list(map(type, misc_utils.filter_type(data, A))))

    def test_skipiter(self):
        self.assertEqual([0, 3, 6, 9], list(misc_utils.skipiter(range(10), 3)))

    def test_get_screen_width(self):
        max_width = misc_utils.get_screen_width()
        self.assertTrue(type(int), max_width)
        # Note: Allow zero because the console function fails in test when
        # capture is disabled.
        self.assertLess(-1, max_width)

    @mock.patch("beancount.utils.misc_utils.import_curses", raise_import_error)
    def test_no_curses(self):
        # Make sure the patch works.
        with self.assertRaises(ImportError):
            misc_utils.import_curses()

        # Test functions that would require curses.
        self.assertEqual(0, misc_utils.get_screen_width())
        self.assertEqual(0, misc_utils.get_screen_height())

    def test_get_screen_height(self):
        max_height = misc_utils.get_screen_height()
        self.assertTrue(type(int), max_height)
        # Note: Allow zero because the console function fails in test when
        # capture is disabled.
        self.assertLess(-1, max_height)

    def test_is_sorted(self):
        self.assertTrue(misc_utils.is_sorted([1, 3, 4, 5, 5, 6, 8]))
        self.assertFalse(misc_utils.is_sorted([1, 3, 4, 5, 5, 6, 8], cmp=operator.lt))
        self.assertFalse(misc_utils.is_sorted([3, 2, 6, 7]))
        self.assertTrue(misc_utils.is_sorted([7, 6, 3, 1], cmp=operator.gt))

    def test_escape_string(self):
        # Note that ``escape_string()` only escapes double quotes and backward
        # slashes. Other characters (including non-printable and whitespace
        # characters) are passed through unchanged.
        escaped = misc_utils.escape_string('Entry with escaped "symbols" \\ \r\n')
        self.assertEqual(escaped, 'Entry with escaped \\"symbols\\" \\\\ \r\n')


class TestUniquify(unittest.TestCase):
    def test_sorted_uniquify_first(self):
        data = [
            ("d", 9),
            ("b", 4),
            ("c", 8),
            ("c", 6),
            ("c", 7),
            ("a", 3),
            ("a", 1),
            ("a", 2),
            ("b", 5),
        ]
        unique_data = misc_utils.sorted_uniquify(data, lambda x: x[0], last=False)
        self.assertEqual([("a", 3), ("b", 4), ("c", 8), ("d", 9)], list(unique_data))

    def test_sorted_uniquify_last(self):
        data = [
            ("d", 9),
            ("b", 4),
            ("c", 8),
            ("c", 6),
            ("c", 7),
            ("a", 3),
            ("a", 1),
            ("a", 2),
            ("b", 5),
        ]
        unique_data = misc_utils.sorted_uniquify(data, lambda x: x[0], last=True)
        self.assertEqual([("a", 2), ("b", 5), ("c", 7), ("d", 9)], list(unique_data))

    def test_uniquify_first(self):
        data = [
            ("d", 9),
            ("b", 4),
            ("c", 8),
            ("c", 6),
            ("c", 7),
            ("a", 3),
            ("a", 1),
            ("a", 2),
            ("b", 5),
        ]
        unique_data = misc_utils.uniquify(data, lambda x: x[0], last=False)
        self.assertEqual([("d", 9), ("b", 4), ("c", 8), ("a", 3)], list(unique_data))

    def test_uniquify_last(self):
        data = [
            ("d", 9),
            ("b", 4),
            ("c", 8),
            ("c", 6),
            ("c", 7),
            ("a", 3),
            ("a", 1),
            ("a", 2),
            ("b", 5),
        ]
        unique_data = misc_utils.uniquify(data, lambda x: x[0], last=True)
        self.assertEqual([("d", 9), ("c", 7), ("a", 2), ("b", 5)], list(unique_data))


class TestLineFileProxy(unittest.TestCase):
    def test_line_file_proxy(self):
        output = []
        fileobj = misc_utils.LineFileProxy(output.append, " ")
        fileobj.write("a")
        fileobj.write("b")
        fileobj.write("c\n")
        fileobj.write("d")
        self.assertEqual([" abc"], output)
        fileobj.flush()
        self.assertEqual([" abc"], output)
        fileobj.write("e\n")
        self.assertEqual([" abc", " de"], output)
        fileobj.close()


if __name__ == "__main__":
    unittest.main()
