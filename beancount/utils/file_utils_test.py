"""Tests for file utilities."""

__copyright__ = "Copyright (C) 2014, 2016-2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import os
import tempfile
import unittest
from os import path
from pathlib import Path

from beancount.utils import file_utils
from beancount.utils import test_utils


def clean(prefix, iterable):
    return [Path(f).relative_to(prefix).as_posix() for f in iterable]


class TestFileUtilsFind(test_utils.TestTempdirMixin, test_utils.TestCase):
    def setUp(self):
        super().setUp()
        for filename in [
            "alice/",
            "alice/bottle.txt",
            "rabbit/",
            "rabbit/suit/",
            "rabbit/suit/glasses.txt",
            "caterpillar/",
            "caterpillar/who-are-you.txt",
        ]:
            abs_filename = path.join(self.tempdir, filename)
            if filename.endswith("/"):
                os.makedirs(abs_filename)
            else:
                open(abs_filename, "w", encoding="utf-8").close()

    def test_find_files(self):
        def walk(fords):
            return sorted(clean(self.tempdir, file_utils.find_files(fords)))

        self.assertEqual(
            sorted(
                [
                    "alice/bottle.txt",
                    "rabbit/suit/glasses.txt",
                    "caterpillar/who-are-you.txt",
                ]
            ),
            walk([self.tempdir]),
        )

        self.assertEqual(
            ["alice/bottle.txt"], walk([path.join(self.tempdir, "alice/bottle.txt")])
        )

        self.assertEqual([], walk([path.join(self.tempdir, "alice/blabla.txt")]))

        # Test a string directly.
        self.assertEqual(
            ["alice/bottle.txt"], walk(path.join(self.tempdir, "alice/bottle.txt"))
        )


class TestMiscFileUtils(unittest.TestCase):
    def test_guess_file_format(self):
        self.assertEqual("csv", file_utils.guess_file_format("/user/output.csv"))
        self.assertEqual("text", file_utils.guess_file_format("/user/output.text"))
        self.assertEqual("text", file_utils.guess_file_format("/user/output.txt"))
        self.assertEqual("html", file_utils.guess_file_format("/user/output.html"))
        self.assertEqual("html", file_utils.guess_file_format("/user/output.xhtml"))
        self.assertEqual(None, file_utils.guess_file_format("/user/output"))

    def test_path_greedy_split(self):
        root = tempfile.gettempdir()
        for expected_filename, ext, input_filename in [
            ("1", None, "1"),
            ("2", ".tgz", "2.tgz"),
            ("3", ".tar.gz", "3.tar.gz"),
        ]:
            actual_input = os.path.join(root, input_filename)
            expected = (os.path.join(root, expected_filename), ext)
            actual = file_utils.path_greedy_split(actual_input)
            self.assertEqual(
                expected,
                actual,
                msg=f"failed with {actual_input!r}",
            )


if __name__ == "__main__":
    unittest.main()
