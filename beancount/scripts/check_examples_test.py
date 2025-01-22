"""Just make sure that all the example files can be processed without errors."""

__copyright__ = "Copyright (C) 2015-2017, 2019, 2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import os
import unittest
from os import path

from beancount.scripts import check
from beancount.utils import test_utils


def find_example_files():
    """Find a list of example files in out repository.

    Yields:
      A list of strings, absolute paths to example files Beancount provides.
    """
    examples_dir = path.join(test_utils.find_repository_root(__file__), "examples")
    for root, dirs, files in os.walk(examples_dir):
        for filename in files:
            if filename.endswith(".beancount"):
                yield path.join(root, filename)


class TestCheckExamples(test_utils.ClickTestCase):
    def test_example_files(self):
        for filename in find_example_files():
            result = self.run_with_args(check.main, filename)
            self.assertEqual(0, result.exit_code)
            self.assertLines("", result.stdout)


if __name__ == "__main__":
    unittest.main()
