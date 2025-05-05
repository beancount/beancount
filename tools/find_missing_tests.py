#!/usr/bin/env python3
"""
Find missing test coverage in our source code.

This program find source code and warns us if associated tests are
missing or incomplete. This is used to track progress in test coverage
and to ensure that the entire software suite is covered by appropriate
testing code.
"""

__copyright__ = "Copyright (C) 2014-2017, 2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import os
import re
from os import path


def find_missing_tests(source_dir):
    """Find source files with incomplete tests.

    Args:
      source_dir: A string, the name of the source directory.
    Yields:
      Tuples of source filename, test filename, and an is-missing boolean.
    """
    for root, dirs, files in os.walk(source_dir):
        for relative_filename in files:
            if (
                (not relative_filename.endswith(".py"))
                or relative_filename.endswith("_test.py")
                or relative_filename == "__init__.py"
            ):
                continue

            filename = path.join(root, relative_filename)
            test_filename = re.sub(".py$", "_test.py", filename)
            if not path.exists(test_filename):
                yield (filename, test_filename, True)
            elif not is_complete(test_filename):
                yield (filename, test_filename, False)


def is_complete(filename):
    """A predicate that is true if the given test file is incomplete.

    Args:
      filename: A string, the name of a test file.
    Returns:
      A boolean, true if the tests are complete.
    """
    contents = open(filename, encoding="utf-8").read()
    return not (
        re.search("^__incomplete__", contents, re.M)
        or re.search(r"raise \bNotImplementedError\b", contents, re.M)
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("source_root", action="store")
    opts = parser.parse_args()

    missing_tests = list(find_missing_tests(opts.source_root))
    if missing_tests:
        for filename, test_filename, missing in missing_tests:
            missing_str = "MISSING" if missing else "INCOMPLETE"
            print("Missing Test: {:60} {}".format(filename, missing_str))


if __name__ == "__main__":
    main()
