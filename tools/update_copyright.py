#!/usr/bin/env python3
"""Process all source code and update the license and copyright headers to be current.

hg log --template '{date|isodate} {desc}\n' beancount/core/inventory.py
"""

__copyright__ = "Copyright (C) 2016-2017, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import contextlib
import datetime
import logging
import os
import re
import subprocess
import unittest
from os import path

COPYRIGHT = '__copyright__ = "Copyright (C) {years}  Martin Blais"'
LICENSE = '__license__ = "GNU GPLv2"'


def find_files(rootdir, regexp_files, ignore_dirs):
    """Find the files we need to apply this to."""
    for root, dirs, files in os.walk(rootdir):
        with contextlib.suppress(ValueError):
            dirs.remove("build")
        dirs[:] = [dirname for dirname in dirs if not re.match(ignore_dirs, dirname)]
        for filename in files:
            if re.match(regexp_files, filename):
                yield path.join(root, filename)


def find_existing_copyright(lines: list[str]) -> tuple[int | None, int | None, int | None]:
    """Find the line numbers for an existing copyright.

    Returns:
      Three integers, one for the copyright line and one for the license
      line. If the patterns aren't found return None instead of the line number.
    """
    indexes = []
    for pattern in [
        "__copyright__ = .* Martin Blais",
        "__license__ = ",
        "__author__ = ",
    ]:
        for index, line in enumerate(lines):
            if re.match(pattern, line):
                break
        else:
            index = None
        indexes.append(index)
    return tuple(indexes)


def find_start(lines):
    contents = "".join(line + os.linesep for line in lines)
    start = 0
    while True:
        match = re.match(r'(^#[^\n]*|""".*?""".*?)\n', contents[start:], re.DOTALL)
        if match:
            start += match.end()
        else:
            break
    return len(contents[:start].splitlines())


def get_change_years_hg(filename: str, cwd: str) -> list[int]:
    """Find the relevant years where there was a change."""
    log_lines = subprocess.check_output(
        ["hg", "log", "--template", "{date|isodate}\n", filename], cwd=cwd
    )
    log_years = set()
    for line in log_lines.decode("utf8").splitlines():
        log_years.add(int(re.match("([0-9]{4})-", line).group(1)))
    return sorted(log_years)


def get_change_years_git(filename: str, cwd: str) -> list[int]:
    """Extracts the years in which changes were made to a specific file."""
    try:
        # Get the commit history for the file
        git_log_output = subprocess.check_output(
            ["git", "log", "--pretty=format:%ci", "--follow", filename], text=True, cwd=cwd
        )

        # Extract the commit dates from the output
        commit_dates = git_log_output.splitlines()

        # Extract the years from the dates
        years = set()
        for date in commit_dates:
            year = int(date.split()[0].split("-")[0])
            years.add(year)

        return sorted(years)

    except subprocess.CalledProcessError:
        logging.error(f"Error: Unable to get Git log for {filename}")
        return []


def compress_years(years):
    """Compress a list of years into a list of pairs of ints."""
    if not years:
        return []
    intervals = []
    ystack = list(years)
    first = last = ystack.pop(0)
    while ystack:
        new = ystack.pop(0)
        if last + 1 == new:
            last = new
        else:
            intervals.append((first, last))
            first = last = new
    intervals.append((first, last))
    return intervals


class TestCompressYears(unittest.TestCase):
    def test_one_unique(self):
        self.assertEqual([(2015, 2015)], compress_years([2015]))

    def test_one_interval(self):
        self.assertEqual([(2014, 2015)], compress_years([2014, 2015]))

    def test_one_interval_compressed(self):
        self.assertEqual([(2013, 2015)], compress_years([2013, 2014, 2015]))

    def test_two_uniques(self):
        self.assertEqual([(2013, 2013), (2015, 2015)], compress_years([2013, 2015]))

    def test_mixed1(self):
        self.assertEqual(
            [(1998, 2000), (2002, 2002), (2005, 2006), (2008, 2008)],
            compress_years([1998, 1999, 2000, 2002, 2005, 2006, 2008]),
        )


def format_years(year_pairs):
    """Format a list of year-pairs."""
    if not year_pairs:
        return str(datetime.date.today().year)
    return ", ".join(
        str(first) if first == last else "{}-{}".format(first, last)
        for first, last in year_pairs
    )


def parse_years_from_copyright(line):
    years = []
    for match in re.finditer(r"(20\d\d)(?:-(20\d\d))?", line):
        start = int(match.group(1))
        if "-" in match.group(0):
            end = int(match.group(2))
            years.extend(range(start, end + 1))
        else:
            years.append(start)
    return years


def get_copyright(filename, prev_line, cwd):
    """Get the copyright string."""

    historical_years = parse_years_from_copyright(prev_line)
    change_years = get_change_years_git(filename, cwd)
    combined_years = sorted(set(historical_years) | set(change_years))

    years_str = format_years(compress_years(combined_years))
    return COPYRIGHT.format(years=years_str)


def process(filename: str, contents: str):
    """Process the copyright on a single file, return the modified contents."""
    logging.info("Processing {:60}".format(filename))

    lines = contents.splitlines()
    copyright_index, license_index, author_index = find_existing_copyright(lines)
    if copyright_index is None:
        logging.error(f"Copyright not found in {filename}")
    if license_index is None:
        logging.error(f"License not found in {filename}")
    if copyright_index is None or license_index is None:
        return

    # Update copyright and license lines.
    for index, updated_line in [
        (
            copyright_index,
            get_copyright(filename, lines[copyright_index], cwd=path.dirname(filename)),
        ),
        (
            license_index,
            LICENSE,
        ),
    ]:
        if index is None:
            logging.error("Line not found in file: {}".format(updated_line))
            start_index = find_start(lines)
            lines[start_index:start_index] = [updated_line]
        else:
            existing_line = lines[index]
            if existing_line != updated_line:
                logging.warning(
                    "Replacing line:\n{}\n{}".format(existing_line, updated_line)
                )
                lines[index] = updated_line

    # Remove author line, if present.
    if author_index is not None:
        logging.info("Removing author line at {}:{}".format(filename, author_index))
        del lines[author_index]

    return "".join(line + os.linesep for line in lines)


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument(
        "--root", default=path.dirname(path.dirname(__file__)), help="Root directory"
    )

    parser.add_argument(
        "-n", "--dry-run", action="store_true", help="Don't actually overwrite the files"
    )

    args = parser.parse_args()

    for filename in find_files(args.root, r".*\.py$", r"\.hg$"):
        with open(filename) as file_:
            contents = file_.read()
        if not contents.strip():
            continue

        new_contents = process(filename, contents)
        if new_contents is None:
            continue

        if args.dry_run:
            continue
        with open(filename, "w") as file_:
            file_.write(new_contents)


if __name__ == "__main__":
    main()
