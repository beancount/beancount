#!/usr/bin/env python3
"""A script to process all source code and update the license and copyright headers.

hg log --template '{date|isodate} {desc}\n' src/python/beancount/core/inventory.py    2>&1 | less
"""
__copyright__ = "Copyright (C) 2007-2016  Martin Blais"
__license__ = "GNU GPL v2 only"

import argparse
import contextlib
import datetime
import logging
import os
import re
import tempfile
import subprocess
import unittest
from os import path


COPYRIGHT = [
    '__copyright__ = "Copyright (C) {years}  Martin Blais"',
    '__license__ = "GNU GPLv2"',
    ]


def find_files(rootdir, regexp_files, ignore_dirs):
    """Find the files we need to apply this to."""
    for root, dirs, files in os.walk(rootdir):
        with contextlib.suppress(ValueError):
            dirs.remove('build')
        dirs[:] = [dirname
                   for dirname in dirs
                   if not re.match(ignore_dirs, dirname)]
        for filename in files:
            if re.match(regexp_files, filename):
                yield path.join(root, filename)


def find_start(lines):
    contents = ''.join(line + os.linesep for line in lines)
    start = 0
    while True:
        match = re.match(r'(^#[^\n]*|""".*?""".*?)\n', contents[start:], re.DOTALL)
        if match:
            start += match.end()
        else:
            break
    return len(contents[:start].splitlines())


def get_change_years(filename):
    """Find the relevant years where there was a change."""
    log_lines = subprocess.check_output(
        ['hg', 'log', '--template', '{date|isodate}\n', filename],
        cwd="/tmp")
    log_years = set()
    for line in log_lines.decode('utf8').splitlines():
        log_years.add(int(re.match('([0-9]{4})-', line).group(1)))
    return sorted(log_years)


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
        self.assertEqual([(1998, 2000),
                          (2002, 2002),
                          (2005, 2006),
                          (2008, 2008)],
                         compress_years([1998, 1999, 2000, 2002, 2005, 2006, 2008]))

def format_years(year_pairs):
    """Format a list of year-pairs."""
    if not year_pairs:
        return str(datetime.date.today().year)
    return ', '.join(str(first) if first == last else '{}-{}'.format(first, last)
                     for first, last in year_pairs)

def add_copyright(filename, lines, index):
    if index is None:
        index = find_start(lines)
    years_str = format_years(compress_years(get_change_years(filename)))
    print(filename, ':', years_str)
    return (lines[:index] +
            [line.format(years=years_str) for line in COPYRIGHT] +
            lines[index:])


def process(filename, contents):
    """Process the copyright on a single file, return the modified contents."""
    lines = contents.splitlines()
    for index, line in enumerate(lines):
        if re.match('__author__', line):
            break
    else:
         index = None
    lines = [line for line in lines if not re.match('__author__', line)]
    lines = add_copyright(filename, lines, index)
    return ''.join(line + os.linesep for line in lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('--root', default=path.dirname(path.dirname(__file__)),
                        help='Root directory')
    args = parser.parse_args()

    for filename in find_files(args.root, r'.*\.py$', r'\.hg$'):
        with open(filename) as f:
            contents = f.read()
        if not contents.strip():
            continue

        new_contents = process(filename, contents)

        if 0:
        with tempfile.NamedTemporaryFile('w') as f:
            f.write(new_contents)
            f.flush()
            if 0:
                print(filename)
                subprocess.call(['diff', filename, f.name])

        with open(filename, 'w') as f:
            f.write(new_contents)


if __name__ == '__main__':
    main()
