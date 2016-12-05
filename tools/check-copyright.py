#!/usr/bin/env python3
"""Check that copyright notices are present on all source code files.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

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


REQUIRED = [
    ('Copyright', r'__copyright__ = "Copyright \(C\) ([0-9\,\-\ ]+)  Martin Blais"'),
    ('License', r'__license__ = "GNU GPLv2"'),
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


def check_required(filename, contents, errorfun):
    """Process the copyright on a single file, return the modified contents."""
    for line_type, required_regexp in REQUIRED:
        if not re.search(required_regexp, contents):
            errorfun("{}:0: {} not found.".format(filename, line_type))


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
        check_required(filename, contents, print)


if __name__ == '__main__':
    main()
