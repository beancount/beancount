#!/usr/bin/env python3
"""Find the instances of multi-line docstring Returns sections.

See: https://bitbucket.org/birkenfeld/sphinx-contrib/issues/111
"""

__copyright__ = "Copyright (C) 2016-2017, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import io
import logging
import os
import re
import textwrap
from os import path

ROOT = path.join(path.dirname(path.dirname(__file__)), "beancount")


def find_python_files(directory):
    for root, dirs, files in os.walk(directory):
        for filename in files:
            if filename.endswith(".py"):
                yield path.join(root, filename)


def main():
    # logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("--root", default=ROOT, help="Root directory to search")
    args = parser.parse_args()

    for filename in find_python_files(args.root):
        logging.info("File: %s", filename)
        lines = enumerate(open(filename))
        for lineno, line in lines:
            if re.match(r"\s*Returns:", line):
                oss = io.StringIO()
                lineno, line = next(lines)
                first_no = lineno
                while True:
                    if re.match(r"\s*(\"\"\"|'''|Raises:)", line):
                        break
                    print(line, end="", file=oss)
                    lineno, line = next(lines)
                paragraph = textwrap.dedent(oss.getvalue())
                if any(
                    re.match(r"\s+", line)
                    for line in paragraph.splitlines()
                    if line.strip()
                ):
                    print("{}:{}:".format(filename, first_no))
                    print(oss.getvalue())
                    print()


if __name__ == "__main__":
    main()
