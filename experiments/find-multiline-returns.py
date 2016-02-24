#!/usr/bin/env python3
"""Find the instances of multi-line docstring Returns sections.

See: https://bitbucket.org/birkenfeld/sphinx-contrib/issues/111/napoleon-multiple-returns-indentations
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import logging
import os
import re
import io
import textwrap
from os import path


ROOT = path.join(path.dirname(path.dirname(__file__)), 'src/python/beancount')


def find_python_files(directory):
    for root, dirs, files in os.walk(directory):
        for filename in files:
            if filename.endswith('.py'):
                yield path.join(root, filename)


def main():
    #logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('--root', default=ROOT, help='Root directory to search')
    args = parser.parse_args()

    for filename in find_python_files(args.root):
        logging.info("File: %s", filename)
        it = enumerate(open(filename))
        for no, line in it:
            if re.match(r'\s*Returns:', line):
                oss = io.StringIO()
                no, line = next(it)
                first_no = no
                while True:
                    if re.match(r"\s*(\"\"\"|'''|Raises:)", line):
                        break
                    print(line, end="", file=oss)
                    no, line = next(it)
                paragraph = textwrap.dedent(oss.getvalue())
                if any(re.match(r"\s+", line)
                       for line in paragraph.splitlines()
                       if line.strip()):
                    print("{}:{}:".format(filename, first_no))
                    print(oss.getvalue())
                    print()


if __name__ == '__main__':
    main()
