#!/usr/bin/env python3
"""LedgerHub has had a bug for a while where some narrations have doubled
description, such as

  2018-10-17 * "TD AMERITRADE    ACH IN / TD AMERITRADE    ACH IN"

This script attempts to fix this by replacing such strings in a text file.
Note: It does not parse the input file with Beancount (it does not need to).
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import re
import sys


def replace_dupped_string(match):
    text = match.group(1)
    groups = text.split('/')
    if len(groups) == 2:
        group1 = groups[0].strip()
        group2 = groups[1].strip()
        if group1 == group2:
            text = group1
    return '"{}"'.format(text)


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Filename')
    parser.add_argument('-o', '--output', action='store',
                        help="Output filename (defaut is stdout)")
    args = parser.parse_args()

    outfile = open(args.output, 'wb') if args.output else sys.stdout.buffer
    for line in open(args.filename, encoding='utf8'):
        newline = re.sub('"(.*?/.*?)"', replace_dupped_string, line)
        outfile.write(newline.encode('utf8'))


if __name__ == '__main__':
    main()
