#!/usr/bin/env python3
"""Extract the error types output from pylint.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import re
import argparse

def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Lint output errors')
    args = parser.parse_args()

    error_types = collections.defaultdict(list)
    for line in open(args.filename):
        match = re.match(r'.*\[([A-Z].*),.*\]', line)
        if match:
            etype = match.group(1)
            error_types[etype].append(line)

    for etype, lines in sorted(error_types.items()):
        print('{:40}: {}'.format(etype, len(lines)))


if __name__ == '__main__':
    main()
