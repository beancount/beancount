#!/usr/bin/env python3
"""
Extract the org-mode structure and open/close directives from the input text.
No actual parsing.
This is a utility script used to share with others my chart-of-accounts.
"""
import re

def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Filename')
    opts = parser.parse_args()

    for line in open(opts.filename):
        if re.match(r'#!/', line):
            print(line, end='')

        if re.search(r'-\*- mode:', line):
            print(line, end='')

        if re.match(r'\*', line):
            print()
            print(line, end='')

        elif re.match(r'\d\d\d\d-\d\d-\d\d\s+(open|close)\b', line):
            print(line, end='')

        elif re.match(r'option\b', line):
            print(line, end='')


if __name__ == '__main__':
    main()
