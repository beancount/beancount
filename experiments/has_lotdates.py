#!/usr/bin/env python3
"""A simple script that checks whether we have a lot-date on any postings at all.

This prints out entries with an explicit lot-date on one of its postings.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

from beancount.core import data
from beancount.parser import printer
from beancount import loader


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Ledger filename')
    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)
    for entry in entries:
        if (isinstance(entry, data.Transaction)
            and any(posting.position.lot.lot_date
                    for posting in entry.postings)):
            printer.print_entry(entry)


if __name__ == '__main__':
    main()
