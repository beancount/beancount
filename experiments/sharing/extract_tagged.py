#!/usr/bin/env python3
"""Extract and convert tagged transactions from a Beancount file.

This script reads a Beancount file, extracts all transactions with a tag, and
rewrites them to convert its inflows to a single account.
"""

import argparse
import logging
import re

from beancount import loader
from beancount.core import data
from beancount.parser import printer


def adjust_entry(entry: data.Transaction, account: str, tag: str):
    new_postings = []
    for posting in entry.postings:
        new_account = posting.account
        if re.match('Expenses:', posting.account):
            new_account = posting.meta.get('diverted_account', account)
        else:
            new_account = account
        new_postings.append(posting._replace(account=new_account, meta=None))
    new_tags = entry.tags.difference(set(tag))
    return entry._replace(postings=new_postings,
                          tags=new_tags)


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename', action='store',
                        help="Beancount filename to read")

    parser.add_argument('tag', action='store',
                        help="Tag to select transactions")

    parser.add_argument('inflows_account', action='store',
                        help="Inflows account to use for the rewritten transactions")

    args = parser.parse_args()

    entries, _, options_map = loader.load_file(args.filename)

    imported_entries = []
    tag = args.tag.lstrip('#')
    for entry in data.filter_txns(entries):
        if tag in entry.tags:
            entry = adjust_entry(entry, args.inflows_account, args.tag)
            imported_entries.append(entry)

    printer.print_entries(imported_entries)


if __name__ == '__main__':
    main()
