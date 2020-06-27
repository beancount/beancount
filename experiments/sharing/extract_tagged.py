#!/usr/bin/env python3
"""Extract and convert tagged transactions from a Beancount file.

This script reads a Beancount file, extracts all transactions with a tag, and
rewrites them to convert its inflows to a single account.
"""

import argparse
import logging
import re
import sys
from typing import Dict

from beancount import loader
from beancount.core import data
from beancount.core.account import ACCOUNT_RE
from beancount.parser import printer


def adjust_entry(entry: data.Transaction, match_account: str, account: str, tag: str,
                 translate_map: Dict[str, str]):
    new_postings = []
    for posting in entry.postings:
        new_account = posting.account

        # Use the diverted account.
        if re.match(match_account, posting.account):
            new_account = posting.meta.get('diverted_account', account)
        else:
            new_account = account

        # Apply translations.
        new_account = translate_map.get(new_account, new_account)

        new_postings.append(posting._replace(account=new_account, meta=None))
    new_tags = entry.tags.difference(set(tag))
    return entry._replace(postings=new_postings,
                          tags=new_tags)


def parse_translate_map(translate_options):
    """Convert the --translate option to a dict."""
    translate_map = {}
    for translate in translate_options:
        match = re.match('({})=({})$'.format(ACCOUNT_RE, ACCOUNT_RE), translate)
        assert match
        tfrom, tto = match.groups()
        translate_map[tfrom] = tto
    return translate_map


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename', action='store',
                        help="Beancount filename to read")

    parser.add_argument('tag', action='store',
                        help="Tag to select transactions")

    parser.add_argument('inflows_account', action='store',
                        help="Inflows account to use for the rewritten transactions")

    parser.add_argument('-t', '--translate', action='append',
                        help=("Explicit account translations. This can be used to make "
                              "transfers be treated specially."))

    args = parser.parse_args()
    translate_map = parse_translate_map(args.translate)
    entries, _, options_map = loader.load_file(args.filename)
    imported_entries = []
    tag = args.tag.lstrip('#')
    match_account = 'Expenses:Kai'  # FIXME: Make this an option.
    for entry in data.filter_txns(entries):
        if tag in entry.tags:
            entry = adjust_entry(entry, match_account, args.inflows_account, args.tag,
                                 translate_map)
            imported_entries.append(entry)

    printer.print_entries(imported_entries)


if __name__ == '__main__':
    main()
