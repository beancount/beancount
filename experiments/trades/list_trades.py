#!/usr/bin/env python3
"""Extract a list of trades from metadata crumbs inserted by the booking code."""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import collections
import logging

from beancount import loader
from beancount.core import data


def extract_trades(entries):
    """Extract trades from a stream of directives."""
    trade_map = collections.defaultdict(list)
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        for posting in entry.postings:
            trade_ids = posting.meta.get("trade_ids", None) if posting.meta else None
            if trade_ids:
                for trade_id in trade_ids:
                    trade_map[trade_id].append(posting)
    return trade_map


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount filename')
    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)
    trade_map = extract_trades(entries)
    for trade_id, postings in trade_map.items():
        print(trade_id)
        for posting in postings:
            print("  ", posting)
        print()


if __name__ == '__main__':
    main()
