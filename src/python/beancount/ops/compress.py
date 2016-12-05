"""Compress multiple entries into a single one.

This can be used during import to compress the effective output, for accounts
with a large number of similar entries. For example, I had a trading account
which would pay out interest every single day. I have no desire to import the
full detail of these daily interests, and compressing these interest-only
entries to monthly ones made sense. This is the code that was used to carry this
out.
"""
__copyright__ = "Copyright (C) 2013, 2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core.number import Decimal
from beancount.core.amount import Amount
from beancount.core import data


def compress(entries, predicate):
    """Compress multiple transactions into single transactions.

    Replace consecutive sequences of Transaction entries that fulfill the given
    predicate by a single entry at the date of the last matching entry.
    'predicate' is the function that determines if an entry should be
    compressed.

    This can be used to simply a list of transactions that are similar and occur
    frequently. As an example, in a retail FOREX trading account, differential
    interest of very small amounts is paid every day; it is not relevant to look
    at the full detail of this interest unless there are other transactions. You
    can use this to compress it into single entries between other types of
    transactions.

    Args:
      entries: A list of directives.
      predicate: A callbable which accepts an entry and return true if the entry
          is intended to be compressed.
    Returns:
      A list of directives, with compressible transactions replaced by a summary
      equivalent.
    """
    new_entries = []
    pending = []
    for entry in entries:
        if isinstance(entry, data.Transaction) and predicate(entry):
            # Save for compressing later.
            pending.append(entry)
        else:
            # Compress and output all the pending entries.
            if pending:
                new_entries.append(merge(pending, pending[-1]))
                pending.clear()

            # Output the differing entry.
            new_entries.append(entry)

    if pending:
        new_entries.append(merge(pending, pending[-1]))

    return new_entries


def merge(entries, prototype_txn):
    """Merge the postings of a list of Transactions into a single one.

    Merge postings the given entries into a single entry with the Transaction
    attributes of the prototype. Return the new entry. The combined list of
    postings are merged if everything about the postings is the same except the
    number.

    Args:
      entries: A list of directives.
      prototype_txn: A Transaction which is used to create the compressed
          Transaction instance. Its list of postings is ignored.
    Returns:
      A new Transaction instance which contains all the postings from the input
      entries merged together.

    """
    # Aggregate the postings together. This is a mapping of numberless postings
    # to their number of units.
    postings_map = collections.defaultdict(Decimal)
    for entry in data.filter_txns(entries):
        for posting in entry.postings:
            # We strip the number off the posting to act as an aggregation key.
            key = data.Posting(posting.account,
                               Amount(None, posting.units.currency),
                               posting.cost,
                               posting.price,
                               posting.flag,
                               None)
            postings_map[key] += posting.units.number

    # Create a new transaction with the aggregated postings.
    new_entry = data.Transaction(prototype_txn.meta,
                                 prototype_txn.date,
                                 prototype_txn.flag,
                                 prototype_txn.payee,
                                 prototype_txn.narration,
                                 data.EMPTY_SET, data.EMPTY_SET, [])

    # Sort for at least some stability of output.
    sorted_items = sorted(postings_map.items(),
                          key=lambda item: (item[0].account,
                                            item[0].units.currency,
                                            item[1]))

    # Issue the merged postings.
    for posting, number in sorted_items:
        units = Amount(number, posting.units.currency)
        new_entry.postings.append(
            data.Posting(posting.account, units, posting.cost, posting.price,
                         posting.flag, posting.meta))

    return new_entry
