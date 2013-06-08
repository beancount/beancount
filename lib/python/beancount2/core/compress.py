"""
Code to compress multiple entries into a single one, as is done during import.
"""
import collections

from beancount2.core.data import Transaction, Posting
from beancount2.core.inventory import Position


def compress(entries, predicate):
    """Replace consecutive sequences of Transaction entries that fulfill the given
    predicate by a single entry at the date of the last entry found. 'predicate'
    is a function that accepts an entry as an argument and returns a boolean,
    true if the entry should be compressed.

    This can be used to simply a list of transactions that are similar and occur
    frequently. As an example, in a retail FOREX trading account, differential
    interest of very small amounts is paid every day; it is not relevant to look
    at the full detail of this interest unless there are other transactions. You
    can use this to compress it into single entries between other types of
    transactions.
    """

    new_entries = []

    pending = []
    for entry in entries:
        if isinstance(entry, Transaction) and predicate(entry):
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


def merge(entries, prototype_entry):
    """Merge the given entries into a single entry with the characteristics of the
    prototype. Return the new entry. The combined list of postings are merged if
    everything about the postings is the same except the number.
    """

    # Aggregate the postings together.
    postings_map = collections.defaultdict(Decimal)
    for entry in entries:
        for posting in entry.postings:
            # Strip the number off the posting to act as an aggregation key.
            lot = posting.position.lot
            key = Posting(None, posting.account, lot, posting.price, posting.flag)

            postings_map[key] += posting.position.number

    # Create a new transaction with the aggregated postings.
    new_entry = Transaction(prototype_entry.fileloc,
                            prototype_entry.date,
                            prototype_entry.flag,
                            prototype_entry.payee,
                            prototype_entry.narration,
                            None, None, [])

    for posting, number in sorted(postings_map.items()):
        lot = posting.position
        position = Position(lot, number)
        new_entry.postings.append(
            Posting(new_entry,
                    posting.account, position, posting.price, posting.flag))

    return new_entry
