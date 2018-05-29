"""Identify similar entries.

This can be used during import in order to identify and flag duplicate entries.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import collections

from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core.number import ONE
from beancount.core import data
from beancount.core import amount
from beancount.core import interpolate


def find_similar_entries(entries, source_entries, comparator=None, window_days=2):
    """Find which entries from a list are potential duplicates of a set.

    Note: If there are multiple entries from 'source_entries' matching an entry
    in 'entries', only the first match is returned. Note that this function
    could in theory decide to merge some of the imported entries with each
    other.

    Args:
      entries: The list of entries to classify as duplicate or note.
      source_entries: The list of entries against which to match. This is the
        previous, or existing set of entries to compare against. This may be null
        or empty.
      comparator: A functor used to establish the similarity of two entries.
      window_days: The number of days (inclusive) before or after to scan the
        entries to classify against.
    Returns:
      A list of pairs of entries (entry, source_entry) where entry is from
      'entries' and is deemed to be a duplicate of source_entry, from
      'source_entries'.
    """
    window_head = datetime.timedelta(days=window_days)
    window_tail = datetime.timedelta(days=window_days + 1)

    if comparator is None:
        comparator = SimilarityComparator()

    # For each of the new entries, look at existing entries at a nearby date.
    duplicates = []
    if source_entries is not None:
        for entry in data.filter_txns(entries):
            for source_entry in data.filter_txns(
                    data.iter_entry_dates(source_entries,
                                          entry.date - window_head,
                                          entry.date + window_tail)):
                if comparator(entry, source_entry):
                    duplicates.append((entry, source_entry))
                    break
    return duplicates


class SimilarityComparator:
    """Similarity comparator of transactions.

    This comparator needs to be able to handle Transaction instances which are
    incomplete on one side, which have slightly different dates, or potentially
    slightly different numbers.
    """

    # Fraction difference allowed of variation.
    EPSILON = D('0.05')  # 5%

    def __init__(self, max_date_delta=None):
        """Constructor a comparator of entries.
        Args:
          max_date_delta: A datetime.timedelta instance of the max tolerated
            distance between dates.
        """
        self.cache = {}
        self.max_date_delta = max_date_delta

    def __call__(self, entry1, entry2):
        """Compare two entries, return true if they are deemed similar.

        Args:
          entry1: A first Transaction directive.
          entry2: A second Transaction directive.
        Returns:
          A boolean.
        """
        # Check the date difference.
        if self.max_date_delta is not None:
            delta = ((entry1.date - entry2.date)
                     if entry1.date > entry2.date else
                     (entry2.date - entry1.date))
            if delta > self.max_date_delta:
                return False

        try:
            amounts1 = self.cache[id(entry1)]
        except KeyError:
            amounts1 = self.cache[id(entry1)] = amounts_map(entry1)
        try:
            amounts2 = self.cache[id(entry2)]
        except KeyError:
            amounts2 = self.cache[id(entry2)] = amounts_map(entry2)

        # Look for amounts on common accounts.
        common_keys = set(amounts1) & set(amounts2)
        for key in sorted(common_keys):
            # Compare the amounts.
            number1 = amounts1[key]
            number2 = amounts2[key]
            if number1 == ZERO and number2 == ZERO:
                break
            diff = abs((number1 / number2)
                       if number2 != ZERO
                       else (number2 / number1))
            if diff == ZERO:
                return False
            if diff < ONE:
                diff = ONE/diff
            if (diff - ONE) < self.EPSILON:
                break
        else:
            return False

        # Here, we have found at least one common account with a close
        # amount. Now, we require that the set of accounts are equal or that
        # one be a subet of the other.
        accounts1 = set(posting.account for posting in entry1.postings)
        accounts2 = set(posting.account for posting in entry2.postings)
        return accounts1.issubset(accounts2) or accounts2.issubset(accounts1)


def amounts_map(entry):
    """Compute a mapping of (account, currency) -> Decimal balances.

    Args:
      entry: A Transaction instance.
    Returns:
      A dict of account -> Amount balance.
    """
    amounts = collections.defaultdict(D)
    for posting in entry.postings:
        # Skip interpolated postings.
        if posting.meta and interpolate.AUTOMATIC_META in posting.meta:
            continue
        currency = isinstance(posting.units, amount.Amount) and posting.units.currency
        if isinstance(currency, str):
            key = (posting.account, currency)
            amounts[key] += posting.units.number
    return amounts
