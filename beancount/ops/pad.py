"""Automatic padding of gaps between entries.
"""
__copyright__ = "Copyright (C) 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import account
from beancount.core import amount
from beancount.core import inventory
from beancount.core import data
from beancount.core import position
from beancount.core import flags
from beancount.core import realization
from beancount.utils import misc_utils
from beancount.ops import balance

__plugins__ = ('pad',)


PadError = collections.namedtuple('PadError', 'source message entry')


def pad(entries, options_map):
    """Insert transaction entries for to fulfill a subsequent balance check.

    Synthesize and insert Transaction entries right after Pad entries in order
    to fulfill checks in the padded accounts. Returns a new list of entries.
    Note that this doesn't pad across parent-child relationships, it is a very
    simple kind of pad. (I have found this to be sufficient in practice, and
    simpler to implement and understand.)

    Furthermore, this pads for a single currency only, that is, balance checks
    are specified only for one currency at a time, and pads will only be
    inserted for those currencies.

    Args:
      entries: A list of directives.
      options_map: A parser options dict.
    Returns:
      A new list of directives, with Pad entries inserted, and a list of new
      errors produced.
    """
    pad_errors = []

    # Find all the pad entries and group them by account.
    pads = list(misc_utils.filter_type(entries, data.Pad))
    pad_dict = misc_utils.groupby(lambda x: x.account, pads)

    # Partially realize the postings, so we can iterate them by account.
    by_account = realization.postings_by_account(entries)

    # A dict of pad -> list of entries to be inserted.
    new_entries = {id(pad): [] for pad in pads}

    # Process each account that has a padding group.
    for account_, pad_list in sorted(pad_dict.items()):

        # Last encountered / currency active pad entry.
        active_pad = None

        # Gather all the postings for the account and its children.
        postings = []
        is_child = account.parent_matcher(account_)
        for item_account, item_postings in by_account.items():
            if is_child(item_account):
                postings.extend(item_postings)
        postings.sort(key=data.posting_sortkey)

        # A set of currencies already padded so far in this account.
        padded_lots = set()

        pad_balance = inventory.Inventory()
        for entry in postings:

            assert not isinstance(entry, data.Posting)
            if isinstance(entry, data.TxnPosting):
                # This is a transaction; update the running balance for this
                # account.
                pad_balance.add_position(entry.posting)

            elif isinstance(entry, data.Pad):
                if entry.account == account_:
                    # Mark this newly encountered pad as active and allow all lots
                    # to be padded heretofore.
                    active_pad = entry
                    padded_lots = set()

            elif isinstance(entry, data.Balance):
                check_amount = entry.amount

                # Compare the current balance amount to the expected one from
                # the check entry. IMPORTANT: You need to understand that this
                # does not check a single position, but rather checks that the
                # total amount for a particular currency (which itself is
                # distinct from the cost).
                balance_amount = pad_balance.get_currency_units(check_amount.currency)
                diff_amount = amount.sub(balance_amount, check_amount)

                # Use the specified tolerance or automatically infer it.
                tolerance = balance.get_balance_tolerance(entry, options_map)

                if abs(diff_amount.number) > tolerance:
                    # The check fails; we need to pad.

                    # Pad only if pad entry is active and we haven't already
                    # padded that lot since it was last encountered.
                    if active_pad and (check_amount.currency not in padded_lots):

                        # Note: we decide that it's an error to try to pad
                        # positions at cost; we check here that all the existing
                        # positions with that currency have no cost.
                        positions = [pos
                                     for pos in pad_balance.get_positions()
                                     if pos.units.currency == check_amount.currency]
                        for position_ in positions:
                            if position_.cost is not None:
                                pad_errors.append(
                                    PadError(entry.meta,
                                             ("Attempt to pad an entry with cost for "
                                              "balance: {}".format(pad_balance)),
                                             active_pad))

                        # Thus our padding lot is without cost by default.
                        diff_position = position.Position.from_amounts(
                            amount.Amount(check_amount.number - balance_amount.number,
                                          check_amount.currency))

                        # Synthesize a new transaction entry for the difference.
                        narration = ('(Padding inserted for Balance of {} for '
                                     'difference {})').format(check_amount, diff_position)
                        new_entry = data.Transaction(
                            active_pad.meta.copy(), active_pad.date, flags.FLAG_PADDING,
                            None, narration, data.EMPTY_SET, data.EMPTY_SET, [])

                        new_entry.postings.append(
                            data.Posting(active_pad.account,
                                         diff_position.units, diff_position.cost,
                                         None, None, None))
                        neg_diff_position = -diff_position
                        new_entry.postings.append(
                            data.Posting(active_pad.source_account,
                                         neg_diff_position.units, neg_diff_position.cost,
                                         None, None, None))

                        # Save it for later insertion after the active pad.
                        new_entries[id(active_pad)].append(new_entry)

                        # Fixup the running balance.
                        pos, _ = pad_balance.add_position(diff_position)
                        if pos is not None and pos.is_negative_at_cost():
                            raise ValueError(
                                "Position held at cost goes negative: {}".format(pos))

                # Mark this lot as padded. Further checks should not pad this lot.
                padded_lots.add(check_amount.currency)

    # Insert the newly created entries right after the pad entries that created them.
    padded_entries = []
    for entry in entries:
        padded_entries.append(entry)
        if isinstance(entry, data.Pad):
            entry_list = new_entries[id(entry)]
            if entry_list:
                padded_entries.extend(entry_list)
            else:
                # Generate errors on unused pad entries.
                pad_errors.append(
                    PadError(entry.meta, "Unused Pad entry", entry))

    return padded_entries, pad_errors
