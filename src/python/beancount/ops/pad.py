"""Automatic padding of gaps between entries.
"""
from collections import namedtuple, defaultdict

from beancount.core.position import Lot, Position
from beancount.core.inventory import Inventory
from beancount.core.amount import Decimal, amount_sub
from beancount.core.data import Transaction, Balance, Open, Close, Pad, Note, Document, Posting
from beancount import utils
from beancount.core import flags


PadError = namedtuple('PadError', 'fileloc message entry')

PAD_PRECISION = Decimal('.015')

def pad(entries):
    """Synthesize and insert Transaction entries right after Pad entries in order to
    fulfill checks in the padded accounts. Returns a new list of entries. Note
    that this doesn't pad across parent-child relationships, it is a very simple
    kind of pad. (I have found this to be sufficient in practice, and simpler to
    implement and understand.) """

    # Find all the pad entries and group them by account.
    pads = list(utils.filter_type(entries, Pad))
    pad_dict = utils.groupby(lambda x: x.account, pads)

    # Partially realize the postings, so we can iterate them by account.
    by_account = group_postings_by_account(entries, set(pad_dict.keys()))

    # A dict of pad -> list of entries to be inserted.
    new_entries = {pad: [] for pad in pads}

    # Process each account that has a padding group.
    for account, pad_list in sorted(pad_dict.items()):

        # Last encountered / currency active pad entry.
        active_pad = None

        # A set of currencies already padded so far in this account.
        padded_lots = set()

        balance = Inventory()
        for entry in by_account[account]:

            if isinstance(entry, Posting):
                # This is a transaction; update the running balance for this
                # account.
                balance.add_position(entry.position, allow_negative=True)

            elif isinstance(entry, Pad):
                # Mark this newly encountered pad as active and allow all lots
                # to be padded heretofore.
                active_pad = entry
                padded_lots = set()

            elif isinstance(entry, Balance):
                check_amount = entry.amount

                # Compare the current balance amount to the expected one from
                # the check entry. IMPORTANT: You need to understand that this
                # does not check a single position, but rather checks that the
                # total amount for a particular currency (which itself is
                # distinct from the cost).
                balance_amount = balance.get_amount(check_amount.currency)
                diff_amount = amount_sub(balance_amount, check_amount)
                if abs(diff_amount.number) > PAD_PRECISION:
                    # The check fails; we need to pad.

                    # Pad only if pad entry is active and we haven't already
                    # padded that lot since it was last encountered.
                    if active_pad and (check_amount.currency not in padded_lots):

                        # Calculate the difference.
                        balance_number = (ZERO
                                          if balance_amount is None
                                          else balance_amount.number)

                        # Note: we decide that it's an error to try to pad
                        # position at cost; we check here that all the existing
                        # positions with that currency have no cost.
                        positions = balance.get_positions_with_currency(check_amount.currency)
                        for position in positions:
                            if position.lot.cost is not None:
                                pad_errors.append(
                                    PadError(entry.fileloc,
                                             "Attempt to pad an entry with cost for balance: {}".format(balance),
                                             active_pad))

                        # Thus our padding lot is without cost by default.
                        lot = Lot(check_amount.currency, None, None)
                        diff_position = Position(lot, check_amount.number - balance_amount.number)

                        # Synthesize a new transaction entry for the difference.
                        narration = '(Padding inserted for Balance of {} for difference {})'.format(
                            check_amount, diff_position)
                        new_entry = Transaction(
                            active_pad.fileloc, active_pad.date, flags.FLAG_PADDING, None, narration, None, None, [])

                        new_entry.postings.append(
                            Posting(new_entry, active_pad.account, diff_position, None, None))
                        new_entry.postings.append(
                            Posting(new_entry, active_pad.account_pad, -diff_position, None, None))

                        # Save it for later insertion after the active pad.
                        new_entries[active_pad].append(new_entry)

                        # Fixup the running balance.
                        balance.add_position(diff_position)

                        # Mark this lot as padded. Further checks should not pad this lot.
                        padded_lots.add(check_amount.currency)

    # Insert the newly created entries right after the pad entries that created them.
    padded_entries = []
    pad_errors = []
    for entry in entries:
        padded_entries.append(entry)
        if isinstance(entry, Pad):
            entry_list = new_entries[entry]
            padded_entries.extend(entry_list)

            # Generate errors on unused pad entries.
            if not entry_list:
                pad_errors.append(
                    PadError(entry.fileloc, "Unused Pad entry: {}".format(entry), entry))

    return padded_entries, pad_errors


def group_postings_by_account(entries, only_accounts=None):
    """Build lists of entries by account.
    Return a dict of account -> (entry or Posting instance)."""

    by_accounts = defaultdict(list)

    for entry in entries:

        if isinstance(entry, Transaction):
            for posting in entry.postings:
                if (only_accounts is not None and
                    posting.account not in only_accounts):
                    continue
                by_accounts[posting.account].append(posting)

        elif isinstance(entry, (Balance, Open, Close, Pad, Note, Document)):
            if (only_accounts is not None and
                entry.account not in only_accounts):
                continue
            by_accounts[entry.account].append(entry)

    return by_accounts
