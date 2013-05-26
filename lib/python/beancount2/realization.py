"""Realization of specific lists of account postings into reports.
"""
import datetime
from itertools import chain, repeat
from collections import namedtuple, defaultdict
from copy import copy
import collections

from beancount2.utils import tree_utils
from beancount2.utils.bisect_key import bisect_left_withkey
from beancount2.inventory import Inventory, Position
from beancount2.parser import parser
from beancount2.data import *
from beancount2 import data
from beancount2 import utils


# A realized account, inserted in a tree, that contains the list of realized
# entries.
RealAccount = namedtuple('RealAccount', 'name account children postings')


# All realized entries are either one of RealEntry or RealPosting.

# A realized entry, that possibly contains the actual balance that was seen at
# that point. This is used for all non-posting entries.
RealEntry = namedtuple('RealEntry', 'entry balance')

# A realized posting, that points to a particular posting of transaction entry.
# The 'entry' attribute may point to a transaction or a Pad entry.
RealPosting = namedtuple('RealPosting', 'entry posting balance')



RealError = namedtuple('RealError', 'fileloc message')


class RealAccountTree(tree_utils.TreeDict):
    """A container for a hierarchy of accounts, that can conveniently
    create and maintain a hierarchy of accounts."""

    def __init__(self, accounts_map):
        self.accounts_map = accounts_map
        tree_utils.TreeDict.__init__(self, self, ':')

    def create_node(self, account_name):
        account = self.accounts_map.get(account_name)
        return RealAccount(account_name, account, [], [])

    def get_name(self, real_account):
       return real_account.name.split(':')[-1]

    def get_children(self, real_account):
        return real_account.children


def group_postings_by_account(entries, only_accounts=None):
    """Build lists of entries by account. Return a dict of account -> (RealEntry or
    RealPosting)."""

    by_accounts = defaultdict(list)

    for entry in entries:

        if isinstance(entry, Transaction):
            for posting in entry.postings:
                if (only_accounts is not None and
                    posting.account not in only_accounts):
                    continue
                by_accounts[posting.account].append(
                    RealPosting(entry, posting, None))

        elif isinstance(entry, (Check, Open, Close, Pad, Note)):
            if (only_accounts is not None and
                entry.account not in only_accounts):
                continue
            by_accounts[entry.account].append(
                RealEntry(entry, None))

    return by_accounts


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
        for real_entry in by_account[account]:

            if isinstance(real_entry, RealPosting):
                # This is a transaction; update the running balance for this
                # account.
                balance.add_position(real_entry.posting.position, allow_negative=True)

            elif isinstance(real_entry.entry, Pad):
                # Mark this newly encountered pad as active and allow all lots
                # to be padded heretofore.
                active_pad = real_entry.entry
                padded_lots = set()

            elif isinstance(real_entry.entry, Check):
                check = real_entry.entry
                check_position = check.position

                # Compare the current balance position to the expected one from
                # the check entry.
                balance_position = balance.get_position(check.position.lot)
                if check_position != balance_position:
                    # The check fails; we need to pad.

                    # Pad only if pad entry is active and we haven't already
                    # padded that lot since it was last encountered.
                    if active_pad and (check_position.lot not in padded_lots):
                        # Calculate the difference.
                        balance_number = (ZERO
                                          if balance_position is None
                                          else balance_position.number)
                        diff_position = Position(check_position.lot,
                                                 check_position.number - balance_number)

                        # Synthesize a new transaction entry for the difference.
                        narration = '(Padding inserted for Check of {})'.format(check_position)
                        postings = [
                            Posting(active_pad.account, diff_position, None, None),
                            Posting(active_pad.account_pad, -diff_position, None, None),
                        ]
                        new_entry = Transaction(
                            active_pad.fileloc, active_pad.date, FLAG_PADDING, None, narration, set(), postings)

                        # Save it for later insertion after the active pad.
                        new_entries[active_pad].append(new_entry)

                        # Fixup the running balance.
                        balance.add_position(diff_position)

                        # Mark this lot as padded. Further checks should not pad this lot.
                        padded_lots.add(check_position.lot)

    # Insert the newly created entries right after the pad entries that created them.
    padded_entries = []
    for entry in entries:
        padded_entries.append(entry)
        if isinstance(entry, Pad):
            padded_entries.extend(new_entries[entry])

    # Generate errors on unused pad entries.
    pad_errors = []
    for pad, entry_list in new_entries.items():
        if not entry_list:
            pad_errors.append(
                RealError(pad.fileloc, "Unused Pad entry: {}".format(pad)))

    return padded_entries, pad_errors


def realize(entries, do_check=False):
    """Compute the running balances and realize a list of entries into a tree of
    realized accounts, which contains shadow entries with balances. This is then
    used to issue reports. This routine is actually really simple: it runs
    through a list of entries, and creates per-account list of "realized"
    postings, which may be of two possible types:

      RealPosting -> points to a specific posting of a transaction entry.
      RealEntry -> points to any other type of entry.

    These realized entries also have a pre-computed balance used for rendering
    reports. The function returns a tree of "realized" accounts, which contain
    lists of these realized entries. Here's a simple diagram that summarizes
    this seemingly complex, but rather simple data structure:

       +-------------+      +-------------+     +------+
       | RealAccount |----->|  RealEntry  |---->| Open |
       +-------------+      +-------------+     +------+
                                   |
                                   v
                            +-------------+     +-------------+
                            | RealPosting |---->| Transaction |
                            +-------------+     +-------------+
                                   |      \        |         \
                                   |       \  +---------+  +---------+
                                   |        `>| Posting |  | Posting |
                                   |          +---------+  +---------+
                                   v
                            +-------------+     +-----+
                            |  RealEntry  |---->| Pad |
                            +-------------+     +-----+
                                   |
                                   v
                            +-------------+     +-------+
                            |  RealEntry  |---->| Check |
                            +-------------+     +-------+
                                   |
                                   v
                            +-------------+     +-------+
                            |  RealEntry  |---->| Close |
                            +-------------+     +-------+
                                   |
                                   .

    If 'check' is true, verify that Check entry balances succeed and issue error
    messages if they fail.
    """

    # Handle sanity checks when the check is at the beginning of the day.
    check_is_at_beginning_of_day = parser.SORT_ORDER[Check] < 0

    accounts_map = data.gather_accounts(entries)

    real_accounts = RealAccountTree(accounts_map)
    real_errors = []

    def add_real_entry(account, entry):
        "Create RealEntry instances with the running balance."
        balance = balances[entry.account]
        real_account = real_accounts.get_create(account.name)
        real_account.postings.append(RealEntry(entry, copy(balance)))

    # Running balance for each account.
    balances = defaultdict(Inventory)

    prev_date = datetime.date(1900, 1, 1)
    for entry in entries:

        if isinstance(entry, Transaction):

            # Update the balance inventory for each of the postings' accounts.
            for posting in entry.postings:
                balance = balances[posting.account]
                try:
                    # Note: if this is from a padding transaction, allow negative lots at cost.
                    balance.add_position(posting.position,
                                         allow_negative=entry.flag in (FLAG_PADDING, FLAG_SUMMARIZE))
                except ValueError as e:
                    real_errors.append(
                        RealError(entry.fileloc, "Error balancing '{}' -- {}".format(posting.account.name, e)))

                real_account = real_accounts.get_create(posting.account.name)
                real_account.postings.append(
                    RealPosting(entry, posting, copy(balance)))

        elif isinstance(entry, Check):

            # Add the check realization to the account.
            # FIXME: We somehow need to indicate the success or failure of this check somehow, for rendering.
            add_real_entry(entry.account, entry)

            # Check the balance against the check entry.
            if do_check:

                check_position = entry.position
                balance = balances[entry.account]
                balance_position = balance.get_position(check_position.lot)
                if check_position != balance_position:
                    # This check failed; issue an error.
                    real_errors.append(
                        RealError(entry.fileloc, "Check failed for '{}': {} != {}".format(
                            entry.account.name, balance_position, check_position)))

        elif isinstance(entry, Pad):

            # Insert the pad entry in both realized accounts.
            add_real_entry(entry.account, entry)
            add_real_entry(entry.account_pad, entry)

        elif isinstance(entry, (Open, Close, Note)):

            # Append some other entries in the realized list.
            add_real_entry(entry.account, entry)

        if check_is_at_beginning_of_day:
            # Note: Check entries are assumed to have been sorted to be before any
            # other entries with the same date. This is supposed to be done by the
            # parser. Verify this invariant here.
            if isinstance(entry, (Check, Open)):
                assert entry.date > prev_date, (entry, prev_entry)
            else:
                prev_entry = entry
                prev_date = entry.date

    return (real_accounts, real_errors)


def get_real_subpostings(real_account):
    """Given a RealAccount instance, return a sorted list of all its postings and
    the postings of its child accounts."""

    accumulator = []
    _get_real_subpostings(real_account, accumulator)
    accumulator.sort(key=lambda rposting: parser.entry_sortkey(rposting.entry))
    return accumulator

def _get_real_subpostings(real_account, accumulator):
    "Internal recursive routine to get all the child postings."
    accumulator.extend(real_account.postings)
    for child_account in real_account.children:
        _get_real_subpostings(child_account, accumulator)


def dump_tree_balances(real_accounts, foutput):
    """Dump a simple tree of the account balances, for debugging."""
    lines = list(real_accounts.render_lines())
    width = max(len(line[0]) for line in lines)
    for line_first, line_next, real_account in lines:
        last_entry = real_account.postings[-1] if real_account.postings else None
        balance = getattr(last_entry, 'balance', None)
        if balance:
            amounts = balance.get_cost().get_amounts()
            positions = ['{0.number:12.2f} {0.currency}'.format(amount)
                         for amount in sorted(amounts, key=amount_sortkey)]
        else:
            positions = ['']
        for position, line in zip(positions, chain((line_first,), repeat(line_next))):
            foutput.write('{:{width}}   {:16}\n'.format(line, position, width=width))


def find_balance(real_account, date=None):
    """Find the balance of a realzed right before the given date.
    If date is None, get the final balance of the entire list of realized entries."""

    postings = real_account.postings
    if date is None:
        # Find the last posting that had a non-null balance.
        index = len(postings) - 1
    else:
        index = bisect_left_withkey(postings, date,
                                    key=lambda real_posting: real_posting.entry.date)
    if index == 0:
        return 0, Inventory()
    else:
        # Take the balance of the previous element, the last one on previous
        # dates.
        index -= 1

        # Find the last posting that had a non-null balance.
        for i in range(index, -1, -1):
            real_posting = postings[i]
            if isinstance(real_posting, RealPosting):
                return (index+1), real_posting.balance
        else:
            return 0, Inventory()


def compute_total_balance(entries):
    """Simply sum up all the positions in the transactions in the list of entries
    and return an inventory of it."""

    total_balance = Inventory()
    for entry in entries:
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                total_balance.add_position(posting.position, allow_negative=True)
    return total_balance
