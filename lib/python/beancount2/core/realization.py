"""Realization of specific lists of account postings into reports.
"""
import sys
import datetime
from itertools import chain, repeat
from collections import namedtuple, defaultdict
from copy import copy
import collections

from beancount2.utils import tree_utils
from beancount2.utils.bisect_key import bisect_left_withkey
from beancount2.core.inventory import Inventory, Position
from beancount2.core.data import *
from beancount2.core import data
from beancount2 import utils


# A realized account, inserted in a tree, that contains the list of realized
# entries.
RealAccount = namedtuple('RealAccount', 'name account balance children postings')



class RealAccountTree(tree_utils.TreeDict):
    """A container for a hierarchy of accounts, that can conveniently
    create and maintain a hierarchy of accounts."""

    def __init__(self, accounts_map):
        self.accounts_map = accounts_map
        tree_utils.TreeDict.__init__(self, self, ':')

    def create_node(self, account_name):
        account = self.accounts_map.get(account_name)
        return RealAccount(account_name, account, Inventory(), [], [])

    def get_name(self, real_account):
       return real_account.name.split(':')[-1]

    def get_children(self, real_account):
        return real_account.children


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

        elif isinstance(entry, (Check, Open, Close, Pad, Note)):
            if (only_accounts is not None and
                entry.account not in only_accounts):
                continue
            by_accounts[entry.account].append(entry)

    return by_accounts


## FIXME: This doesn't really belong in realization anymore; more this somewhere else.

PadError = namedtuple('PadError', 'fileloc message')

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

            elif isinstance(entry, Check):
                check_amount = entry.amount

                # Compare the current balance amount to the expected one from
                # the check entry. IMPORTANT: You need to understand that this
                # does not check a single position, but rather checks that the
                # total amount for a particular currency (which itself is
                # distinct from the cost).
                balance_amount = balance.get_amount(check_amount.currency)
                diff_amount = amount_sub(balance_amount, check_amount)
                if diff_amount.number.abs() > CHECK_PRECISION:
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
                                    PadError(entry.fileloc, "Attempt to pad an entry with cost for balance: {}".format(balance)))

                        # Thus our padding lot is without cost by default.
                        lot = Lot(check_amount.currency, None, None)
                        diff_position = Position(lot, check_amount.number - balance_amount.number)

                        # Synthesize a new transaction entry for the difference.
                        narration = '(Padding inserted for Check of {} for difference {})'.format(
                            check_amount, diff_position)
                        new_entry = Transaction(
                            active_pad.fileloc, active_pad.date, FLAG_PADDING, None, narration, set(), [])

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
                    PadError(entry.fileloc, "Unused Pad entry: {}".format(pad)))

    return padded_entries, pad_errors




## FIXME: Move this to validation.

CheckError = namedtuple('CheckError', 'fileloc message')

CHECK_PRECISION = Decimal('.001')

def check(entries):
    """Check for all the Check directives and replace failing ones by new ones with
    a flag that indicates failure."""

    check_errors = []

    # Running balance for each account.
    balances = defaultdict(Inventory)

    new_entries = []
    for entry in entries:

        if isinstance(entry, Transaction):
            # Update the balance inventory for each of the postings' accounts.
            for posting in entry.postings:
                balance = balances[posting.account]
                try:
                    # Note: if this is from a padding transaction, we allow negative lots at cost.
                    allow_negative = entry.flag in (FLAG_PADDING, FLAG_SUMMARIZE)
                    balance.add_position(posting.position, allow_negative)
                except ValueError as e:
                    check_errors.append(
                        CheckError(entry.fileloc, "Error balancing '{}' -- {}".format(posting.account.name, e)))

        elif isinstance(entry, Check):
            # Check the balance against the check entry.
            check_amount = entry.amount
            balance = balances[entry.account]
            balance_amount = balance.get_amount(check_amount.currency)
            diff_amount = amount_sub(balance_amount, check_amount)
            if diff_amount.number.abs() > CHECK_PRECISION:
                check_errors.append(
                    CheckError(entry.fileloc, "Check failed for '{}': {} != {}".format(
                        entry.account.name, balance_amount, check_amount)))

                # Substitute the entry by a failing entry.
                entry = Check(entry.fileloc, entry.date, entry.account, entry.amount, diff_amount)

        new_entries.append(entry)

    return new_entries, check_errors




def realize(entries, do_check=False, min_accounts=None):
    """Group entries by account, into a "tree" of realized accounts. RealAccount's
    are essentially containers for lists of postings and the final balance of
    each account, and may be non-leaf accounts (used strictly for organizing
    accounts into a hierarchy). This is then used to issue reports.

    The lists of postings in each account my be any of the entry types, except
    for Transaction, whereby Transaction entries are replaced by the specific
    Posting legs that belong to the account. Here's a simple diagram that
    summarizes this seemingly complex, but rather simple data structure:

       +-------------+         +------+
       | RealAccount |---------| Open |
       +-------------+         +------+
                                   |
                                   v
                              +---------+     +-------------+
                              | Posting |---->| Transaction |
                              +---------+     +-------------+
                                   |                         \
                                   |                       +---------+
                                   |                       | Posting |
                                   v                       +---------+
                                +-----+
                                | Pad |
                                +-----+
                                   |
                                   v
                               +-------+
                               | Check |
                               +-------+
                                   |
                                   v
                               +-------+
                               | Close |
                               +-------+
                                   |
                                   .

    If 'do_check' is true, verify that Check entry balances succeed and issue error
    messages if they fail.

    'min_accounts' provides a sequence of accounts to ensure that we create no matter
    what, even if empty. This is typically used for the root accounts.
    """

    accounts_map = data.gather_accounts(entries)
    real_accounts = RealAccountTree(accounts_map)

    # Ensure the minimal list of accounts has been created.
    if min_accounts:
      for account_name in min_accounts:
        real_accounts.get_create(account_name)

    def add_to_account(account, entry):
        "Update an account's posting list with the given entry."
        real_account = real_accounts.get_create(account.name)
        real_account.postings.append(entry)

    # Running balance for each account.
    balances = defaultdict(Inventory)

    prev_date = datetime.date(1900, 1, 1)
    for entry in entries:

        if isinstance(entry, Transaction):

            # Update the balance inventory for each of the postings' accounts.
            for posting in entry.postings:
                balance = balances[posting.account]
                balance.add_position(posting.position, allow_negative=True)

                add_to_account(posting.account, posting)

        elif isinstance(entry, (Open, Close, Check, Note)):

            # Append some other entries in the realized list.
            add_to_account(entry.account, entry)

        elif isinstance(entry, Pad):

            # Insert the pad entry in both realized accounts.
            add_to_account(entry.account, entry)
            add_to_account(entry.account_pad, entry)

    # Create a tree with updated balances.
    for account, balance in balances.items():
        real_account = real_accounts.get(account.name)
        real_account.balance.update(balance)

    return real_accounts


def get_subpostings(real_account):
    """Given a RealAccount instance, return a sorted list of all its postings and
    the postings of its child accounts."""

    accumulator = []
    _get_subpostings(real_account, accumulator)
    accumulator.sort(key=data.posting_sortkey)
    return accumulator

def _get_subpostings(real_account, accumulator):
    "Internal recursive routine to get all the child postings."
    accumulator.extend(real_account.postings)
    for child_account in real_account.children:
        _get_subpostings(child_account, accumulator)


def dump_tree_balances(real_accounts, foutput=None):
    """Dump a simple tree of the account balances, for debugging."""

    if foutput is None:
        foutput = sys.stdout

    lines = list(real_accounts.render_lines())
    width = max(len(line[0]) for line in lines)
    for line_first, line_next, account_name, real_account in lines:
        last_entry = real_account.postings[-1] if real_account.postings else None
        balance = getattr(last_entry, 'balance', None)
        if balance:
            amounts = balance.get_cost().get_amounts()
            positions = ['{0.number:12,.2f} {0.currency}'.format(amount)
                         for amount in sorted(amounts, key=amount_sortkey)]
        else:
            positions = ['']
        for position, line in zip(positions, chain((line_first + account_name,),
                                                   repeat(line_next))):
            foutput.write('{:{width}}   {:16}\n'.format(line, position, width=width))


def compute_real_total_balance(real_accounts):
    """Sum up all the positions in the transactions in the realized tree of accounts
    and return an inventory of it."""

    total_balance = Inventory()
    for real_account in real_accounts.values():
        if real_account.postings:
            balance = real_account.balance
            total_balance += balance
    return total_balance


def compare_realizations(real_accounts1, real_accounts2):
    """Compare two realizations; return True if the balances are equal
    for all accounts."""
    real1 = real_accounts1.copy()
    real2 = real_accounts2.copy()
    for account_name, real_account1 in real1.items():
        real_account2 = real2.pop(account_name)
        balance1 = real_account1.balance
        balance2 = real_account2.balance
        if balance1 != balance2:
            return False
    return True


def real_cost_as_dict(real_accounts):
    """Convert a tree of real accounts as a dict for easily doing
    comparisons for testing."""
    return {real_account.name: str(real_account.balance.get_cost())
            for account_name, real_account in real_accounts.items()
            if real_account.account}








# FIXME: move this to next to sum_to_date in realize.py

def compute_total_balance(entries):
    """Sum up all the positions in the transactions in the list of entries and
    return an inventory of it."""

    total_balance = Inventory()
    for entry in entries:
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                total_balance.add_position(posting.position, allow_negative=True)
    return total_balance
