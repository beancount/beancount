"""
Realization of specific lists of account postings into reports.


FIXME here you need some ascii art


"""
import datetime
from itertools import chain, repeat
from collections import namedtuple, defaultdict
import copy

from beancount2.utils import tree_utils
from beancount2.inventory import Inventory, Position
from beancount2.parser import parser
from beancount2.data import *
from beancount2 import data


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



## FIXME: remove
# class Ledger:
#     """A class that contains a particular list of entries and an
#     associated account tree. Note: the account tree is redundant and
#     could be recalculated from the list of entries."""
#
#     def __init__(self, entries):
#
#         # A list of sorted entries in this ledger.
#         assert isinstance(entries, list)
#         # entries.sort(key=lambda x: x.date)
#         self.entries = entries
#
#         # # A tree of accounts.
#         # self.real_accounts = RealAccountTree()


class RealAccountState:
    """Per-account running state for realization."""

    def __init__(self):
        # Latest inventory balance.
        self.balance = Inventory()

        # Pad instance for the last pad seen.
        self.last_pad_entry = None

        # Insertion point of the last pad instance.
        # Points to where we'll be adding synthesized postings.
        self.index = None
        self.index_pad = None

        # A mapping of currency to the synthesized postings.
        self.padded_lots = set()



def realize(entries, check=False):
    """Realize a list of entries into a tree of realized accounts, which contains
    shadow entries with balances. This is then used to make a report.

    If 'check' is true, verify the inventory balances at the point of
    'Check' entries.

    """

    # Handle sanity checks when the check is at the beginning of the day.
    check_is_at_beginning_of_day = parser.SORT_ORDER[Check] < 0

    accounts_map = data.gather_accounts(entries)

    real_accounts = RealAccountTree(accounts_map)
    real_errors = []

    # Per-account state.
    real_states = defaultdict(RealAccountState)

    prev_date = datetime.date(1900, 1, 1)
    for entry in entries:

        if isinstance(entry, Transaction):

            # Realizing a transaction updates the balance inventory for each of
            # the accounts in its postings.
            #
            # Note: we create multiple RealPosting entries for a single
            # transaction that has multiple legs on the same account.
            for posting in entry.postings:
                balance_inventory = real_states[posting.account].balance
                try:
                    balance_inventory.add_position(posting.position)
                except ValueError as e:
                    real_errors.append(
                        RealError(entry.fileloc, str(e)))

                real_account = real_accounts.get_create(posting.account.name)
                real_account.postings.append(
                    RealPosting(entry, posting, copy.copy(balance_inventory)))

        elif isinstance(entry, Check):

# FIXME: This should insert a transaction.
#
# There's really two things going on here: padding, which really
# should just occur in the original list of entries and is a
# separate thing, and computing the global balance. We ought to
# split those two tasks: 1. enrich the list of entries, and 2.
# compute the balances. Both will be simpler, even if it means we
# have to compute the balances twice (we have to anyway).

            real_state = real_states[entry.account]
            lot = entry.position.lot
            balance_position = real_state.balance.get_position(lot)
            if entry.position != balance_position:

                # Calculate the difference.
                balance_number = ZERO if balance_position is None else balance_position.number
                diff_number = entry.position.number - balance_number
                diff_position = Position(lot, diff_number)

                # Pad if it was requested, that is, if a pad entry was already
                # seen for this account, and if it also has not already been
                # padded since then.
                pad = real_state.last_pad_entry
                assert (pad is None) or (pad.account is entry.account)
                if pad and (lot not in real_state.padded_lots):

                    # Fix the balance.
                    real_state.balance.add_position(diff_position)

                    # Create the leg that will balance this account.
                    real_account = real_accounts.get_create(pad.account.name)
                    real_posting = Posting(pad.account, diff_position, None, None)
                    real_account.postings.insert(
                        real_state.index,
                        RealPosting(pad, real_posting, real_state.balance))
                    real_state.index += 1

                    # Update the balance of the other leg of the padding.
                    other_position = diff_position.get_negative()
                    other_state = real_states[pad.account_pad]
                    other_state.balance.add_position(other_position, allow_negative=True)

                    # Create the other leg of the other account.
                    other_account = real_accounts.get_create(pad.account_pad.name)
                    other_posting = Posting(pad.account_pad, other_position, None, None)
                    other_account.postings.insert(
                        real_state.index_pad,
                        RealPosting(pad, other_posting, other_state.balance))
                    real_state.index_pad += 1

                    real_state.padded_lots.add(lot)

                else:
                    # We have no allowed padding ability; the check failed.
                    real_errors.append(
                        RealError(entry.fileloc, "Check failed for '{}': {} != {}".format(
                            entry.account.name, balance_position, entry.position)))

            # Add the check realization to the account.
            real_account = real_accounts.get_create(entry.account.name)
            real_account.postings.append(
                RealEntry(entry, copy.copy(real_state.balance)))

        elif isinstance(entry, Pad):

            # Insert the pad entry in both realized accounts.
            real_account = real_accounts.get_create(entry.account.name)
            real_account.postings.append(
                RealEntry(entry, copy.copy(real_states[entry.account].balance)))

            other_account = real_accounts.get_create(entry.account_pad.name)
            other_account.postings.append(
                RealEntry(entry, copy.copy(real_states[entry.account_pad].balance)))

            # Check and warn if the last pad entry was unused.
            real_state = real_states[entry.account]
            if real_state.last_pad_entry and not real_state.padded_lots:
                real_errors.append(
                    RealError(real_state.last_pad.fileloc, "Superfluous padding: {}".format(real_state.last_pad_entry)))

            # Save it in the running state in order to find it later.
            real_state.last_pad_entry = entry
            real_state.index = len(real_account.postings)
            real_state.index_pad = len(other_account.postings)
            real_state.padded_lots = set()

        elif isinstance(entry, (Open, Close, Note)):
            # Append some other entries in the realized list.
            real_account = real_accounts.get_create(entry.account.name)
            real_account.postings.append(
                RealEntry(entry, copy.copy(real_states[entry.account].balance)))

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
