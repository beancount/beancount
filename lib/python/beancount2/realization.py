"""
Realization of specific lists of account postings into reports.
"""
import datetime
from collections import namedtuple, defaultdict
import copy

from beancount2.utils import tree_utils
from beancount2.inventory import Inventory, Position
from beancount2.parser import parser
from beancount2.data import *


# A realized account, inserted in a tree, that contains the list of realized
# entries.
RealAccount = namedtuple('RealAccount', 'name children postings')

# A realized posting, that points to a particular posting of transaction entry.
RealPosting = namedtuple('RealPosting', 'entry posting balance')

# A synthesized padding entry, with a synthesized posting, that points to the
# padding entry that triggered it.
RealPadPosting = namedtuple('RealPadPosting', 'entry posting balance')

# A realized check, that contains the actual balance that was seen at that
# point.
RealCheck = namedtuple('RealCheck', 'entry balance')


RealError = namedtuple('RealError', 'fileloc message')


class RealAccountTree(tree_utils.TreeDict):
    """A container for a hierarchy of accounts, that can conveniently
    create and maintain a hierarchy of accounts."""

    def create_node(self, account_name):
        return RealAccount(account_name, [], [])

    def get_name(self, real_account):
       return real_account.name.split(':')[-1]

    def get_children(self, real_account):
        return real_account.children

    def __init__(self):
        tree_utils.TreeDict.__init__(self, self, ':')



class Ledger:
    """A class that contains a particular list of entries and an
    associated account tree. Note: the account tree is redundant and
    could be recalculated from the list of entries."""

    def __init__(self, entries):

        # A list of sorted entries in this ledger.
        assert isinstance(entries, list)
        # entries.sort(key=lambda x: x.date)
        self.entries = entries

        # # A tree of accounts.
        # self.real_accounts = RealAccountTree()




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
        self.padded_currencies = {}



def realize(entries, check=False):
    """Realize a list of entries into a Ledger object, which contains
    placeholders with balances for each entry, and an accounts tree.
    This is then used to render a report.

    If 'check' is true, verify the inventory balances at the point of
    'Check' entries."""

    # Handle sanity checks when the check is at the beginning of the day.
    check_is_at_beginning_of_day = parser.SORT_ORDER[Check] < 0

    real_accounts = RealAccountTree()
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

                real_account = real_accounts[posting.account.name]
                real_account.postings.append(
                    RealPosting(entry, posting, copy.copy(balance_inventory)))

        elif isinstance(entry, Check):

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
                if pad and (lot.currency not in real_state.padded_currencies):

                    # Fix the balance.
                    real_state.balance.add_position(diff_position)

                    # Create the leg that will balance this account.
                    real_account = real_accounts[pad.account.name]
                    real_posting = Posting(pad.account, diff_position, None, None)
                    real_account.postings.insert(
                        real_state.index,
                        RealPadPosting(pad, real_posting, real_state.balance))
                    real_state.index += 1

                    # Update the balance of the other leg of the padding.
                    other_position = diff_position.get_negative()
                    other_state = real_states[pad.account_pad]
                    other_state.balance.add_position(other_position, allow_negative=True)

                    # Create the other leg of the other account.
                    other_account = real_accounts[pad.account_pad.name]
                    other_posting = Posting(pad.account_pad, other_position, None, None)
                    other_account.postings.insert(
                        real_state.index_pad,
                        RealPadPosting(pad, other_posting, other_state.balance))
                    real_state.index_pad += 1

                else:
                    # We have no allowed padding ability; the check failed.
                    real_errors.append(
                        RealError(entry.fileloc, "Check failed for '{}': {} != {}".format(
                            entry.account.name, balance_position, entry.position)))

            # Add the check realization to the account.
            real_account = real_accounts[entry.account.name]
            real_account.postings.append(
                RealCheck(entry, copy.copy(real_state.balance)))

        elif isinstance(entry, Pad):

            # Insert the pad entry in both realized accounts.
            real_account = real_accounts[entry.account.name]
            real_account.postings.append(entry)

            other_account = real_accounts[entry.account_pad.name]
            other_account.postings.append(entry)

            # Check and warn if the last pad entry was unused.
            real_state = real_states[entry.account]
            if real_state.last_pad_entry and not real_state.padded_currencies:
                real_errors.append(
                    RealError(real_state.last_pad.fileloc, "Superfluous padding: {}".format(real_state.last_pad_entry)))

            # Save it in the running state in order to find it later.
            real_state.last_pad_entry = entry
            real_state.index = len(real_account.postings)
            real_state.index_pad = len(other_account.postings)

        elif isinstance(entry, (Open, Close, Note)):
            # Append some other entries in the realized list.
            real_account = real_accounts[entry.account.name]
            real_account.postings.append(entry)

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


def dump_tree_balances(real_accounts, foutput):
    """Dump a simple tree of the account balances, for debugging."""
    lines = list(real_accounts.render_lines())
    width = max(len(line[0]) for line in lines)
    for line, real_account in lines:
        last_entry = real_account.postings[-1] if real_account.postings else None
        balance = getattr(last_entry, 'balance', '')
        foutput.write('{:{width}}    {}\n'.format(line, balance, width=width))
