"""
Realization of specific lists of account postings into reports.
"""
import datetime
from collections import namedtuple, defaultdict
import copy

from beancount2.utils import tree_utils
from beancount2.inventory import Inventory
from beancount2.data import Transaction, Check, Open, Pad


RealAccount = namedtuple('Account', 'name children postings')
RealPosting = namedtuple('RealPosting', 'entry posting inventory')


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




def realize(entries, check=False):
    """Realize a list of entries into a Ledger object, which contains
    placeholders with balances for each entry, and an accounts tree.
    This is then used to render a report.

    If 'check' is true, verify the inventory balances at the point of
    'Check' entries."""

    real_accounts = RealAccountTree()
    real_errors = []

    inventory_map = defaultdict(Inventory)
    prev_date = datetime.date(1900, 1, 1)
    for entry in entries:

        if isinstance(entry, Transaction):

            # Realizing a transaction updates the balance inventory for each of
            # the accounts in its postings.
            for posting in entry.postings:
                account_name = posting.account.name

                inventory = inventory_map[account_name]
                try:
                    inventory.add_position(posting.position)
                except ValueError as e:
                    real_errors.append(
                        RealError(entry.fileloc, str(e)))
                    
                real_account = real_accounts[account_name]
                real_account.postings.append(
                    RealPosting(entry, posting, copy.copy(inventory)))

        elif isinstance(entry, Check):
            # Note: the Check entries are assumed to have been sorted to be
            # before any other entries with the same date. This is done by the
            # parser. Verify this invariant here.
            assert entry.date > prev_date, (entry, prev_entry)

            inventory = inventory_map[entry.account.name]
            balance_amount = inventory.get_amount(entry.amount.currency)
            if balance_amount != entry.amount:
                real_errors.append(
                    RealError(entry.fileloc, "Check failed: {} != {}".format(balance_amount, entry.amount)))

        if not isinstance(entry, (Check, Open)):
            prev_entry = entry
            prev_date = entry.date

    # FIXME: We need to add auto padding here maybe?

    return (real_accounts, real_errors)
