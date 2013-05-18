"""
Realization of specific lists of account postings into reports.
"""
from collections import namedtuple, defaultdict
from beancount2.utils import tree_utils
from beancount2.inventory import Inventory
from beancount2.data import Transaction


RealAccount = namedtuple('Account', 'name children postings')
RealPosting = namedtuple('RealPosting', 'entry posting inventory')


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




def realize(entries):
    """Realize a list of entries into a Ledger object, which contains
    placeholders with balances for each entry, and an accounts tree.
    This is then used to render a report."""

    real_accounts = RealAccountTree()

    inventory_map = defaultdict(Inventory)
    for entry in entries:
        if isinstance(entry, Transaction):
            # Realizing a transaction updates the balance inventory for that entry.

            for posting in entry.postings:
                account_name = posting.account.name

                inventory = inventory_map[account_name]
                inventory.add_position(posting.position)

                real_account = real_accounts[account_name]
                real_account.postings.append(
                    RealPosting(entry, posting, inventory.copy()))

    return real_accounts
