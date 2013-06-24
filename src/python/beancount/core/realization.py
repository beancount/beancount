"""Realization of specific lists of account postings into reports.
"""
import sys
import datetime
from itertools import chain, repeat
from collections import namedtuple, defaultdict

from beancount.utils import tree_utils
from beancount.core.inventory import Inventory
from beancount.core.amount import amount_sortkey
from beancount.core import data
from beancount.core.data import Transaction, Check, Open, Close, Pad, Note, Document


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

        elif isinstance(entry, (Open, Close, Check, Note, Document)):

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
    """Dump a simple tree of the account balances at cost, for debugging."""

    if foutput is None:
        foutput = sys.stdout

    lines = list(real_accounts.render_lines())
    width = max(len(line[0] + line[2]) for line in lines)

    for line_first, line_next, account_name, real_account in lines:
        last_entry = real_account.postings[-1] if real_account.postings else None
        balance = getattr(real_account, 'balance', None)
        if balance:
            amounts = balance.get_cost().get_amounts()
            positions = ['{0.number:12,.2f} {0.currency}'.format(amount)
                         for amount in sorted(amounts, key=amount_sortkey)]
        else:
            positions = ['']

        for position, line in zip(positions, chain((line_first + account_name,),
                                                   repeat(line_next))):
            foutput.write('{:{width}}   {:16}\n'.format(line, position, width=width))


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




# FIXME: I don't think I need this at all?

# def compute_real_total_balance(real_accounts):
#     """Sum up all the positions in the transactions in the realized tree of accounts
#     and return an inventory of it."""
#     total_balance = Inventory()
#     for real_account in real_accounts.values():
#         if real_account.postings:
#             balance = real_account.balance
#             total_balance += balance
#     return total_balance
