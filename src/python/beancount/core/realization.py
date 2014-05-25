"""Realization of specific lists of account postings into reports.
"""
import sys
from itertools import chain, repeat
from collections import OrderedDict
import operator
import copy

from beancount.core import inventory
from beancount.core.amount import amount_sortkey
from beancount.utils import misc_utils
from beancount.core import data
from beancount.core.data import Transaction, Balance, Open, Close, Pad, Note, Document
from beancount.core.data import Posting
from beancount.core.account import account_name_leaf, account_name_parent
from beancount.core import account
from beancount.utils import tree_utils


__plan__ = """
X  fullname       -> renames to .account, which is the full name of the account, we never need the leaf name
X  balance        -> (stays the same.)
X  postings       -> (stays the same.)
X  children       -> becomes .values() from dict interface
X  asdict()       -> becomes the object itself, not really needed anymore.
X  __getitem__    -> should work as keys(), not deep. Deep uses go to get_deep_item()
X  add(account)   -> inserts leaf name automatically, not needed anymore, only used here.
X  __iter__       -> delete, just becomes keys() like dict
__contains__   -> remove support for recursive, onyl work on direct children, use

X  get_children() -> values()


Note: maybe these are all module functions with simple names...
  realization.get(key, None) instead.
  realization.set(real_account): automatically creates the intervening nodes
  realization.contains(account_name)
  realization.iter(real_account)

values_recursively -> provide a function to do this instead


get_deep(key, default): Work down the tree at multiple levels, convert get_in().
If key does not contain sep, should still work as usual with just children.
Needs to have a default value in order to stand in for __contains__ iwth depth
below.


tree_utils should be converted to work on dicts of dicts just like this one, or maybe removed.
"""

class RealAccount(dict):
    """A realized account, inserted in a tree, that contains the list of realized entries.

    Attributes:
      account: A string, the full name of the corresponding account.
      postings: A list of postings associated with this accounting (does not
        include the postings of children accounts).
      balance: The final balance of the list of postings associated with this account.
    """
    __slots__ = ('account', 'postings', 'balance')

    def __init__(self, account_name, *args, **kwargs):
        """Create a RealAccount instance.

        Args:
          account_name: a string, the name of the account. Maybe not be None.
        """
        dict.__init__(self, *args, **kwargs)
        assert isinstance(account_name, str)
        self.account = account_name
        self.postings = []
        self.balance = inventory.Inventory()

    # def __str__(self):
    #     """Return a human-readable string representation of this RealAccount.
    #
    #     Returns:
    #       A readable string, with the name and the balance.
    #     """
    #     return "RealAccount({}){}".format(self.account,
    #                                       dict.__str__(self))

    # def __iter__(self):
    #     # Disable implicit iteration for now.
    #     raise NotImplementedError






UNSET = object()

def get(real_account, subaccount_name, default=UNSET):
    """Fetch the account name from the real_account node.

    Args:
      subaccount_name: A string, the name of a possibly indirect child leaf
      found down within this dict..
    Returns:
      A RealAccount instance for the child, or the default value, if one
      is specified.
    Raises:
      KeyError: If the child is not found.
    """
    assert isinstance(childname, str), childname
    if not childname:
        return self
    if account.sep in childname:
        directname = childname.split(account.sep, 1)[0]
        restname = childname[len(directname)+1:]
        child = self.children[directname]
        return child[restname]
    else:
        return self.children[childname]






    # # FIXME: Do I really need to support more than direct children here? Remove
    # # if possible before 2.0 release.
    # def __contains__(self, account_name_leaf):
    #     """True if the given string is in this RealAccount instance.
    #
    #     Args:
    #       account_name: A string, the leaf name of a child account of this node, or
    #         of a more distant child of this node.
    #     Returns:
    #       A boolean, true the name is a child of this node.
    #     """
    #     directname = account_name_leaf.split(account.sep, 1)[0]
    #     restname = account_name_leaf[len(directname)+1:]
    #     try:
    #         child = self.children[directname]
    #         if restname:
    #             return child.__contains__(restname)
    #         else:
    #             return True
    #     except KeyError:
    #         return False




# FIXME: make this work
def values_recursively(self):
    """Iterate this account node and all its children, depth-first.

    Yields:
      Instances of RealAccount, beginning with this account. The order is
      undetermined.
    """
    yield self
    for child in self.children.values():
        for subchild in child.values_recursively():
            yield subchild




# FIXME: Can you remove 'min_accounts' and move that to a second step.
# (This is only called from views, and only for root accounts. Do it, simplify.)
def realize(entries):
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
                              +---------+
                              | Balance |
                              +---------+
                                   |
                                   v
                               +-------+
                               | Close |
                               +-------+
                                   |
                                   .

    Args:
      entries: A list of directive instances.
    Returns:
      The root RealAccount instance.
    """

    # FIXME: Simplify this here by creating RealAccount from a defaultdict and
    # set the names in a second stage.

    real_dict = {}
    for entry in entries:

        if isinstance(entry, Transaction):
            # Update the balance inventory for each of the postings' accounts.
            for posting in entry.postings:
                real_account = append_entry_to_real_account(real_dict,
                                                            posting.account, posting)
                real_account.balance.add_position(posting.position, allow_negative=True)

        elif isinstance(entry, (Open, Close, Balance, Note, Document)):
            # Append some other entries in the realized list.
            append_entry_to_real_account(real_dict, entry.account, entry)

        elif isinstance(entry, Pad):
            # Insert the pad entry in both realized accounts.
            append_entry_to_real_account(real_dict, entry.account, entry)
            append_entry_to_real_account(real_dict, entry.account_pad, entry)

    return create_real_accounts_tree(real_dict)


def ensure_min_accounts(real_account, min_accounts):
    """Ensure that the given accounts are added to the given realization.

    Args:
      real_account: An instance of RealAccount, the root account.
      min_accounts: An list of child account names to ensure exist as child
        of this account.
    Returns:
      The real_account instance is returned modified.
    """
    # Ensure the minimal list of accounts has been created.
    for account_name in min_accounts:
        if account_name not in real_account:
            real_account.add(RealAccount(account_name))
    return real_account


# FIXME: You can remove this method, use a defaultdict(account_name -> postings-list)
# in the caller and get rid of this call.
# Compute the balances in a second step.
def append_entry_to_real_account(real_dict, account_name, entry):
    """Append an account's posting to its corresponding account in the real_dict
    dictionary. The relevance of this method is that it creates RealAccount
    instances on-demand.

    Args:
      real_dict: A dictionary of full account name to RealAccount instance.
      account_name: A string, the account name to associate the entry to.
    Returns:
      The RealAccount instance that this entry was associated with.
    """
    # Create the account, if not already there.
    try:
        real_account = real_dict[account_name]
    except KeyError:
        real_account = RealAccount(account_name)
        real_dict[account_name] = real_account

    # If specified, add the new entry to the list of postings.
    assert entry is not None
    real_account.postings.append(entry)

    return real_account


def create_real_accounts_tree(real_dict):
    """Create a full tree of realized accounts from leaf nodes.

    Args:
      real_dict: a dict of of name to RealAccount instances.
    Returns:
      A RealAccount instance, the root node that contains the full tree
      of RealAccount instances implied by the nodes given as input.
    """
    assert isinstance(real_dict, dict)
    full_dict = real_dict.copy()

    # Create root account (in case the real_dict is empty).
    root_account = RealAccount('')
    full_dict[''] = root_account

    for real_account in real_dict.values():
        while True:
            parent_name = account_name_parent(real_account.fullname)
            if parent_name is None:
                break
            try:
                parent_account = full_dict[parent_name]
            except KeyError:
                parent_account = RealAccount(parent_name)
                full_dict[parent_name] = parent_account

            parent_account.add(real_account)
            real_account = parent_account

    # Return the root account node.
    return root_account


def filter_tree(real_account, predicate):
    """Visit the tree and apply the predicate on each node;
    return a mapping of nodes where the predicate was true,
    and of nodes which has children with the predicate true as well.
    """
    assert isinstance(real_account, RealAccount)

    children_copy = OrderedDict()
    for child in real_account.get_children():
        child_copy = filter_tree(child, predicate)
        if child_copy is not None:
            leafname = account_name_leaf(child.fullname)
            children_copy[leafname] = child_copy

    if children_copy or predicate(real_account):
        real_account_copy = copy.copy(real_account)
        real_account_copy.children = children_copy
        return real_account_copy


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
    for child_account in real_account.get_children():
        _get_subpostings(child_account, accumulator)


def dump_tree_balances(real_account, foutput=None):
    """Dump a simple tree of the account balances at cost, for debugging."""

    if foutput is None:
        foutput = sys.stdout

    lines = list(tree_utils.render(
        real_account,
        lambda ra: ra.fullname.split(account.sep)[-1],
        lambda ra: sorted(ra.get_children(), key=lambda x: x.fullname)))
    if not lines:
        return
    width = max(len(line[0] + line[2]) for line in lines)

    for line_first, line_next, account_name, real_account in lines:
        last_entry = real_account.postings[-1] if real_account.postings else None
        balance = getattr(real_account, 'balance', None)
        if not balance.is_empty():
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
    real1 = copy.copy(real_accounts1)
    real2 = copy.copy(real_accounts2)
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
    return {real_account.fullaname: str(real_account.balance.get_cost())
            for account_name, real_account in real_accounts.items()
            if real_account.fullname}


def iterate_with_balance(postings_or_entries):
    """Iterate over the entries accumulating the balance.
    For each entry, it yields

      (entry, change, balance)

    'entry' is the entry for this line. If the list contained Posting instance,
    this yields the corresponding Transaction object.

    'change' is an Inventory object that reflects the change due to this entry
    (this may be multiple positions in the case that a single transaction has
    multiple legs).

    The 'balance' yielded is never None; it's up to the one displaying the entry
    to decide whether to render for a particular type.

    Also, multiple postings for the same transaction are de-duped
    and when a Posting is encountered, the parent Transaction entry is yielded,
    with the balance updated for just the postings that were in the list.
    (We attempt to preserve the original ordering of the postings as much as
    possible.)
    """

    # The running balance.
    balance = inventory.Inventory()

    # Previous date.
    prev_date = None

    # A list of entries at the current date.
    date_entries = []

    first = lambda pair: pair[0]
    for entry in postings_or_entries:

        # Get the posting if we are dealing with one.
        if isinstance(entry, Posting):
            posting = entry
            entry = posting.entry
        else:
            posting = None

        if entry.date != prev_date:
            prev_date = entry.date

            # Flush the dated entries.
            for date_entry, date_postings in date_entries:
                change = inventory.Inventory()
                if date_postings:
                    # Compute the change due to this transaction and update the
                    # total balance at the same time.
                    for date_posting in date_postings:
                        change.add_position(date_posting.position, True)
                        balance.add_position(date_posting.position, True)
                yield date_entry, date_postings, change, balance

            date_entries.clear()
            assert not date_entries

        if posting is not None:
            # De-dup multiple postings on the same transaction entry by
            # grouping their positions together.
            index = misc_utils.index_key(date_entries, entry, first, operator.is_)
            if index is None:
                date_entries.append((entry, [posting]))
            else:
                # We are indeed de-duping!
                postings = date_entries[index][1]
                postings.append(posting)
        else:
            # This is a regular entry; nothing to add/remove.
            date_entries.append((entry, []))

    # Flush the final dated entries if any, same as above.
    for date_entry, date_postings in date_entries:
        change = inventory.Inventory()
        if date_postings:
            for date_posting in date_postings:
                change.add_position(date_posting.position, True)
                balance.add_position(date_posting.position, True)
        yield date_entry, date_postings, change, balance
    date_entries.clear()


# FIXME: TODO, implement these properly.

def reorder_accounts_tree(real_accounts):
    """Reorder the children in a way that is sensible for display.
    We want the most active accounts near the top, and the least
    important ones at the bottom."""

    reorder_accounts_node_by_declaration(real_accounts.get_root())


def reorder_accounts_node_by_declaration(real_account):

    children = []
    for child_account in real_account.children:
        fileloc = reorder_accounts_node_by_declaration(child_account)
        children.append((fileloc, child_account))

    children.sort()
    real_account.children[:] = [x[1] for x in children]

    print(real_account.fullname)
    for fileloc, child in children:
        print('  {:64}  {}'.format(child.name, fileloc))
    print()

    if real_account.postings:
        fileloc = real_account.postings[0].fileloc
    else:
        fileloc = children[0][0]
    return fileloc



def reorder_accounts_node_by_date(real_account):

    children = []
    for child_account in real_account.children:
        reorder_accounts_node_by_date(child_account)
        children.append((child_date, child_account))
    children.sort(reverse=True)

    real_account.children[:] = [x[1] for x in children]

    last_date = children[0][0] if children else datetime.date(1970, 1, 1)

    if real_account.postings:
        last_posting = real_account.postings[-1]
        if hasattr(last_posting, 'date'):
            date = last_posting.date
        else:
            date = last_posting.entry.date

        if date > last_date:
            last_date = date

    return last_date





# FIXME: This file is being cleaned up. Don't worry about all the FIXMEs [2014-02-26]
