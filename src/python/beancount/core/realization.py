"""Realization of specific lists of account postings into reports.
"""
import sys
from itertools import chain, repeat
from collections import OrderedDict
import operator
import copy

from beancount.core.inventory import Inventory
from beancount.core.amount import amount_sortkey
from beancount.utils import index_key
from beancount.core import data
from beancount.core import getters
from beancount.core.data import Transaction, Balance, Open, Close, Pad, Note, Document
from beancount.core.data import Posting
from beancount.core.account import account_name_leaf, account_name_parent


class RealAccount:
    """A realized account, inserted in a tree, that contains the list of realized entries.

    Attributes:
      fullname: A string, the full name of the corresponding account.
      leafname: A string, the name of the leaf of the account.
      account:  An Account instance
      balance: The final balance of the list of postings associated with this account.
      children: A dict of children names to children RealAccount instances.
      postings: A list of postings associated with this accounting (does not include
        the postings of children accounts).
    """

    __slots__ = 'fullname leafname account balance children postings'.split()

    # FIXME: This is a stupid constructor, ditch the Account class and use just the string here.r
    def __init__(self, account_name, account):
        """Create a RealAccount instance.

        Args:
          account_name: a string, which may be empty (but never None).
          account: an Account instance, which may be None if there is no associated
                   account to this node.
        Returns:
          A new RealAccount instance.
        """
        if account:
            # Make sure to use the same string instance.
            assert account.name == account_name, (account_name, account)
            account_name = account.name

        self.fullname = account_name
        self.leafname = account_name_leaf(account_name)
        self.account = account

        self.balance = Inventory()
        self.children = OrderedDict()
        self.postings = []

    def __str__(self):
        """Return a human-readable string representation of this RealAccount.

        Returns:
          A readable string, with the name and the balance.
        """
        return "<RealAccount '{}' {}>".format(self.fullname, self.balance)

    def __getitem__(self, childname):
        """Return a child account from this account.

        Args:
          childname: A string, the name of the child of this account.
        Returns:
          A RealAccount instance for the child.
        """
        if not childname:
            return self
        if ':' in childname:
            directname = childname.split(':', 1)[0]
            restname = childname[len(directname)+1:]
            child = self.children[directname]
            return child[restname]
        else:
            return self.children[childname]

    # FIXME: During cleanup, swap these two method names with appropriate renames.
    def __iter__(self):
        """Iterate this account node and all its children, depth-first.

        Yields:
          Instances of RealAccount, beginning with this account. The order is
          undetermined.
        """
        yield self
        for child in self.children.values():
            yield from child

    def get_children(self):
        """Get the children of this node.

        Returns:
          An iterator of RealAccount instances.
        """
        return self.children.values()

    # FIXME: Do I really need to support more than direct children here? Remove if possible before 2.0 release.
    def __contains__(self, account_leaf_name):
        """True if the given string is in this RealAccount instance.

        Args:
          account_name: A string, the leaf name of a child account of this node, or
            of a more distant child of this node.
        Returns:
          A boolean, true the name is a child of this node.
        """
        directname = account_leaf_name.split(':', 1)[0]
        restname = account_leaf_name[len(directname)+1:]
        try:
            child = self.children[directname]
            if restname:
                return child.__contains__(restname)
            else:
                return True
        except KeyError:
            return False

    def add(self, real_account):
        """Add a new child to this real account.

        Args:
          real_account: An instance of RealAccount to add as a child to this node.
        """
        ## FIXME: When would this trigger and why?
        ##assert real_account.leafname not in self.children
        self.children[real_account.leafname] = real_account


# FIXME: Can you remove 'min_accounts' and move that to a second step.
# (This is only called from views, and only for root accounts. Do it, simplify.)
def realize(entries, min_accounts=None):
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

    'min_accounts' provides a sequence of accounts to ensure that we create no matter
    what, even if empty. This is typically used for the root accounts.

    Args:
      entries: A list of directive instances.
      min_accounts: An optional list of child account names to ensure exist as child
        of this account.
    Returns:
      The root RealAccount instance.
    """
    real_dict = {}
    for entry in entries:

        if isinstance(entry, Transaction):
            # Update the balance inventory for each of the postings' accounts.
            for posting in entry.postings:
                real_account = append_entry_to_real_account(real_dict, posting.account, posting)
                real_account.balance.add_position(posting.position, allow_negative=True)

        elif isinstance(entry, (Open, Close, Balance, Note, Document)):
            # Append some other entries in the realized list.
            append_entry_to_real_account(real_dict, entry.account, entry)

        elif isinstance(entry, Pad):
            # Insert the pad entry in both realized accounts.
            append_entry_to_real_account(real_dict, entry.account, entry)
            append_entry_to_real_account(real_dict, entry.account_pad, entry)

    # Ensure the minimal list of accounts has been created.
    if min_accounts:
        for account_name in min_accounts:
            if account_name not in real_dict:
                real_dict[account_name] = RealAccount(account_name, None)

    return create_real_accounts_tree(real_dict)


# FIXME: You can remove this method, use a defaultdict(account_name -> postings-list)
# in the caller and get rid of this call.
# Compute the balances in a second step.
def append_entry_to_real_account(real_dict, account, entry):
    """Append an account's posting to its corresponding account in the real_dict
    dictionary. The relevance of this method is that it creates RealAccount
    instances on-demand.

    Args:
      real_dict: A dictionary of full account name to RealAccount instance.
      account: An instance of Account to associate the entry to.
    Returns:
      The RealAccount instance that this entry was associated with.
    """

    # Create the account, if not already there.
    try:
        real_account = real_dict[account.name]
    except KeyError:
        real_account = RealAccount(account.name, account)
        real_dict[account.name] = real_account

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
    for real_account in real_dict.values():
        while True:
            parent_name = account_name_parent(real_account.fullname)
            if parent_name is None:
                break
            try:
                parent_account = full_dict[parent_name]
            except KeyError:
                parent_account = RealAccount(parent_name, None)
                full_dict[parent_name] = parent_account

            parent_account.add(real_account)
            real_account = parent_account

    # Return the root node.
    return full_dict['']


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
            children_copy[child.leafname] = child_copy

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
            if real_account.account}


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
    balance = Inventory()

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
                if date_postings:
                    # Compute the change due to this transaction and update the
                    # total balance at the same time.
                    change = Inventory()
                    for date_posting in date_postings:
                        change.add_position(date_posting.position, True)
                        balance.add_position(date_posting.position, True)
                else:
                    change = None
                yield date_entry, date_postings, change, balance

            date_entries.clear()
            assert not date_entries

        if posting is not None:
            # De-dup multiple postings on the same transaction entry by
            # grouping their positions together.
            index = index_key(date_entries, entry, first, operator.is_)
            if index is None:
                date_entries.append( (entry, [posting]) )
            else:
                # We are indeed de-duping!
                postings = date_entries[index][1]
                postings.append(posting)
        else:
            # This is a regular entry; nothing to add/remove.
            date_entries.append( (entry, []) )

    # Flush the final dated entries if any, same as above.
    for date_entry, date_postings in date_entries:
        if date_postings:
            change = Inventory()
            for date_posting in date_postings:
                change.add_position(date_posting.position, True)
                balance.add_position(date_posting.position, True)
        else:
            change = None
        yield date_entry, date_postings, change, balance
    date_entries.clear()



# FIXME: This file is being cleaned up. Don't worry about all the FIXMEs [2014-02-26]
