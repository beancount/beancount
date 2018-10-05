"""Realization of specific lists of account postings into reports.

This code converts a list of entries into a tree of RealAccount nodes (which
stands for "realized accounts"). The RealAccount objects contain lists of
Posting instances instead of Transactions, or other entry types that are
attached to an account, such as a Balance or Note entry.

The interface of RealAccount corresponds to that of a regular Python dict, where
the keys are the names of the individual components of an account's name, and
the values are always other RealAccount instances. If you want to get an account
by long account name, there are helper functions in this module for this purpose
(see realization.get(), for instance). RealAccount instances also contain the
final balance of that account, resulting from its list of postings.

You should not build RealAccount trees yourself; instead, you should filter the
list of desired directives to display and call the realize() function with them.
"""
__copyright__ = "Copyright (C) 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"

import io
import collections
import operator
import copy

from beancount.core.data import Transaction
from beancount.core.data import Posting
from beancount.core.data import TxnPosting
from beancount.core.data import Balance
from beancount.core.data import Open
from beancount.core.data import Close
from beancount.core.data import Pad
from beancount.core.data import Note
from beancount.core.data import Document
from beancount.core.data import Custom
from beancount.core import inventory
from beancount.core import amount
from beancount.core import data
from beancount.core import account
from beancount.core import flags
from beancount.core import convert


class RealAccount(dict):
    """A realized account, inserted in a tree, that contains the list of realized entries.

    Attributes:
      account: A string, the full name of the corresponding account.
      postings: A list of postings associated with this accounting (does not
        include the postings of children accounts).
      balance: The final balance of the list of postings associated with this account.
    """
    __slots__ = ('account', 'txn_postings', 'balance')

    def __init__(self, account_name, *args, **kwargs):
        """Create a RealAccount instance.

        Args:
          account_name: a string, the name of the account. Maybe not be None.
        """
        super().__init__(*args, **kwargs)
        assert isinstance(account_name, str)
        self.account = account_name
        self.txn_postings = []
        self.balance = inventory.Inventory()

    def __setitem__(self, key, value):
        """Prevent the setting of non-string or non-empty keys on this dict.

        Args:
          key: The dictionary key. Must be a string.
          value: The value, must be a RealAccount instance.
        Raises:
          KeyError: If the key is not a string, or is invalid.
          ValueError: If the value is not a RealAccount instance.
        """
        if not isinstance(key, str) or not key:
            raise KeyError("Invalid RealAccount key: '{}'".format(key))
        if not isinstance(value, RealAccount):
            raise ValueError("Invalid RealAccount value: '{}'".format(value))
        if not value.account.endswith(key):
            raise ValueError("RealAccount name '{}' inconsistent with key: '{}'".format(
                value.account, key))
        return super().__setitem__(key, value)

    def copy(self):
        """Override dict.copy() to clone a RealAccount.

        This is only necessary to correctly implement the copy method.
        Otherwise, calling .copy() on a RealAccount instance invokes the base
        class' method, which return just a dict.

        Returns:
          A cloned instance of RealAccount, with all members shallow-copied.
        """
        return copy.copy(self)

    def __eq__(self, other):
        """Equality predicate. All attributes are compared.

        Args:
          other: Another instance of RealAccount.
        Returns:
          A boolean, True if the two real accounts are equal.
        """
        return (dict.__eq__(self, other) and
                self.account == other.account and
                self.balance == other.balance and
                self.txn_postings == other.txn_postings)

    def __ne__(self, other):
        """Not-equality predicate. See __eq__.

        Args:
          other: Another instance of RealAccount.
        Returns:
          A boolean, True if the two real accounts are not equal.
        """
        return not self.__eq__(other)


def iter_children(real_account, leaf_only=False):
    """Iterate this account node and all its children, depth-first.

    Args:
      real_account: An instance of RealAccount.
      leaf_only: A boolean flag, true if we should yield only leaves.
    Yields:
      Instances of RealAccount, beginning with this account. The order is
      undetermined.
    """
    if leaf_only:
        if len(real_account) == 0:
            yield real_account
        else:
            for key, real_child in sorted(real_account.items()):
                for real_subchild in iter_children(real_child, leaf_only):
                    yield real_subchild
    else:
        yield real_account
        for key, real_child in sorted(real_account.items()):
            for real_subchild in iter_children(real_child):
                yield real_subchild


def get(real_account, account_name, default=None):
    """Fetch the subaccount name from the real_account node.

    Args:
      real_account: An instance of RealAccount, the parent node to look for
        children of.
      account_name: A string, the name of a possibly indirect child leaf
        found down the tree of 'real_account' nodes.
      default: The default value that should be returned if the child
        subaccount is not found.
    Returns:
      A RealAccount instance for the child, or the default value, if the child
      is not found.
    """
    if not isinstance(account_name, str):
        raise ValueError
    components = account.split(account_name)
    for component in components:
        real_child = real_account.get(component, default)
        if real_child is default:
            return default
        real_account = real_child
    return real_account


def get_or_create(real_account, account_name):
    """Fetch the subaccount name from the real_account node.

    Args:
      real_account: An instance of RealAccount, the parent node to look for
        children of, or create under.
      account_name: A string, the name of the direct or indirect child leaf
        to get or create.
    Returns:
      A RealAccount instance for the child, or the default value, if the child
      is not found.
    """
    if not isinstance(account_name, str):
        raise ValueError
    components = account.split(account_name)
    path = []
    for component in components:
        path.append(component)
        real_child = real_account.get(component, None)
        if real_child is None:
            real_child = RealAccount(account.join(*path))
            real_account[component] = real_child
        real_account = real_child
    return real_account


def contains(real_account, account_name):
    """True if the given account node contains the subaccount name.

    Args:
      account_name: A string, the name of a direct or indirect subaccount of
        this node.
    Returns:
      A boolean, true the name is a child of this node.
    """
    return get(real_account, account_name) is not None


def realize(entries, min_accounts=None, compute_balance=True):
    r"""Group entries by account, into a "tree" of realized accounts. RealAccount's
    are essentially containers for lists of postings and the final balance of
    each account, and may be non-leaf accounts (used strictly for organizing
    accounts into a hierarchy). This is then used to issue reports.

    The lists of postings in each account my be any of the entry types, except
    for Transaction, whereby Transaction entries are replaced by the specific
    Posting legs that belong to the account. Here's a simple diagram that
    summarizes this seemingly complex, but rather simple data structure:

       +-------------+ postings  +------+
       | RealAccount |---------->| Open |
       +-------------+           +------+
                                     |
                                     v
                              +------------+     +-------------+
                              | TxnPosting |---->| Transaction |
                              +------------+     +-------------+
                                     |      \                 \\\
                                     v       `\.__          +---------+
                                  +-----+         `-------->| Posting |
                                  | Pad |                   +---------+
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
      entries: A list of directives.
      min_accounts: A list of strings, account names to ensure we create,
        regardless of whether there are postings on those accounts or not.
        This can be used to ensure the root accounts all exist.
      compute_balance: A boolean, true if we should compute the final
        balance on the realization.
    Returns:
      The root RealAccount instance.
    """
    # Create lists of the entries by account.
    txn_postings_map = postings_by_account(entries)

    # Create a RealAccount tree and compute the balance for each.
    real_root = RealAccount('')
    for account_name, txn_postings in txn_postings_map.items():
        real_account = get_or_create(real_root, account_name)
        real_account.txn_postings = txn_postings
        if compute_balance:
            real_account.balance = compute_postings_balance(txn_postings)

    # Ensure a minimum set of accounts that should exist. This is typically
    # called with an instance of AccountTypes to make sure that those exist.
    if min_accounts:
        for account_name in min_accounts:
            get_or_create(real_root, account_name)

    return real_root


def postings_by_account(entries):
    """Create lists of postings and balances by account.

    This routine aggregates postings and entries grouping them by account name.
    The resulting lists contain postings in-lieu of Transaction directives, but
    the other directives are stored as entries. This yields a list of postings
    or other entries by account. All references to accounts are taken into
    account.

    Args:
      entries: A list of directives.
    Returns:
       A mapping of account name to list of TxnPosting instances or
       non-Transaction directives, sorted in the same order as the entries.
    """
    txn_postings_map = collections.defaultdict(list)
    for entry in entries:

        if isinstance(entry, Transaction):
            # Insert an entry for each of the postings.
            for posting in entry.postings:
                txn_postings_map[posting.account].append(
                    TxnPosting(entry, posting))

        elif isinstance(entry, (Open, Close, Balance, Note, Document)):
            # Append some other entries in the realized list.
            txn_postings_map[entry.account].append(entry)

        elif isinstance(entry, Pad):
            # Insert the pad entry in both realized accounts.
            txn_postings_map[entry.account].append(entry)
            txn_postings_map[entry.source_account].append(entry)

        elif isinstance(entry, Custom):
            # Insert custom entry for each account in its values.
            for custom_value in entry.values:
                if custom_value.dtype == account.TYPE:
                    txn_postings_map[custom_value.value].append(entry)

    return txn_postings_map


def filter(real_account, predicate):
    """Filter a RealAccount tree of nodes by the predicate.

    This function visits the tree and applies the predicate on each node. It
    returns a partial clone of RealAccount whereby on each node
    - either the predicate is true, or
    - for at least one child of the node the predicate is true.
    All the leaves have the predicate be true.

    Args:
      real_account: An instance of RealAccount.
      predicate: A callable/function which accepts a real_account and returns
        a boolean. If the function returns True, the node is kept.
    Returns:
      A shallow clone of RealAccount is always returned.
    """
    assert isinstance(real_account, RealAccount)

    real_copy = RealAccount(real_account.account)
    real_copy.balance = real_account.balance
    real_copy.txn_postings = real_account.txn_postings

    for child_name, real_child in real_account.items():
        real_child_copy = filter(real_child, predicate)
        if real_child_copy is not None:
            real_copy[child_name] = real_child_copy

    if len(real_copy) > 0 or predicate(real_account):
        return real_copy


def get_postings(real_account):
    """Return a sorted list a RealAccount's postings and children.

    Args:
      real_account: An instance of RealAccount.
    Returns:
      A list of Posting or directories.
    """
    # We accumulate all the postings at once here instead of incrementally
    # because we need to return them sorted.
    accumulator = []
    for real_child in iter_children(real_account):
        accumulator.extend(real_child.txn_postings)
    accumulator.sort(key=data.posting_sortkey)
    return accumulator


def iterate_with_balance(txn_postings):
    """Iterate over the entries, accumulating the running balance.

    For each entry, this yields tuples of the form:

      (entry, postings, change, balance)

    entry: This is the directive for this line. If the list contained Posting
      instance, this yields the corresponding Transaction object.
    postings: A list of postings on this entry that affect the balance. Only the
      postings encountered in the input list are included; only those affect the
      balance. If 'entry' is not a Transaction directive, this should always be
      an empty list. We preserve the original ordering of the postings as they
      appear in the input list.
    change: An Inventory object that reflects the total change due to the
      postings from this entry that appear in the list. For example, if a
      Transaction has three postings and two are in the input list, the sum of
      the two postings will be in the 'change' Inventory object. However, the
      position for the transactions' third posting--the one not included in the
      input list--will not be in this inventory.
    balance: An Inventory object that reflects the balance *after* adding the
      'change' inventory due to this entry's transaction. The 'balance' yielded
      is never None, even for entries that do not affect the balance, that is,
      with an empty 'change' inventory. It's up to the caller, the one rendering
      the entry, to decide whether to render this entry's change for a
      particular entry type.

    Note that if the input list of postings-or-entries is not in sorted date
    order, two postings for the same entry appearing twice with a different date
    in between will cause the entry appear twice. This is correct behavior, and
    it is expected that in practice this should never happen anyway, because the
    list of postings or entries should always be sorted. This method attempts to
    detect this and raises an assertion if this is seen.

    Args:
      txn_postings: A list of postings or directive instances.
        Postings affect the balance; other entries do not.
    Yields:
      Tuples of (entry, postings, change, balance) as described above.
    """

    # The running balance.
    running_balance = inventory.Inventory()

    # Previous date.
    prev_date = None

    # A list of entries at the current date.
    date_entries = []

    first = lambda pair: pair[0]
    for txn_posting in txn_postings:

        # Get the posting if we are dealing with one.
        assert not isinstance(txn_posting, Posting)
        if isinstance(txn_posting, TxnPosting):
            posting = txn_posting.posting
            entry = txn_posting.txn
        else:
            posting = None
            entry = txn_posting

        if entry.date != prev_date:
            assert prev_date is None or entry.date > prev_date, (
                "Invalid date order for postings: {} > {}".format(prev_date, entry.date))
            prev_date = entry.date

            # Flush the dated entries.
            for date_entry, date_postings in date_entries:
                change = inventory.Inventory()
                if date_postings:
                    # Compute the change due to this transaction and update the
                    # total balance at the same time.
                    for date_posting in date_postings:
                        change.add_position(date_posting)
                        running_balance.add_position(date_posting)
                yield date_entry, date_postings, change, running_balance

            date_entries.clear()
            assert not date_entries

        if posting is not None:
            # De-dup multiple postings on the same transaction entry by
            # grouping their positions together.
            index = index_key(date_entries, entry, first, operator.is_)
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
                change.add_position(date_posting)
                running_balance.add_position(date_posting)
        yield date_entry, date_postings, change, running_balance
    date_entries.clear()


def compute_balance(real_account):
    """Compute the total balance of this account and all its subaccounts.

    Args:
      real_account: A RealAccount instance.
    Returns:
      An Inventory.
    """
    total_balance = inventory.Inventory()
    for real_acc in iter_children(real_account):
        total_balance += real_acc.balance
    return total_balance


def find_last_active_posting(txn_postings):
    """Look at the end of the list of postings, and find the last
    posting or entry that is not an automatically added directive.
    Note that if the account is closed, the last posting is assumed
    to be a Close directive (this is the case if the input is valid
    and checks without errors.

    Args:
      txn_postings: a list of postings or entries.
    Returns:
      An entry, or None, if the input list was empty.
    """
    for txn_posting in reversed(txn_postings):
        assert not isinstance(txn_posting, Posting)

        if not isinstance(txn_posting, (TxnPosting, Open, Close, Pad, Balance, Note)):
            continue

        # pylint: disable=bad-continuation
        if (isinstance(txn_posting, TxnPosting) and
            txn_posting.txn.flag == flags.FLAG_UNREALIZED):
            continue
        return txn_posting


def index_key(sequence, value, key, cmp):
    """Find the index of the first element in 'sequence' which is equal to 'value'.
    If 'key' is specified, the value compared to the value returned by this
    function. If the value is not found, return None.

    Args:
      sequence: The sequence to search.
      value: The value to search for.
      key: A predicate to call to obtain the value to compare against.
      cmp: A comparison predicate.
    Returns:
      The index of the first element found, or None, if the element was not found.
    """
    for index, element in enumerate(sequence):
        if cmp(key(element), value):
            return index
    return


def dump(root_account):
    """Convert a RealAccount node to a line of lines.

    Note: this is not meant to be used to produce text reports; the reporting
    code should produce an intermediate object that contains the structure of
    it, which can then be rendered to ASCII, HTML or CSV formats. This is
    intended as a convenient little function for dumping trees of data for
    debugging purposes.

    Args:
      root_account: A RealAccount instance.
    Returns:
      A list of tuples of (first_line, continuation_line, real_account) where
        first_line: A string, the first line to render, which includes the
          account name.
        continuation_line: A string, further line to render if necessary.
        real_account: The RealAccount instance which corresponds to this
          line.
    """
    # Compute all the lines ahead of time in order to calculate the width.
    lines = []

    # Start with the root node. We push the constant prefix before this node,
    # the account name, and the RealAccount instance. We will maintain a stack
    # of children nodes to render.
    stack = [('', root_account.account, root_account, True)]
    while stack:
        prefix, name, real_account, is_last = stack.pop(-1)

        if real_account is root_account:
            # For the root node, we don't want to render any prefix.
            first = cont = ''
        else:
            # Compute the string that precedes the name directly and the one belwo
            # that for the continuation lines.
            #  |
            #  @@@ Bank1    <----------------
            #  @@@ |
            #  |   |-- Checking
            if is_last:
                first = prefix + PREFIX_LEAF_1
                cont = prefix + PREFIX_LEAF_C
            else:
                first = prefix + PREFIX_CHILD_1
                cont = prefix + PREFIX_CHILD_C

        # Compute the name to render for continuation lines.
        #  |
        #  |-- Bank1
        #  |   @@@       <----------------
        #  |   |-- Checking
        if len(real_account) > 0:
            cont_name = PREFIX_CHILD_C
        else:
            cont_name = PREFIX_LEAF_C

        # Add a line for this account.
        if not (real_account is root_account and not name):
            lines.append((first + name,
                          cont + cont_name,
                          real_account))

        # Push the children onto the stack, being careful with ordering and
        # marking the last node as such.
        child_items = sorted(real_account.items(), reverse=True)
        if child_items:
            child_iter = iter(child_items)
            child_name, child_account = next(child_iter)
            stack.append((cont, child_name, child_account, True))
            for child_name, child_account in child_iter:
                stack.append((cont, child_name, child_account, False))

    if not lines:
        return lines

    # Compute the maximum width of the lines and convert all of them to the same
    # maximal width. This makes it easy on the client.
    max_width = max(len(first_line) for first_line, _, __ in lines)
    line_format = '{{:{width}}}'.format(width=max_width)
    return [(line_format.format(first_line),
             line_format.format(cont_line),
             real_node)
            for (first_line, cont_line, real_node) in lines]


PREFIX_CHILD_1 = '|-- '
PREFIX_CHILD_C = '|   '
PREFIX_LEAF_1 = '`-- '
PREFIX_LEAF_C = '    '


def dump_balances(real_root, dformat, at_cost=False, fullnames=False, file=None):
    """Dump a realization tree with balances.

    Args:
      real_root: An instance of RealAccount.
      dformat: An instance of DisplayFormatter to format the numbers with.
      at_cost: A boolean, if true, render the values at cost.
      fullnames: A boolean, if true, don't render a tree of accounts and
        render the full account names.
      file: A file object to dump the output to. If not specified, we
        return the output as a string.
    Returns:
      A string, the rendered tree, or nothing, if 'file' was provided.
    """
    if fullnames:
        # Compute the maximum account name length;
        maxlen = max(len(real_child.account)
                     for real_child in iter_children(real_root, leaf_only=True))
        line_format = '{{:{width}}} {{}}\n'.format(width=maxlen)
    else:
        line_format = '{}       {}\n'

    output = file or io.StringIO()
    for first_line, cont_line, real_account in dump(real_root):
        if not real_account.balance.is_empty():
            if at_cost:
                rinv = real_account.balance.reduce(convert.get_cost)
            else:
                rinv = real_account.balance.reduce(convert.get_units)
            amounts = [position.units for position in rinv.get_positions()]
            positions = [amount_.to_string(dformat)
                         for amount_ in sorted(amounts, key=amount.sortkey)]
        else:
            positions = ['']

        if fullnames:
            for position in positions:
                if not position and len(real_account) > 0:
                    continue  # Skip parent accounts with no position to render.
                output.write(line_format.format(real_account.account, position))
        else:
            line = first_line
            for position in positions:
                output.write(line_format.format(line, position))
                line = cont_line

    if file is None:
        return output.getvalue()


def compute_postings_balance(txn_postings):
    """Compute the balance of a list of Postings's or TxnPosting's positions.

    Args:
      postings: A list of Posting instances and other directives (which are
        skipped).
    Returns:
      An Inventory.
    """
    final_balance = inventory.Inventory()
    for txn_posting in txn_postings:
        if isinstance(txn_posting, Posting):
            final_balance.add_position(txn_posting)
        elif isinstance(txn_posting, TxnPosting):
            final_balance.add_position(txn_posting.posting)
    return final_balance
