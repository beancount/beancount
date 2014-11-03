"""Code used to automatically complete postings without positions.
"""
import collections
import copy

from beancount.core.amount import D
from beancount.core.amount import ZERO
from beancount.core.amount import amount_mult
from beancount.core.inventory import Inventory
from beancount.core.position import Lot
from beancount.core.position import Position
from beancount.core.data import Transaction
from beancount.core.data import Posting
from beancount.core.data import reparent_posting
from beancount.core.data import entry_replace
from beancount.core import getters
from beancount.core import inventory


# An error from balancing the postings.
BalanceError = collections.namedtuple('BalanceError', 'source message entry')


# The difference amount at which we consider a transaction to be balanced.
# Note: This could probably be a little smaller and that would be a good thing.
# See https://docs.google.com/document/d/1MY2JMiiXUmcwsOT0CkiK-fCo0ZE7nbr8uTcTL50b6X4/
SMALL_EPSILON = D('0.005')


def get_balance_amount(posting):
    """Get the amount that will need to be balanced from a posting of a transaction.

    This is a *key* element of the semantics of transactions in this software. A
    balance amount is the amount used to check the balance of a transaction.
    Here are all relevant examples, with the amounts used to balance the
    postings:

      Assets:Account  5234.50 USD                             ->  5234.50 USD
      Assets:Account  3877.41 EUR @ 1.35 USD                  ->  5234.50 USD
      Assets:Account       10 GOOG {523.45 USD}               ->  5234.50 USD
      Assets:Account       10 GOOG {523.45 USD} @ 545.60 CAD  ->  5234.50 USD

    Args:
      posting: A Posting instance.
    Returns:
      An amount, required to balance this posting.
    """
    position = posting.position
    lot = position.lot

    # It the position has a cost, use that to balance this posting.
    if lot.cost is not None:
        amount = amount_mult(lot.cost, position.number)

    # If there is a price, use that to balance this posting.
    elif posting.price is not None:
        amount = amount_mult(posting.price, position.number)

    # Otherwise, just use the amount itself.
    else:
        amount = position.get_amount()

    return amount


def has_nontrivial_balance(posting):
    """Return True if a Posting has a balance amount that would have to be calculated.

    Args:
      posting: A Posting instance.
    Returns:
      A boolean.
    """
    lot = posting.position.lot
    return lot.cost or posting.price


def compute_residual(postings):
    """Compute the residual of a set of complete postings.
    This is used to cross-check a balanced transaction.

    Args:
      postings: A list of Posting instances.
    Returns:
      An instance of Inventory, with the residual of the given list
      of postings.
    """
    inventory = Inventory()
    for posting in postings:
        inventory.add_amount(get_balance_amount(posting))
    return inventory


def fill_residual_posting(entry, account_rounding):
    """If necessary, insert a posting to absorb the residual.
    This makes the transaction balance exactly.

    Args:
      entry: An instance of a Transaction.
      account_rounding: A string, the name of the rounding account that
        absorbs residuals / rounding errors.
    Returns:
      A possibly new, modified entry with a new posting. If a residual
      was not needed - the transaction already balanced perfectly - no new
      leg is inserted.
    """
    residual = compute_residual(entry.postings)
    if residual.is_empty():
        return entry
    else:
        new_postings = list(entry.postings)
        for position in residual.get_positions():
            rounding_posting = Posting(None, account_rounding, -position, None, None)
            new_postings.append(rounding_posting)
        return entry_replace(entry, postings=new_postings)


def get_incomplete_postings(entry):
    """Return new postings to balance an incomplete entry, that is, and entry
    with postings that have no amounts on them.

    Returns a new list of balanced postings, with the incomplete postings
    replaced with completed ones. This is probably the only place where there
    is a bit of non-trivial logic in this entire project (and the rewrite was
    to make sure it was *that* simple.)

    Note: The 'postings' parameter may be modified or destroyed for performance
    reasons; don't reuse it.

    Args:
      entry: An instance of a valid directive.
    Returns:
      A tuple of: a list of new postings to replace the entry's unbalanced
      postings, a boolean set to true if we've inserted new postings, and a list
      of balance errors generated during the balancing process.
    """
    # Make a copy of the original list of postings.
    postings = list(entry.postings)

    # Errors during balancing.
    balance_errors = []

    # The list of postings without and with an explicit position.
    auto_postings_indices = []

    # Currencies seen in complete postings.
    currencies = set()

    # An inventory to accumulate the residual balance.
    inventory = Inventory()

    # Process all the postings.
    has_nonzero_amount = False
    has_regular_postings = False
    for i, posting in enumerate(postings):
        position = posting.position

        if position is None:
            # This posting will have to get auto-completed.
            auto_postings_indices.append(i)
        else:
            currencies.add(position.lot.currency)

            # Compute the amount to balance and update the inventory.
            balance_amount = get_balance_amount(posting)
            inventory.add_amount(balance_amount)

            has_regular_postings = True
            if balance_amount:
                has_nonzero_amount = True

    # If there are auto-postings, fill them in.
    has_inserted = False
    if auto_postings_indices:

        # If there are too many such postings, we can't do anything, barf.
        if len(auto_postings_indices) > 1:
            balance_errors.append(
                BalanceError(entry.source,
                             "Too many auto-postings; cannot fill in",
                             entry))
            # Delete the redundant auto-postings.
            for index in auto_postings_indices[1:]:
                del postings[index]

        index = auto_postings_indices[0]
        old_posting = postings[index]
        assert old_posting.price is None

        residual_positions = inventory.get_positions()

        # If there are no residual positions, we want to still insert a posting
        # but with a zero position for each currency, so that the posting shows
        # up anyhow. We insert one such posting for each currency seen in the
        # complete postings. Note: if all the non-auto postings are zero, we
        # want to avoid sending a warning; the input text clearly implies the
        # author knows this would be useless.
        new_postings = []
        if not residual_positions and ((has_regular_postings and has_nonzero_amount) or
                                       not has_regular_postings):
            balance_errors.append(
                BalanceError(entry.source,
                             "Useless auto-posting: {}".format(inventory), entry))
            for currency in currencies:
                position = Position(Lot(currency, None, None), ZERO)
                new_postings.append(
                    Posting(entry, old_posting.account, position, None, old_posting.flag))
                has_inserted = True
        else:
            # Convert all the residual positions in inventory into a posting for
            # each position.
            for position in residual_positions:
                position.number = -position.number
                new_postings.append(
                    Posting(entry, old_posting.account, position, None, old_posting.flag))
                has_inserted = True

        postings[index:index+1] = new_postings

    else:
        # Checking for unbalancing transactions has been moved to the validation
        # stage, so although we already have the input transaction's residuals
        # conveniently precomputed here, we are postponing the check to allow
        # plugins to "fixup" unbalancing transactions. We want to allow users to
        # be able to input unbalancing transactions as long as the final
        # transactions objects that appear on the stream (after processing the
        # plugins) are balanced. See {9e6c14b51a59}.
        pass

    return (postings, has_inserted, balance_errors)


def balance_incomplete_postings(entry):
    """Balance an entry with incomplete postings, modifying the
    empty postings on the entry itself. This also sets the parent of
    all the postings to this entry.

    Args:
      entry: An instance of a valid directive. This entry is modified by
        having new postings inserted to it.
    Returns:
      A list of errors, or None, if none occurred.
    """
    # No postings... nothing to do.
    if not entry.postings:
        return None

    postings, inserted, errors = get_incomplete_postings(entry)

    # If we could make this faster to avoid the unnecessary copying, it would
    # make parsing substantially faster.
    # PERF(25ms): could be saved here by avoiding reparenting.
    entry.postings.clear()
    for posting in postings:
        entry.postings.append(reparent_posting(posting, entry))

    return errors or None


def compute_postings_balance(postings):
    """Compute the balance of a list of Postings's positions.

    Args:
      postings: A list of Posting instances and other directives (which are
        skipped).
    Returns:
      An Inventory.
    """
    final_balance = Inventory()
    for posting in postings:
        if isinstance(posting, Posting):
            final_balance.add_position(posting.position)
    return final_balance


def compute_entries_balance(entries, prefix=None, date=None):
    """Compute the balance of all postings of a list of entries.

    Sum up all the positions in all the postings of all the transactions in the
    list of entries and return an inventory of it.

    Args:
      entries: A list of directives.
      prefix: If specified, a prefix string to restrict by account name. Only
        postings with an account that starts with this prefix will be summed up.
      date: A datetime.date instance at which to stop adding up the balance.
        The date is exclusive.
    Returns:
      An instance of Inventory.
    """
    total_balance = Inventory()
    for entry in entries:
        if not (date is None or entry.date < date):
            break
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                if prefix is None or posting.account.startswith(prefix):
                    total_balance.add_position(posting.position)
    return total_balance


def compute_entry_context(entries, context_entry):
    """Compute the balances of all accounts referenced by entry up to entry.

    This provides the inventory of the accounts to which the entry is to be
    applied, before and after.

    Args:
      entries: A list of directives.
      context_entry: The entry for which we want to obtain the before and after
        context.
    Returns:
      Two dicts of account-name to Inventory instance, one which represents the
      context before the entry is applied, and one that represents the context
      after it has been applied.
    """
    assert context_entry is not None, "context_entry is missing."

    # Get the set of accounts for which to compute the context.
    context_accounts = getters.get_entry_accounts(context_entry)

    # Iterate over the entries until we find the target one and accumulate the
    # balance.
    context_before = collections.defaultdict(inventory.Inventory)
    for entry in entries:
        if entry is context_entry:
            break
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                if not any(posting.account == account
                           for account in context_accounts):
                    continue
                balance = context_before[posting.account]
                balance.add_position(posting.position)

    # Compute the after context for the entry.
    context_after = copy.deepcopy(context_before)
    if isinstance(context_entry, Transaction):
        for posting in entry.postings:
            balance = context_after[posting.account]
            balance.add_position(posting.position)

    return context_before, context_after
