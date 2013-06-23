"""
Code used to balance a list of postings.
"""
import collections

from beancount.core.amount import Decimal, amount_mult, ZERO
from beancount.core.inventory import Inventory
from beancount.core.position import Lot, Position
from beancount.core.data import Posting


# An error from balancing the postings.
BalanceError = collections.namedtuple('BalanceError', 'fileloc message entry')


# The difference amount at which we consider a transaction to be balanced.
# Note: This could probably be a little smaller and that would be a good thing.
SMALL_EPSILON = Decimal('0.005')


def compute_residual(postings):
    """Compute the residual of a set of complete postings.
    This is used to cross-check a balanced transaction."""

    inventory = Inventory()
    for posting in postings:
        position = posting.position
        lot = position.lot

        # It the position has a cost, use that to balance this posting.
        if lot.cost:
            amount = amount_mult(lot.cost, position.number)

        # If there is a price, use that to balance this posting.
        elif posting.price:
            amount = amount_mult(posting.price, position.number)

        # Otherwise, just use the amount itself.
        else:
            amount = position.get_amount()
        inventory.add(amount)

    return inventory


def get_balance_amount(posting):
    """Get the amount that will need to be balanced from a posting
    of a transaction. (This is a *key* element of the semantics of transactions
    in this software.)"""

    # It the position has a cost, use that to balance this posting.
    position = posting.position
    lot = position.lot
    if lot.cost:
        amount = amount_mult(lot.cost, position.number)

    # If there is a price, use that to balance this posting.
    elif posting.price:
        amount = amount_mult(posting.price, position.number)

    # Otherwise, just use the amount itself.
    else:
        amount = position.get_amount()

    return amount


def balance_incomplete_postings(fileloc, entry):
    """Replace and complete postings that have no amount specified on them.

    Returns a new list of balanced postings, with the incomplete postings
    replaced with completed ones. This is probably the only place where there
    is a bit of non-trivial logic in this entire project (and the rewrite was
    to make sure it was *that* simple.)

    Note: The 'postings' parameter may be modified or destroyed for performance
    reasons; don't reuse it.
    """

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
    for i, posting in enumerate(postings):
        position = posting.position

        if position is None:
            # This posting will have to get auto-completed.
            auto_postings_indices.append(i)
        else:
            currencies.add(position.lot.currency)

            # Compute the amount to balance and update the inventory.
            balance_amount = get_balance_amount(posting)
            inventory.add(balance_amount)

    # If there are auto-postings, fill them in.
    if auto_postings_indices:
        inserted_autopostings = True

        # If there are too many such postings, we can't do anything, barf.
        if len(auto_postings_indices) > 1:
            balance_errors.append(
                BalanceError(fileloc, "Too many auto-postings; cannot fill in.", entry))

        index = auto_postings_indices[0]
        old_posting = postings[index]
        assert old_posting.price is None

        residual_positions = inventory.get_positions()

        # If there are no residual positions, we want to still insert a posting
        # but with a zero position, so that the posting shows up anyhow. We
        # insert one such posting for each currency seen in the complete
        # postings.
        new_postings = []
        if not residual_positions:
            for currency in currencies:
                position = Position(Lot(currency, None, None), ZERO)
                new_postings.append(
                    Posting(entry, old_posting.account, position, None, old_posting.flag))
        else:
            # Convert all the residual positions in inventory into a posting for
            # each position.
            for position in residual_positions:
                position.number = -position.number
                new_postings.append(
                    Posting(entry, old_posting.account, position, None, old_posting.flag))

        postings[index:index+1] = new_postings

    else:
        inserted_autopostings = False

        # Detect complete sets of postings that have residual balance.
        if not inventory.is_small(SMALL_EPSILON):
            balance_errors.append(
                BalanceError(fileloc, "Transaction does not balance: {}.".format(inventory), entry))

    return postings, inserted_autopostings, balance_errors
