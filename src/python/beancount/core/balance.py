"""
Code used to balance a list of postings.
"""
import collections

from beancount.core.amount import Decimal, amount_mult, ZERO
from beancount.core.inventory import Inventory
from beancount.core.position import Lot, Position
from beancount.core.data import Posting, reparent_posting


# An error from balancing the postings.
BalanceError = collections.namedtuple('BalanceError', 'fileloc message entry')


# The difference amount at which we consider a transaction to be balanced.
# Note: This could probably be a little smaller and that would be a good thing.
SMALL_EPSILON = Decimal('0.005')


def get_balance_amount(posting):
    """Get the amount that will need to be balanced from a posting
    of a transaction. (This is a *key* element of the semantics of transactions
    in this software.)"""

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

    return amount


def compute_residual(postings):
    """Compute the residual of a set of complete postings.
    This is used to cross-check a balanced transaction."""
    inventory = Inventory()
    for posting in postings:
        inventory.add(get_balance_amount(posting))
    return inventory


def get_incomplete_postings(entry):
    """Return new postings to balance an incomplete entry, that is, and entry
    with postings that have no amounts on them.

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
    inserted_autopostings = False
    if auto_postings_indices:

        # If there are too many such postings, we can't do anything, barf.
        if len(auto_postings_indices) > 1:
            balance_errors.append(
                BalanceError(entry.fileloc,
                             "Too many auto-postings; cannot fill in.",
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
        # complete postings.
        new_postings = []
        if not residual_positions:
            balance_errors.append(
                BalanceError(entry.fileloc,
                             "Useless auto-posting: {}.".format(inventory), entry))
            for currency in currencies:
                position = Position(Lot(currency, None, None), ZERO)
                new_postings.append(
                    Posting(entry, old_posting.account, position, None, old_posting.flag))
                inserted_autopostings = True
        else:
            # Convert all the residual positions in inventory into a posting for
            # each position.
            for position in residual_positions:
                position.number = -position.number
                new_postings.append(
                    Posting(entry, old_posting.account, position, None, old_posting.flag))
                inserted_autopostings = True

        postings[index:index+1] = new_postings

    else:
        # Detect complete sets of postings that have residual balance;
        # this is where we detect that the user has made a mistake.
        if not inventory.is_small(SMALL_EPSILON):
            balance_errors.append(
                BalanceError(entry.fileloc,
                             "Transaction does not balance: {}.".format(inventory),
                             entry))

    return postings, inserted_autopostings, balance_errors


def balance_incomplete_postings(entry):
    """Balance an entry with incomplete postings, modifying the
    empty postings on the entry itself.
    """
    postings, inserted, _ = get_incomplete_postings(entry)

    # FIXME: Make this faster, there's unnecessary copying IMO.

    # PERF(25ms): could be saved here by avoiding reparenting.
    entry.postings.clear()
    for posting in postings:
        entry.postings.append(reparent_posting(posting, entry))
