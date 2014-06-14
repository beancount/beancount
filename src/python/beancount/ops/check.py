"""Automatic padding of gaps between entries.
"""
from collections import namedtuple

from beancount.ops import validation
from beancount.core.inventory import Inventory
from beancount.core.amount import Decimal, amount_sub
from beancount.core.data import Transaction, Balance
from beancount.core import inventory
from beancount.core import realization


BalanceError = namedtuple('BalanceError', 'fileloc message entry')

# This is based on some real-world usage: FOREX brokerage, for instance,
# accumulates error up to 1bp, and we need to tolerate that if our importers
# insert checks on at regular spaces, so we set the maximum limit at 1bp.
CHECK_PRECISION = Decimal('.015')

def check(entries, unused_options_map):
    """Process the balance assertion directives.

    For each Balance directive, check that their expected balance corresponds to
    the actual balance computed at that time and replace failing ones by new
    ones with a flag that indicates failure.

    Args:
      entries: A list of directives.
      unused_options_map: A dict of options, parsed from the inupt file.
    Returns:
      A pair of a list of directives and a list of balance check errors.
    """
    new_entries = []
    check_errors = []

    # This is similar to realization, but performed in a different order. Here
    # we process the entries one by one along with the balance checks. We use a
    # temporary realization in order to hold the incremental tree of balances,
    # so that we can easily get the amounts of an account's subaccounts for
    # making checks on parent accounts.
    real_root = realization.RealAccount('')

    for entry in entries:
        if isinstance(entry, Transaction):
            # For each of the postings' accounts, update the balance inventory.
            for posting in entry.postings:
                real_account = realization.get_or_create(real_root,
                                                         posting.account)

                # Note: Always allow negative lots for the purpose of balancing.
                # This error should show up somewhere else than here.
                real_account.balance.add_position(posting.position, True)

        elif isinstance(entry, Balance):
            # Check the balance against the check entry.
            expected_amount = entry.amount

            real_account = realization.get_or_create(real_root,
                                                     entry.account)

            # Sum up the current balances for this account and its
            # sub-accounts. We want to support checks for parent accounts
            # for the total sum of their subaccounts.
            balance = inventory.Inventory()
            for real_child in realization.iter_children(real_account, False):
                balance += real_child.balance

            # Get only the amount in the desired currency.
            balance_amount = balance.get_amount(expected_amount.currency)

            # Check if the amount is within bounds of the expected amount.
            diff_amount = amount_sub(balance_amount, expected_amount)
            if abs(diff_amount.number) > CHECK_PRECISION:
                check_errors.append(
                    BalanceError(entry.fileloc,
                                 ("Balance failed for '{}': "
                                  "expected {} != accumulated {} ({} {})").format(
                                      entry.account, balance_amount, expected_amount,
                                      diff_amount,
                                      'too much' if diff_amount else 'too little'),
                                 entry))

                # Substitute the entry by a failing entry, with the diff_amount
                # field set on it. I'm not entirely sure that this is the best
                # of ideas, maybe leaving the original check intact and insert a
                # new error entry might be more functional or easier to
                # understand.
                entry = Balance(entry.fileloc, entry.date, entry.account,
                                entry.amount, diff_amount)

        new_entries.append(entry)

    return new_entries, check_errors
