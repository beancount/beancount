"""Automatic padding of gaps between entries.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import re

from beancount.core.amount import D
from beancount.core.amount import ONE
from beancount.core.amount import ZERO
from beancount.core.data import Transaction
from beancount.core.data import Balance
from beancount.core import amount
from beancount.core import account
from beancount.core import inventory
from beancount.core import realization
from beancount.core import getters

__plugins__ = ('check',)


BalanceError = collections.namedtuple('BalanceError', 'source message entry')


def get_tolerance(balance_entry):
    """Get the tolerance amount for a single entry.

    Args:
      balance_entry: An instance of data.Balance
    Returns:
      A Decimal, the amount of tolerance implied by the directive.
    """
    expo = balance_entry.amount.number.as_tuple().exponent
    if expo < 0:
        return ONE.scaleb(expo)  ## / 2
    return ZERO


def check(entries, options_map):
    """Process the balance assertion directives.

    For each Balance directive, check that their expected balance corresponds to
    the actual balance computed at that time and replace failing ones by new
    ones with a flag that indicates failure.

    Args:
      entries: A list of directives.
      options_map: A dict of options, parsed from the input file.
    Returns:
      A pair of a list of directives and a list of balance check errors.
    """
    new_entries = []
    check_errors = []

    # This is similar to realization, but performed in a different order, and
    # where we only accumulate inventories for accounts that have balance
    # assertions in them (this saves on time). Here we process the entries one
    # by one along with the balance checks. We use a temporary realization in
    # order to hold the incremental tree of balances, so that we can easily get
    # the amounts of an account's subaccounts for making checks on parent
    # accounts.
    real_root = realization.RealAccount('')

    # Figure out the set of accounts for which we need to compute a running
    # inventory balance.
    asserted_accounts = {entry.account
                         for entry in entries
                         if isinstance(entry, Balance)}

    # Add all children accounts of an asserted account to be calculated as well,
    # and pre-create these accounts, and only those (we're just being tight to
    # make sure).
    asserted_match_list = [account.parent_matcher(account_)
                           for account_ in asserted_accounts]
    for account_ in getters.get_accounts(entries):
        if (account_ in asserted_accounts or
            any(match(account_) for match in asserted_match_list)):
            realization.get_or_create(real_root, account_)

    for entry in entries:
        if isinstance(entry, Transaction):
            # For each of the postings' accounts, update the balance inventory.
            for posting in entry.postings:
                real_account = realization.get(real_root, posting.account)

                # The account will have been created only if we're meant to track it.
                if real_account is not None:
                    # Note: Always allow negative lots for the purpose of balancing.
                    # This error should show up somewhere else than here.
                    real_account.balance.add_position(posting.position)

        elif isinstance(entry, Balance):
            # Check the balance against the check entry.
            expected_amount = entry.amount

            real_account = realization.get(real_root, entry.account)
            assert real_account is not None, "Missing {}".format(entry.account)

            # Sum up the current balances for this account and its
            # sub-accounts. We want to support checks for parent accounts
            # for the total sum of their subaccounts.
            subtree_balance = inventory.Inventory()
            for real_child in realization.iter_children(real_account, False):
                subtree_balance += real_child.balance

            # Get only the amount in the desired currency.
            balance_amount = subtree_balance.get_units(expected_amount.currency)

            # Check if the amount is within bounds of the expected amount.
            diff_amount = amount.amount_sub(balance_amount, expected_amount)

            ## FIXME:
            #tolerance = get_tolerance(entry)
            tolerance = options_map['tolerance']

            if abs(diff_amount.number) > tolerance:
                check_errors.append(
                    BalanceError(entry.meta,
                                 ("Balance failed for '{}': "
                                  "expected {} != accumulated {} ({} {})").format(
                                      entry.account, expected_amount, balance_amount,
                                      abs(diff_amount.number),
                                      ('too much'
                                       if diff_amount.number > 0
                                       else 'too little')),
                                 entry))

                # Substitute the entry by a failing entry, with the diff_amount
                # field set on it. I'm not entirely sure that this is the best
                # of ideas, maybe leaving the original check intact and insert a
                # new error entry might be more functional or easier to
                # understand.
                entry = Balance(entry.meta.copy(), entry.date, entry.account,
                                entry.amount, diff_amount)

        new_entries.append(entry)

    return new_entries, check_errors
