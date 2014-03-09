"""Automatic padding of gaps between entries.
"""
from collections import namedtuple

from beancount.core.inventory import Inventory
from beancount.core.amount import Decimal, amount_sub
from beancount.core.data import Transaction, Balance
from beancount.core import flags


BalanceError = namedtuple('BalanceError', 'fileloc message entry')

# This is based on some real-world usage: FOREX brokerage, for instance,
# accumulates error up to 1bp, and we need to tolerate that if our importers
# insert checks on at regular spaces, so we set the maximum limit at 1bp.
CHECK_PRECISION = Decimal('.015')

def check(entries):
    """Check for all the Balance directives and replace failing ones by new ones with
    a flag that indicates failure."""

    check_errors = []

    # Running balance for each account.
    balances = {}

    new_entries = []
    for entry in entries:

        if isinstance(entry, Transaction):
            # Update the balance inventory for each of the postings' accounts.
            for posting in entry.postings:
                try:
                    balance = balances[posting.account]
                except KeyError:
                    balance = balances[posting.account] = Inventory()
                try:
                    # Note: if this is from a padding transaction, we allow negative lots at cost.
                    allow_negative = entry.flag in (flags.FLAG_PADDING, flags.FLAG_SUMMARIZE)
                    balance.add_position(posting.position, allow_negative)
                except ValueError as e:
                    check_errors.append(
                        BalanceError(entry.fileloc,
                                   "Error balancing '{}' -- {}".format(posting.account, e),
                                   entry))

        elif isinstance(entry, Balance):
            # Check the balance against the check entry.
            check_amount = entry.amount
            try:
                # If the check is for a leaf account, just look up the current balance.
                balance = balances[entry.account]
            except KeyError:
                # If the check is for a parent account, sum up the current
                # balances for all the sub-accounts. We want to support checks
                # for parent accounts for the total sum of their subaccounts.
                balance = Inventory()
                match = lambda account_name: account_name.startswith(entry.account)
                for account, balance_account in balances.items():
                    if match(account):
                        balance += balance_account
            balance_amount = balance.get_amount(check_amount.currency)

            diff_amount = amount_sub(balance_amount, check_amount)
            if abs(diff_amount.number) > CHECK_PRECISION:
                check_errors.append(
                    BalanceError(entry.fileloc,
                               "Balance failed for '{}': {} != {} (diff: {})".format(
                                   entry.account, balance_amount, check_amount,
                                   amount_sub(balance_amount, check_amount)),
                               entry))

                # Substitute the entry by a failing entry.
                entry = Balance(entry.fileloc, entry.date, entry.account, entry.amount, diff_amount)

        new_entries.append(entry)

    return new_entries, check_errors
