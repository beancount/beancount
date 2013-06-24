"""Automatic padding of gaps between entries.
"""
from collections import namedtuple, defaultdict

from beancount.core.inventory import Inventory
from beancount.core.amount import Decimal, amount_sub
from beancount.core.data import Transaction, Check
from beancount.core import flags


CheckError = namedtuple('CheckError', 'fileloc message entry')

# This is based on some real-world usage: FOREX brokerage, for instance,
# accumulates error up to 1bp, and we need to tolerate that if our importers
# insert checks on at regular spaces, so we set the maximum limit at 1bp.
CHECK_PRECISION = Decimal('.015')

def check(entries):
    """Check for all the Check directives and replace failing ones by new ones with
    a flag that indicates failure."""

    check_errors = []

    # Running balance for each account.
    balances = defaultdict(Inventory)

    new_entries = []
    for entry in entries:

        if isinstance(entry, Transaction):
            # Update the balance inventory for each of the postings' accounts.
            for posting in entry.postings:
                balance = balances[posting.account]
                try:
                    # Note: if this is from a padding transaction, we allow negative lots at cost.
                    allow_negative = entry.flag in (flags.FLAG_PADDING, flags.FLAG_SUMMARIZE)
                    balance.add_position(posting.position, allow_negative)
                except ValueError as e:
                    check_errors.append(
                        CheckError(entry.fileloc,
                                   "Error balancing '{}' -- {}".format(posting.account.name, e),
                                   entry))

        elif isinstance(entry, Check):
            # Check the balance against the check entry.
            check_amount = entry.amount
            balance = balances[entry.account]
            balance_amount = balance.get_amount(check_amount.currency)
            diff_amount = amount_sub(balance_amount, check_amount)
            if diff_amount.number.abs() > CHECK_PRECISION:
                check_errors.append(
                    CheckError(entry.fileloc,
                               "Check failed for '{}': {} != {}".format(entry.account.name,
                                                                        balance_amount,
                                                                        check_amount), entry))

                # Substitute the entry by a failing entry.
                entry = Check(entry.fileloc, entry.date, entry.account, entry.amount, diff_amount)

        new_entries.append(entry)

    return new_entries, check_errors
