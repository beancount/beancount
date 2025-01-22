"""Automatic padding of gaps between entries."""

__copyright__ = "Copyright (C) 2013-2017, 2020-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"


from beancount.core import account
from beancount.core import amount
from beancount.core import getters
from beancount.core import realization
from beancount.core.data import Balance
from beancount.core.data import Transaction
from beancount.core.interpolate import BalanceError
from beancount.core.number import ONE
from beancount.core.number import ZERO

__plugins__ = ("check",)


def get_balance_tolerance(balance_entry, options_map):
    """Get the tolerance amount for a single entry.

    Args:
      balance_entry: An instance of data.Balance
      options_map: An options dict, as per the parser.
    Returns:
      A Decimal, the amount of tolerance implied by the directive.
    """
    if balance_entry.tolerance is not None:
        # Use the balance-specific tolerance override if it is provided.
        tolerance = balance_entry.tolerance

    else:
        expo = balance_entry.amount.number.as_tuple().exponent
        if expo < 0:
            # Be generous and always allow twice the multiplier on Balance and
            # Pad because the user creates these and the rounding of those
            # balances may often be further off than those used within a single
            # transaction.
            tolerance = options_map["inferred_tolerance_multiplier"] * 2
            tolerance = ONE.scaleb(expo) * tolerance
        else:
            tolerance = ZERO

    return tolerance


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
    real_root = realization.RealAccount("")

    # Figure out the set of accounts for which we need to compute a running
    # inventory balance.
    asserted_accounts = {entry.account for entry in entries if isinstance(entry, Balance)}

    # Add all children accounts of an asserted account to be calculated as well,
    # and pre-create these accounts, and only those (we're just being tight to
    # make sure).
    asserted_match_list = [
        account.parent_matcher(account_) for account_ in asserted_accounts
    ]
    for account_ in getters.get_accounts(entries):
        if account_ in asserted_accounts or any(
            match(account_) for match in asserted_match_list
        ):
            realization.get_or_create(real_root, account_)

    # Get the Open directives for each account.
    open_close_map = getters.get_account_open_close(entries)

    for entry in entries:
        if isinstance(entry, Transaction):
            # For each of the postings' accounts, update the balance inventory.
            for posting in entry.postings:
                real_account = realization.get(real_root, posting.account)

                # The account will have been created only if we're meant to track it.
                if real_account is not None:
                    # Note: Always allow negative lots for the purpose of balancing.
                    # This error should show up somewhere else than here.
                    real_account.balance.add_position(posting)

        elif isinstance(entry, Balance):
            # Check that the currency of the balance check is one of the allowed
            # currencies for that account.
            expected_amount = entry.amount
            open = None
            try:
                open, _ = open_close_map[entry.account]
            except KeyError:
                check_errors.append(
                    BalanceError(
                        entry.meta,
                        "Invalid reference to unknown account '{}'".format(entry.account),
                        entry,
                    )
                )

            if (
                expected_amount is not None
                and open
                and open.currencies
                and expected_amount.currency not in open.currencies
            ):
                check_errors.append(
                    BalanceError(
                        entry.meta,
                        "Invalid currency '{}' for Balance directive: ".format(
                            expected_amount.currency
                        ),
                        entry,
                    )
                )

            # Sum up the current balances for this account and its
            # sub-accounts. We want to support checks for parent accounts
            # for the total sum of their subaccounts.
            #
            # FIXME: Improve the performance further by computing the balance
            # for the desired currency only. This won't allow us to cache in
            # this way but may be faster, if we're not asserting all the
            # currencies. Furthermore, we could probably avoid recomputing the
            # balance if a subtree of positions hasn't been invalidated by a new
            # position added to the realization. Do this.
            real_account = realization.get(real_root, entry.account)
            assert real_account is not None, "Missing {}".format(entry.account)
            subtree_balance = realization.compute_balance(real_account, leaf_only=False)

            # Get only the amount in the desired currency.
            balance_amount = subtree_balance.get_currency_units(expected_amount.currency)

            # Check if the amount is within bounds of the expected amount.
            diff_amount = amount.sub(balance_amount, expected_amount)

            # Use the specified tolerance or automatically infer it.
            tolerance = get_balance_tolerance(entry, options_map)

            if abs(diff_amount.number) > tolerance:
                check_errors.append(
                    BalanceError(
                        entry.meta,
                        (
                            "Balance failed for '{}': "
                            "expected {} != accumulated {} ({} {})"
                        ).format(
                            entry.account,
                            expected_amount,
                            balance_amount,
                            abs(diff_amount.number),
                            ("too much" if diff_amount.number > 0 else "too little"),
                        ),
                        entry,
                    )
                )

                # Substitute the entry by a failing entry, with the diff_amount
                # field set on it. I'm not entirely sure that this is the best
                # of ideas, maybe leaving the original check intact and insert a
                # new error entry might be more functional or easier to
                # understand.
                entry = entry._replace(meta=entry.meta.copy(), diff_amount=diff_amount)

        new_entries.append(entry)

    return new_entries, check_errors
