"""Code used to automatically complete postings without positions.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import copy

from decimal import Decimal

from beancount.core.number import D
from beancount.core.number import ONE
from beancount.core.number import ZERO
from beancount.core.number import MISSING
from beancount.core.amount import Amount
from beancount.core.position import CostSpec
from beancount.core.position import Cost
from beancount.core.inventory import Inventory
from beancount.core import inventory
from beancount.core import convert
from beancount.core.data import Transaction
from beancount.core.data import Posting
from beancount.core import getters
from beancount.utils import defdict


# An upper bound on the tolerance value, this is the maximum the tolerance
# should ever be.
MAXIMUM_TOLERANCE = D('0.5')


# The maximum number of user-specified coefficient digits we should allow for a
# tolerance setting.
MAX_TOLERANCE_DIGITS = 5


def is_tolerance_user_specified(tolerance):
    """Return true if the given tolerance number was user-specified.

    This would allow the user to provide a tolerance like # 0.1234 but not
    0.123456. This is used to detect whether a tolerance value # is input by the
    user and not inferred automatically.

    Args:
      tolerance: An instance of Decimal.
    Returns:
      A boolean.
    """
    return len(tolerance.as_tuple().digits) < MAX_TOLERANCE_DIGITS



# An error from balancing the postings.
BalanceError = collections.namedtuple('BalanceError', 'source message entry')


def has_nontrivial_balance(posting):
    """Return True if a Posting has a balance amount that would have to be calculated.

    Args:
      posting: A Posting instance.
    Returns:
      A boolean.
    """
    return posting.cost or posting.price


def compute_residual(postings):
    """Compute the residual of a set of complete postings, and the per-currency precision.

    This is used to cross-check a balanced transaction.

    The precision is the maximum fraction that is being used for each currency
    (a dict). We use the currency of the weight amount in order to infer the
    quantization precision for each currency. Integer amounts aren't
    contributing to the determination of precision.

    Args:
      postings: A list of Posting instances.
    Returns:
      An instance of Inventory, with the residual of the given list of postings.
    """
    inventory = Inventory()
    for posting in postings:
        # Skip auto-postings inserted to absorb the residual (rounding error).
        if posting.meta and posting.meta.get(AUTOMATIC_RESIDUAL, False):
            continue
        # Add to total residual balance.
        inventory.add_amount(convert.get_weight(posting))
    return inventory


def infer_tolerances(postings, options_map, use_cost=None):
    """Infer tolerances from a list of postings.

    The tolerance is the maximum fraction that is being used for each currency
    (a dict). We use the currency of the weight amount in order to infer the
    quantization precision for each currency. Integer amounts aren't
    contributing to the determination of precision.

    The 'use_cost' option allows one to experiment with letting postings at cost
    and at price influence the maximum value of the tolerance. It's tricky to
    use and alters the definition of the tolerance in a non-trivial way, if you
    use it. The tolerance is expanded by the sum of the cost times a fraction 'M'
    of the smallest digits in the number of units for all postings held at cost.

    For example, in this transaction:

        2006-01-17 * "Plan Contribution"
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:VWELX 18.572 VWELX {30.96 USD}
          Assets:Investments:Cash -1150.00 USD

    The tolerance for units of USD will calculated as the MAXIMUM of:

      0.01 * M = 0.005 (from the 1150.00 USD leg)

      The sum of
        0.001 * M x 30.96 = 0.01548 +
        0.001 * M x 30.96 = 0.01548
                          = 0.03096

    So the tolerance for USD in this case is max(0.005, 0.03096) = 0.03096. Prices
    contribute similarly to the maximum tolerance allowed.

    Note that 'M' above is the inferred_tolerance_multiplier and its default
    value is 0.5.

    Args:
      postings: A list of Posting instances.
      options_map: A dict of options.
      use_cost: A boolean, true if we should be using a combination of the smallest
        digit of the number times the cost or price in order to infer the tolerance.
        If the value is left unspecified (as 'None'), the default value can be
        overridden by setting an option.
    Returns:
      A dict of currency to the tolerated difference amount to be used for it,
      e.g. 0.005.
    """
    if use_cost is None:
        use_cost = options_map["infer_tolerance_from_cost"]

    inferred_tolerance_multiplier = options_map["inferred_tolerance_multiplier"]

    default_tolerances = options_map["inferred_tolerance_default"]
    tolerances = default_tolerances.copy()

    cost_tolerances = collections.defaultdict(D)
    for posting in postings:
        # Skip the precision on automatically inferred postings.
        if posting.meta and AUTOMATIC_META in posting.meta:
            continue
        units = posting.units
        if not (isinstance(units, Amount) and isinstance(units.number, Decimal)):
            continue

        # Compute bounds on the number.
        currency = units.currency
        expo = units.number.as_tuple().exponent
        if expo < 0:
            # Note: the exponent is a negative value.
            tolerance = ONE.scaleb(expo) * inferred_tolerance_multiplier
            tolerances[currency] = max(tolerance,
                                       tolerances.get(currency, -1024))

            if not use_cost:
                continue

            # Compute bounds on the smallest digit of the number implied as cost.
            cost = posting.cost
            if cost is not None:
                cost_currency = cost.currency
                if isinstance(cost, Cost):
                    cost_tolerance = min(tolerance * cost.number, MAXIMUM_TOLERANCE)
                else:
                    assert isinstance(cost, CostSpec)
                    cost_tolerance = MAXIMUM_TOLERANCE
                    for cost_number in cost.number_total, cost.number_per:
                        if cost_number is None or cost_number is MISSING:
                            continue
                        cost_tolerance = min(tolerance * cost_number, cost_tolerance)
                cost_tolerances[cost_currency] += cost_tolerance

            # Compute bounds on the smallest digit of the number implied as cost.
            price = posting.price
            if isinstance(price, Amount) and isinstance(price.number, Decimal):
                price_currency = price.currency
                price_tolerance = min(tolerance * price.number, MAXIMUM_TOLERANCE)
                cost_tolerances[price_currency] += price_tolerance

    for currency, tolerance in cost_tolerances.items():
        tolerances[currency] = max(tolerance, tolerances.get(currency, -1024))

    default = tolerances.pop('*', ZERO)
    return defdict.ImmutableDictWithDefault(tolerances, default=default)


# Meta-data field appended to automatically inserted postings.
# (Note: A better name might have been '__interpolated__'.)
AUTOMATIC_META = '__automatic__'

# Meta-data field appended to postings inserted to absorb rounding error.
AUTOMATIC_RESIDUAL = '__residual__'

# Meta-data field added for the tolerances inferred for this entry.
AUTOMATIC_TOLERANCES = '__tolerances__'


def get_residual_postings(residual, account_rounding):
    """Create postings to book the given residuals.

    Args:
      residual: An Inventory, the residual positions.
      account_rounding: A string, the name of the rounding account that
        absorbs residuals / rounding errors.
    Returns:
      A list of new postings to be inserted to reduce the given residual.
    """
    meta = {AUTOMATIC_META: True,
            AUTOMATIC_RESIDUAL: True}
    return [Posting(account_rounding, -position.units, position.cost, None, None,
                    meta.copy())
            for position in residual.get_positions()]


def fill_residual_posting(entry, account_rounding):
    """If necessary, insert a posting to absorb the residual.
    This makes the transaction balance exactly.

    Note: This was developed in order to tweak transactions before exporting
    them to Ledger. A better method would be to enable the feature that
    automatically inserts these rounding postings on all transactions, and so
    maybe this method can be deprecated if we do so.

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
    if not residual.is_empty():
        new_postings = list(entry.postings)
        new_postings.extend(get_residual_postings(residual, account_rounding))
        entry = entry._replace(postings=new_postings)
    return entry


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
                    total_balance.add_position(posting)
    return total_balance


def compute_entry_context(entries, context_entry, additional_accounts=None):
    """Compute the balances of all accounts referenced by entry up to entry.

    This provides the inventory of the accounts to which the entry is to be
    applied, before and after.

    Args:
      entries: A list of directives.
      context_entry: The entry for which we want to obtain the before and after
        context.
      additional_accounts: Additional list of accounts to include in calculating
        the balance. This is used when invoked for debugging, in case the booked
        & interpolated transaction doesn't have all the accounts we need because
        it had an error (the booking code will remove invalid postings).
    Returns:
      Two dicts of account-name to Inventory instance, one which represents the
      context before the entry is applied, and one that represents the context
      after it has been applied.
    """
    assert context_entry is not None, "context_entry is missing."

    # Get the set of accounts for which to compute the context.
    context_accounts = getters.get_entry_accounts(context_entry)
    if additional_accounts:
        context_accounts.update(additional_accounts)

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
                balance.add_position(posting)

    # Compute the after context for the entry.
    context_after = copy.deepcopy(context_before)
    if isinstance(context_entry, Transaction):
        for posting in entry.postings:
            balance = context_after[posting.account]
            balance.add_position(posting)

    return context_before, context_after


def quantize_with_tolerance(tolerances, currency, number):
    """Quantize the units using the tolerance dict.

    Args:
      tolerances: A dict of currency to tolerance Decimalvalues.
      number: A number to quantize.
      currency: A string currency.
    Returns:
      A Decimal, the number possibly quantized.
    """
    # Applying rounding to the default tolerance, if there is one.
    tolerance = tolerances.get(currency)
    if tolerance:
        quantum = (tolerance * 2).normalize()

        # If the tolerance is a neat number provided by the user,
        # quantize the inferred numbers. See doc on quantize():
        #
        # Unlike other operations, if the length of the coefficient
        # after the quantize operation would be greater than
        # precision, then an InvalidOperation is signaled. This
        # guarantees that, unless there is an error condition, the
        # quantized exponent is always equal to that of the
        # right-hand operand.
        if is_tolerance_user_specified(quantum):
            number = number.quantize(quantum)
    return number
