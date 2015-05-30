"""A plugin that cross-checks declared gains against prices on lot sales.

When you sell stock, the gains can be automatically implied by the corresponding
cash amounts. For example, in the following transaction the 2nd and 3rd postings
should match the value of the stock sold:

1999-07-31 * "Sell"
  Assets:US:BRS:Company:ESPP          -81 ADSK {26.3125 USD}
  Assets:US:BRS:Company:Cash      2141.36 USD
  Expenses:Financial:Fees            0.08 USD
  Income:US:Company:ESPP:PnL      -10.125 USD

The cost basis is checked against: 2141.36 + 008 + -10.125.

But... usually the income leg isn't given to you.
Beancount can automatically infer it using the balance, like this:

1999-07-31 * "Sell"
  Assets:US:BRS:Company:ESPP          -81 ADSK {26.3125 USD}
  Assets:US:BRS:Company:Cash      2141.36 USD
  Expenses:Financial:Fees            0.08 USD
  Income:US:Company:ESPP:PnL

However, most often you have the sales prices given to you on your transaction
confirmation statement, so you can enter this:

1999-07-31 * "Sell"
  Assets:US:BRS:Company:ESPP          -81 ADSK {26.3125 USD} @ 26.4375 USD
  Assets:US:BRS:Company:Cash      2141.36 USD
  Expenses:Financial:Fees            0.08 USD
  Income:US:Company:ESPP:PnL

This plugin does the following: For transactions which have a price, it verifies
that the proceeds of the lot will sum up to the total of the asset accounts,
disregarding the income legs. This provides yet another level of verification
and allows you to elide the income amounts, knowing that the price is there to
provide an extra level of error-checking in case you enter a typo.

"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections

from beancount.core.amount import ZERO
from beancount.core import data
from beancount.core import amount
from beancount.core import inventory
from beancount.core import account_types
from beancount.core import interpolate
from beancount.parser import options

__plugins__ = ('validate_sell_gains',)


SellGainsError = collections.namedtuple('SellGainsError', 'source message entry')


def validate_sell_gains(entries, options_map):
    """Check the sum of asset account totals for lots sold with a price on them.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []
    acc_types = options.get_account_types(options_map)
    proceed_types = set([acc_types.assets,
                         acc_types.liabilities,
                         acc_types.equity,
                         acc_types.expenses])

    default_tolerances = options_map['default_tolerance']

    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue

        # Find transactins whose lots at cost all have a price.
        postings_at_cost = [posting
                            for posting in entry.postings
                            if posting.position.lot.cost is not None]
        if not postings_at_cost or not all(posting.price is not None
                                           for posting in postings_at_cost):
            continue

        # Accumulate the total expected proceeds and the sum of the asset and
        # expenses legs.
        total_price = inventory.Inventory()
        total_proceeds = inventory.Inventory()
        for posting in entry.postings:
            position = posting.position
            # If the posting is held at cost, add the priced value to the balance.
            if position.lot.cost is not None:
                assert posting.price
                price = posting.price
                total_price.add_amount(amount.amount_mult(price, -position.number))
            else:
                # Otherwise, use the weight and ignore postings to Income accounts.
                atype = account_types.get_account_type(posting.account)
                if atype in proceed_types:
                    total_proceeds.add_amount(
                        interpolate.get_posting_weight(posting))

        # Compare inventories, currency by currency.
        dict_price = {pos.lot.currency: pos.number
                      for pos in total_price}
        dict_proceeds = {pos.lot.currency: pos.number
                         for pos in total_proceeds}

        tolerances = interpolate.infer_tolerances(entry.postings, options_map)
        invalid = False
        for currency, price_number in dict_price.items():
            # Accept twice the normal tolerance.
            tolerance = inventory.get_tolerance(tolerances,
                                                default_tolerances,
                                                currency) * 2

            proceeds_number = dict_proceeds.pop(currency, ZERO)
            diff = abs(price_number - proceeds_number)
            if diff > tolerance:
                invalid = True
                break

        if invalid or dict_proceeds:
            errors.append(
                SellGainsError(
                    entry.meta,
                    "Invalid price vs. proceeds/gains: {} vs. {}".format(
                        total_price, total_proceeds),
                    entry))

    return entries, errors
