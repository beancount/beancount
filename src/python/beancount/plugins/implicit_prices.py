"""This plugin synthesizes Price directives for all Postings with a price or
directive or if it is an augmenting posting, has a cost directive.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core.data import Transaction
from beancount.core import data
from beancount.core import amount
from beancount.core import inventory

__plugins__ = ('add_implicit_prices',)


ImplicitPriceError = collections.namedtuple('ImplicitPriceError', 'source message entry')


def add_implicit_prices(entries, unused_options_map):
    """Insert implicitly defined prices from Transactions.

    Explicit price entries are simply maintained in the output list. Prices from
    postings with costs or with prices from Transaction entries are synthesized
    as new Price entries in the list of entries output.

    Args:
      entries: A list of directives. We're interested only in the Transaction instances.
      unused_options_map: A parser options dict.
    Returns:
      A list of entries, possibly with more Price entries than before, and a
      list of errors.
    """
    new_entries = []
    errors = []

    # A dict of (date, currency, cost-currency) to price entry.
    new_price_entry_map = {}

    balances = collections.defaultdict(inventory.Inventory)
    for entry in entries:
        # Always replicate the existing entries.
        new_entries.append(entry)

        if isinstance(entry, Transaction):
            # Inspect all the postings in the transaction.
            for posting in entry.postings:
                units = posting.units
                cost = posting.cost

                # Check if the position is matching against an existing
                # position.
                _, booking = balances[posting.account].add_position(posting)

                # Add prices when they're explicitly specified on a posting. An
                # explicitly specified price may occur in a conversion, e.g.
                #      Assets:Account    100 USD @ 1.10 CAD
                # or, if a cost is also specified, as the current price of the
                # underlying instrument, e.g.
                #      Assets:Account    100 HOOL {564.20} @ {581.97} USD
                if posting.price is not None:
                    meta = data.new_metadata(entry.meta["filename"], entry.meta["lineno"])
                    price_entry = data.Price(meta, entry.date,
                                             units.currency,
                                             posting.price)

                # Add costs, when we're not matching against an existing
                # position. This happens when we're just specifying the cost,
                # e.g.
                #      Assets:Account    100 HOOL {564.20}
                elif (cost is not None and
                      booking != inventory.Booking.REDUCED):
                    meta = data.new_metadata(entry.meta["filename"], entry.meta["lineno"])
                    price_entry = data.Price(meta, entry.date,
                                             units.currency,
                                             amount.Amount(cost.number,
                                                           cost.currency))

                else:
                    price_entry = None

                if price_entry is not None:
                    key = (price_entry.date,
                           price_entry.currency,
                           price_entry.amount.number,  # Ideally should be removed.
                           price_entry.amount.currency)
                    try:
                        new_price_entry_map[key]

                        ## Do not fail for now. We still have many valid use
                        ## cases of duplicate prices on the same date, for
                        ## example, stock splits, or trades on two dates with
                        ## two separate reported prices. We need to figure out a
                        ## more elegant solution for this in the long term.
                        ## Keeping both for now. We should ideally not use the
                        ## number in the de-dup key above.
                        #
                        # dup_entry = new_price_entry_map[key]
                        # if price_entry.amount.number == dup_entry.amount.number:
                        #     # Skip duplicates.
                        #     continue
                        # else:
                        #     errors.append(
                        #         ImplicitPriceError(
                        #             entry.meta,
                        #             "Duplicate prices for {} on {}".format(entry,
                        #                                                    dup_entry),
                        #             entry))
                    except KeyError:
                        new_price_entry_map[key] = price_entry
                        new_entries.append(price_entry)

    return new_entries, errors
