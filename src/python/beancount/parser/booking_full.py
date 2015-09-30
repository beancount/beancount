"""Full (new) booking implementation.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import logging

from beancount.core.number import MISSING
from beancount.core.data import Transaction
from beancount.core import inventory



FullBookingError = collections.namedtuple('FullBookingError', 'source message entry')


class BookingStats:

    def __init__(self):
        self.num_transactions = 0
        self.num_postings = 0
        self.num_interp_amount = 0
        self.num_interp_units = 0
        self.num_lots = 0
        self.num_lots_atcost = 0
        self.num_interp_price = 0

    def __str__(self):
        return '; '.join(["transactions: {s.num_transactions}",
                          "postings: {s.num_postings}",
                          "interp_amount: {s.num_interp_amount}",
                          "interp_units: {s.num_interp_units}",
                          "lots: {s.num_lots}",
                          "lots_atcost: {s.num_lots_atcost}",
                          "interp_price: {s.num_interp_price}"]).format(s=self)


def book(entries, options_map):
    """Interpolate missing data from the entries using the full historical algorithm.

    Args:
      incomplete_entries: A list of directives, with some postings possibly left
        with incomplete amounts as produced by the parser.
      options_map: An options dict as produced by the parser.
    Returns:
      A pair of
        entries: A list of interpolated entries with all their postings completed.
        errors: New errors produced during interpolation.
    """
    print()

    balances = collections.defaultdict(inventory.Inventory)
    stats = BookingStats()
    errors = []
    for entry in entries:
        if isinstance(entry, Transaction):
            stats.num_transactions += 1

            # Group postings by currency.
            groups, free_postings = group_postings_by_currency(entry.postings)

            for posting in entry.postings:
                stats.num_postings += 1
                pos = posting.position
                if pos is None:
                    stats.num_interp_amount += 1

                elif pos.units.number is None:
                    stats.num_interp_units += 1

                elif posting.price and (posting.price.number is None or
                                        posting.price.currency is None):
                    stats.num_interp_price += 1

                if 1:
                    stats.num_lots += 1

                    # FIXME: This is broken; rewrite this.

                    # compound_cost = lot.cost
                    # if compound_cost is not None:
                    #     if compound_cost.number_total is not None:
                    #         # Compute the per-unit cost if there is some total cost
                    #         # component involved.
                    #         units = pos.units.number
                    #         cost_total = compound_cost.number_total
                    #         if compound_cost.number_per is not None:
                    #             cost_total += compound_cost.number_per * units
                    #         unit_cost = cost_total / abs(units)
                    #     else:
                    #         unit_cost = compound_cost.number_per
                    #     cost = Amount(unit_cost, compound_cost.currency)

                    #     # print(cost, 'against', balances[posting.account])

                    #     lot = Lot(lot_spec.currency, cost)
                    #     stats.num_lots_atcost += 1
                    #     # print(posting.position)
                    # else:
                    #     # As per the parser, this cannot happen.
                    #     assert (lot_spec.lot_date is None and
                    #             lot_spec.label is None and
                    #             lot_spec.merge is None), "Internal error"

                    #     # This is a simple position.
                    #     lot = Lot(lot_spec.currency, None, None)

                balance = balances[posting.account]
                balance.add_position(posting.position)

    logging.info("Interpolation Stats: %s", stats)

    return entries, errors


# An error raised if we failed to bucket a posting to a particular currency.
CategorizationError = collections.namedtuple('CategorizationError', 'source message entry')

def categorize_by_currency(entry, balances):
    """Group the postings by the currency they declare.

    This is used to prepare the postings for the next stages: Interpolation and
    booking will then be carried out separately on each currency group. At the
    outset of this routine, we should have distinct groups of currencies without
    any ambiguities regarding which currency they need to balance against.

    Here's how this works. Postings with no cost come in two varieties:

      1. No cost, with currency, e.g.
           Assets:Something       213.45 USD
         or
           Assets:Something              USD
         This is obvious, it buckets into the units currency, i.e., USD.

      2. No cost, no currency, e.g.
           Assets:Something
         This is an auto-posting. One of these should be replicated for every
         currency present in the transaction.

    Then, we have postings with costs, which also come in two varieties:

      3. With an explicit cost currency, e.g.
           Assets:Something       100 HOOL {12.23 USD}
         Or with missing amounts, e.g.,
           Assets:Something       100 HOOL {USD}
         This clearly goes into the USD bucket.

      4. With no explicit cost currency, e.g.,
           Assets:Something       100 HOOL {2014-09-30}
           Assets:Something       100 HOOL {"1b24b1151261"}
           Assets:Something       100 HOOL {}
         These are uncategorized.

         In order to resolve these postings to a specific currency bucket, we
         implement two heuristics:

         a) If all the other legs are of a single currency and there are no
            other uncategorized legs, this posting must also book against those;
            use that currency.

         b) Otherwise, look at the accumulated ante-inventory; if there is a
            single currency for it, the posting must be in that currency.

         Finally, if we aren't able to resolve the currency of that posting
         using (a) or (b), fail interpolation/booking and skip the transaction.

    With that algorithm, we should be able to automatically resolve stock splits
    that look like this, as long as the ante-inventory contains only lots in
    USD:

      2015-09-30 * "Split"
        Assets:Investments:AAPL       -40 AAPL {}
        Assets:Investments:AAPL        80 AAPL {}

    Args:
      postings: A list of incomplete postings to categorize.
      balances: A dict of currency to inventory contents.
    Returns:
      A dict of currency (string) to indexes of the postings. Note that for
      auto-postings (postings without a currency nor cost) - the index may
      appear in multiple groups and the posting need to be duplicated for each
      currency that requires them.
    """
    errors = []

    groups = collections.defaultdict(set)
    auto_postings = []
    unknown = []
    for index, posting in enumerate(entry.postings):
        pos = posting.position

        # If the posting if unspecified, go to the free list to be processed
        # after this loop.
        if pos is MISSING or pos is None:
            unknown.append(posting)
            continue

        if pos.cost is None:
            # Deal with postings with no cost.

            # If the posting has an explicit currency, just use it.
            currency = pos.units.currency
            assert currency is not MISSING
            groups[currency].add(index)
            continue

        else:
            # Deal with postings that have cost.

            # If the posting has an explicit cost currency, just use it.
            cost_currency = pos.cost.currency
            if cost_currency is not MISSING:
                groups[cost_currency].add(index)
                continue

        errors.append(
            CategorizationError(posting.meta, "Failed to categorize posting", entry))
        continue


        # # Categorize based on the lot spec.
        # cost_spec = pos.cost
        # if cost_spec is not None:
        #     if cost_spec.currency:
        #         groups[cost_spec.currency].append(posting)
        #     else:
        #         unknown.append(posting)
        # else:
        #     groups[pos.units.currency].append(posting)

    # FIMXE: Maybe duplicate free postings here for each group so you don't have
    # to return a list of free postings? That would make sense.

    return groups, errors






#------------------------------------------------------------------------------------------------------------------------
"""Implementation notes:

Between book and interpolation:

- You can't perform interpolation first, because the booked cost basis will
  provide necessary amounts to fill in for interpolation.

- You can perform booking first, though there may be cases where interppolation
  would yield a number that could disambiguate booking. For example, consider
  this case:

    With an inventory of 100.00 USD and 101.00 USD shares.

    2015-09-30 *
      Assets:Investments   -10 HOOL {USD}
      Assets:Cash               1000 USD

  If you performed interpolation beforehand you could back out a cost of 100.00 USD
  and then the cost booking would be unambiguous.

We would like to be able to infer those cases. So maybe we can
- Perform a simple, partial interpolation, where possible.
- Do the booking
- Perform the remaining, full interpolation (with a resolution required).

Separate the augmenting legs from the reducing legs. Reducing legs may allow
less DOF because they have to match against the inventory.

"""
