"""Full (new) booking implementation.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import logging
import pprint

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


def get_bucket_currency(refer):
    _, units_currency, cost_currency, price_currency = refer
    currency = None
    if isinstance(cost_currency, str):
        currency = cost_currency
    elif isinstance(price_currency, str):
        currency = price_currency
    elif (cost_currency is None and
          price_currency is None and
          isinstance(units_currency, str)):
        currency = units_currency
    return currency


def categorize_by_currency(entry, balances):
    """Group the postings by the currency they declare.

    This is used to prepare the postings for the next stages: Interpolation and
    booking will then be carried out separately on each currency group. At the
    outset of this routine, we should have distinct groups of currencies without
    any ambiguities regarding which currency they need to balance against.

    Here's how this works.

    - First we apply the constraint that cost-currency and price-currency must
      match, if there is both a cost and a price. This reduces the space of
      possibilities somewahte.

    - If the currency is explicitly specified, we put the posting in that
      currency's bucket.

    - If not, we have a few methods left to disambiguate the currency:

      1. We look at the remaining postings... if they are all of a single
         currency, the posting must be in that currency too.

      2. If we cannot do that, we inspect the contents of the inventory of the
         account for the posting. If all the contents are of a single currency,
         we use that one.

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

    groups = collections.defaultdict(list)
    auto_postings = []
    unknown = []
    for index, posting in enumerate(entry.postings):
        position = posting.position

        # Extract and override the currencies locally.
        units_currency = (position.units.currency
                          if position is not MISSING
                          else None)
        cost_currency = (position.cost.currency
                         if position is not MISSING and position.cost is not None
                         else None)
        price_currency = (posting.price.currency
                          if posting.price is not None
                          else None)

        # First we apply the constraint that cost-currency and price-currency
        # must match, if there is both a cost and a price. This reduces the
        # space of possibilities somewhat.
        if cost_currency is MISSING and isinstance(price_currency, str):
            cost_currency = price_currency
        if price_currency is MISSING and isinstance(cost_currency, str):
            price_currency = cost_currency

        refer = (index, units_currency, cost_currency, price_currency)

        if position is MISSING and price_currency is None:
            # Bucket auto-postings separately from unknown.
            auto_postings.append(refer)
        else:
            # Bucket with what we know so far.
            currency = get_bucket_currency(refer)
            if currency is not None:
                groups[currency].append(refer)
            else:
                # If we need to infer the currency, store in unknown.
                unknown.append(refer)

    # We look at the remaining postings... if they are all of a single currency,
    # the posting must be in that currency too.
    if unknown and len(unknown) == 1 and len(groups) == 1:
        (index, units_currency, cost_currency, price_currency) = unknown.pop()

        other_currency = next(iter(groups.keys()))
        if price_currency is None and cost_currency is None:
            # Infer to the units currency.
            units_currency = other_currency
        else:
            # Infer to the cost and price currencies.
            if price_currency is MISSING:
                price_currency = other_currency
            if cost_currency is MISSING:
                cost_currency = other_currency

        refer = (index, units_currency, cost_currency, price_currency)
        currency = get_bucket_currency(refer)
        assert currency is not None
        groups[currency].append(refer)

    # Finally, try to resolve all the unknown legs using the inventory contents
    # of each account.
    for refer in unknown:
        (index, units_currency, cost_currency, price_currency) = unknown.pop()
        posting = entry.postings[index]
        balance = balances.get(posting.account, None)
        if balance is None:
            balance = inventory.Inventory()

        if units_currency is MISSING:
            balance_currencies = balance.currencies()
            if len(balance_currencies) == 1:
                units_currency = balance_currencies.pop()

        if cost_currency is MISSING or price_currency is MISSING:
            balance_cost_currencies = balance.cost_currencies()
            if len(balance_cost_currencies) == 1:
                balance_cost_currency = balance_cost_currencies.pop()
                if price_currency is MISSING:
                    price_currency = balance_cost_currency
                if cost_currency is MISSING:
                    cost_currency = balance_cost_currency

        refer = (index, units_currency, cost_currency, price_currency)
        currency = get_bucket_currency(refer)
        if currency is not None:
            groups[currency].append(refer)
        else:
            errors.append(
                CategorizationError(posting.meta, "Failed to categorize posting", entry))

    # Fill in missing units currencies if some remain as missing. This may occur
    # if we used the cost or price to bucket the currency but the units currency
    # was missing.
    for currency, refers in groups.items():
        for ri, refer in enumerate(refers):
            index, units_currency, cost_currency, price_currency = refer
            if units_currency is MISSING:
                posting = entry.postings[index]
                balance = balances.get(posting.account, None)
                if balance is None:
                    continue
                balance_currencies = balance.currencies()
                if len(balance_currencies) == 1:
                    units_currency = balance_currencies.pop()
                    new_refer = index, units_currency, cost_currency, price_currency
                    refers[ri] = new_refer

    # Deal with auto-postings.
    if len(auto_postings) > 1:
        index, _, __, ___ = auto_postings[-1]
        posting = entry.postings[index]
        errors.append(
            CategorizationError(posting.meta,
                                "You may not have more than one auto-posting", entry))
        auto_postings = auto_postings[0:1]
    for refer in auto_postings:
        index, _, __, ___ = refer
        for currency in groups.keys():
            groups[currency].append((index, currency, None, None))

    # Issue error for all currencies which we could not resolve.
    for currency, refers in groups.items():
        for refer in refers:
            index, units_currency, cost_currency, price_currency = refer
            posting = entry.postings[index]
            if units_currency is MISSING:
                errors.append(
                    CategorizationError(posting.meta,
                                        "Could not resolve currency", entry))

    index_groups = {currency: {refer[0] for refer in refers}
                    for currency, refers in groups.items()}

    return index_groups, errors






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



"""



    varieties:

      1. No cost, no price, with currency, e.g.
           Assets:Something       213.45 USD
         or
           Assets:Something              USD
         This is obvious, it buckets into the units currency, i.e., USD.

      2. No cost, no price, no currency, e.g.
           Assets:Something
         This is an auto-posting. One of these should be replicated for every
         currency present in the transaction.


    Postings with a price define their currency:

      3. No cost with price:
           Assets:Something       1000 JPY @ 120.0000 USD
         or
           Assets:Something       1000 JPY @          USD
         We use the price currency, e.g. USD

      4. No cost and no price currency:
           Assets:Something       1000 JPY @
         In this case, we must consult the other postings. We look


    Then, we have postings with costs, which also come in two varieties:

      5. With an explicit cost currency, e.g.
           Assets:Something       100 HOOL {12.23 USD}
         Or with missing amounts, e.g.,
           Assets:Something       100 HOOL {USD}
         This clearly goes into the USD bucket.

      6. With no explicit cost currency, e.g.,
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

    Finally, note that postings with both a cost and a price must have a
    currency that matches, as constrained by the parser. If only the price or
    the cost is specified, we used that currency. Both a price and a cost may
    not be missing--that would leave two DOF to fill in.

"""
