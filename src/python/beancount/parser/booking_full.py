"""Full (new) booking implementation.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import logging
import pprint

from beancount.core.number import MISSING
from beancount.core.data import Transaction
from beancount.core.amount import Amount
from beancount.core import amount
from beancount.core import position
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

            # Group postings by currency.
            refer_groups, cat_errors = categorize_by_currency(entry, balances)
            if cat_errors:
                errors.extend(cat_errors)
                continue
            posting_groups = replace_currencies(entry.postings, refer_groups)

            # Resolve each group of postings.
            ## FIXME: TODO
            postings = entry.postings

            # Update running balances using the interpolated values.
            for posting in postings:
                balance = balances[posting.account]
                balance.add_position(posting.position)

    #logging.info("Interpolation Stats: %s", stats)

    return entries, errors


# An error raised if we failed to bucket a posting to a particular currency.
CategorizationError = collections.namedtuple('CategorizationError', 'source message entry')


def get_bucket_currency(refer):
    """Given currency references for a posting, return the bucket currency.

    Args:
      refer: An instance of Refer.
    Returns:
      A currency string.
    """
    currency = None
    if isinstance(refer.cost_currency, str):
        currency = refer.cost_currency
    elif isinstance(refer.price_currency, str):
        currency = refer.price_currency
    elif (refer.cost_currency is None and
          refer.price_currency is None and
          isinstance(refer.units_currency, str)):
        currency = refer.units_currency
    return currency

Refer = collections.namedtuple('Refer', 'index units_currency cost_currency price_currency')


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
      A dict of currency (string) to a list of tuples describing each postings
      and its interpolated currencies, and a list of generated errors for
      currency interpolation. The entry's original postings are left unmodified.
      Each tuple in the value-list contains:
        index: The posting index in the original entry.
        units_currency: The interpolated currency for units.
        cost_currency: The interpolated currency for cost.
        price_currency: The interpolated currency for price.
    """
    errors = []

    groups = collections.defaultdict(list)
    auto_postings = []
    unknown = []
    for index, posting in enumerate(entry.postings):
        pos = posting.position

        # Extract and override the currencies locally.
        units_currency = (pos.units.currency
                          if pos is not MISSING
                          else None)
        cost_currency = (pos.cost.currency
                         if pos is not MISSING and pos.cost is not None
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

        refer = Refer(index, units_currency, cost_currency, price_currency)

        if pos is MISSING and price_currency is None:
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

        refer = Refer(index, units_currency, cost_currency, price_currency)
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

        refer = Refer(index, units_currency, cost_currency, price_currency)
        currency = get_bucket_currency(refer)
        if currency is not None:
            groups[currency].append(refer)
        else:
            errors.append(
                CategorizationError(posting.meta,
                                    "Failed to categorize posting {}".format(index),
                                    entry))

    # Fill in missing units currencies if some remain as missing. This may occur
    # if we used the cost or price to bucket the currency but the units currency
    # was missing.
    for currency, refers in groups.items():
        for ri, refer in enumerate(refers):
            if refer.units_currency is MISSING:
                posting = entry.postings[refer.index]
                balance = balances.get(posting.account, None)
                if balance is None:
                    continue
                balance_currencies = balance.currencies()
                if len(balance_currencies) == 1:
                    refers[ri] = refer._replace(units_currency=balance_currencies.pop())

    # Deal with auto-postings.
    if len(auto_postings) > 1:
        refer = auto_postings[-1]
        posting = entry.postings[refer.index]
        errors.append(
            CategorizationError(posting.meta,
                                "You may not have more than one auto-posting", entry))
        auto_postings = auto_postings[0:1]
    for refer in auto_postings:
        for currency in groups.keys():
            groups[currency].append(Refer(refer.index, currency, None, None))

    # Issue error for all currencies which we could not resolve.
    for currency, refers in groups.items():
        for refer in refers:
            posting = entry.postings[refer.index]
            for currency, name in [(refer.units_currency, 'units'),
                                   (refer.cost_currency, 'cost'),
                                   (refer.price_currency, 'price')]:
                if currency is MISSING:
                    errors.append(CategorizationError(
                        posting.meta,
                        "Could not resolve {} currency".format(name),
                        entry))

    return groups, errors


def replace_currencies(postings, refer_groups):
    """Replace resolved currencies in the entry's Postings.

    Args:
      postings: A list of Posting instances to replace.
      refer_groups: A dict of currency to list of posting references as per
        categorize_by_currency().
    Returns:
      A new mapping of currency to a list of Postings, postings for which the
      currencies have been replaced by their interpolated currency values.
    """
    new_groups = {}
    for currency, refers in refer_groups.items():
        new_postings = []
        for refer in sorted(refers, key=lambda r: r.index):
            posting = postings[refer.index]
            pos = posting.position
            if pos is MISSING:
                posting = posting._replace(position=position.Position(
                    Amount(MISSING, refer.units_currency)))
            else:
                replace = False
                units = pos.units
                cost = pos.cost
                price = posting.price
                if units.currency is MISSING:
                    units = Amount(units.number, refer.units_currency)
                    replace = True
                if cost and cost.currency is MISSING:
                    cost = cost._replace(currency=refer.cost_currency)
                    replace = True
                if price and price.currency is MISSING:
                    price = Amount(price.number, refer.price_currency)
                    replace = True
                if replace:
                    posting = posting._replace(position=position.Position(units, cost),
                                               price=price)
            new_postings.append(posting)
        new_groups[currency] = new_postings
    return new_groups


def interpolate_group(postings, balances):
    """Interpolate missing numbers in the set of postings.

    Args:
      postings: A list of Posting instances.
      balances: A dict of account to its ante-inventory.
    Returns:
      A list of new posting instances and a list of errors.
    """
    errors = []
    new_postings = []
    for posting in postings:
        print(posting)
        new_postings.append(posting)
    return new_posting





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
