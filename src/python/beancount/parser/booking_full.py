"""Full (new) booking implementation.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import logging
import pprint

# pylint: disable=invalid-name
try:
    import enum
    Enum = enum.Enum
except ImportError:
    Enum = object

from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.core.data import Transaction
from beancount.core.amount import Amount
from beancount.core.position import Position
from beancount.core.position import Cost
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import interpolate



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
                balance.add_position(posting)

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


class MissingType(Enum):
    """The type of missing number."""
    UNITS      = 1
    COST_PER   = 2
    COST_TOTAL = 3
    PRICE      = 4


# An error raised if we are not able to interpolate.
InterpolationError = collections.namedtuple('InterpolationError', 'source message entry')


def interpolate_group(postings, balances, currency):
    """Interpolate missing numbers in the set of postings.

    Args:
      postings: A list of Posting instances.
      balances: A dict of account to its ante-inventory.
      currency: The weight currency of this group, used for reporting errors.
    Returns:
      A tuple of
        postings: A lit of new posting instances.
        errors: A list of errors generated during interpolation.
        interpolated: A boolean, true if we did have to interpolate.

      In the case of an error, this returns the original list of postings, which
      is still incomplete. If an error is returned, you should probably skip the
      transaction altogether, or just not include the postings in it. (An
      alternative behaviour would be to return only the list of valid postings,
      but that would likely result in an unbalanced transaction. We do it this
      way by choice.)
    """
    errors = []

    # Figure out which type of amount is missing, by creating a list of
    # incomplete postings and which type of units is missing.
    incomplete = []
    balance = inventory.Inventory()
    for index, posting in enumerate(postings):
        pos = posting.position

        # Identify incomplete postings.
        pre_incomplete = len(incomplete)
        if pos.units.number is MISSING:
            incomplete.append((MissingType.UNITS, index))
        if pos.cost and pos.cost.number_per is MISSING:
            incomplete.append((MissingType.COST_PER, index))
        if pos.cost and pos.cost.number_total is MISSING:
            incomplete.append((MissingType.COST_TOTAL, index))
        if posting.price and posting.price.number is MISSING:
            incomplete.append((MissingType.PRICE, index))

    if len(incomplete) == 0:
        # If there are no missing numbers, return the original list of postings.
        return postings, errors, False

    elif len(incomplete) > 1:
        # If there is more than a single value to be interpolated, generate an
        # error.
        _, posting_index = incomplete[0]
        errors.append(InterpolationError(
            postings[posting_index].meta,
            "Too many missing numbers for currency group '{}'".format(currency),
            None))
        return postings, errors, False

    else:
        # If there is a single missing number, calculate it and fill it in here.
        missing, index = incomplete[0]
        incomplete_posting = postings[index]

        # Compute the balance of the other postings.
        residual = interpolate.compute_residual(posting
                                                for posting in postings
                                                if posting is not incomplete_posting)
        assert len(residual) < 2, "Internal error in grouping postings by currencies."
        if not residual.is_empty():
            respos = residual[0]
            assert respos.cost is None, (
                "Internal error; cost appears in weight calculation.")
            assert respos.units.currency == currency, (
                "Internal error; residual different than currency group.")
            weight = -respos.units.number
            weight_currency = respos.units.currency
        else:
            weight = ZERO
            weight_currency = currency

        if missing == MissingType.UNITS:
            pos = incomplete_posting.position
            cost = pos.cost
            if cost:
                # Handle the special case where we only have total cost.
                if cost.number_per == ZERO:
                    errors.append(InterpolationError(
                        incomplete_posting.meta,
                        "Cannot infer per-unit cost only from total", None))
                    return postings, errors, True

                assert cost.currency == weight_currency, (
                    "Internal error; residual currency different than missing currency.")
                cost_total = cost.number_total or ZERO
                units_number = (weight - cost_total) / cost.number_per
            elif incomplete_posting.price:
                assert incomplete_posting.price.currency == weight_currency, (
                    "Internal error; residual currency different than missing currency.")
                units_number = weight / incomplete_posting.price.number
            else:
                assert pos.units.currency == weight_currency, (
                    "Internal error; residual currency different than missing currency.")
                units_number = weight
            new_pos = Position(Amount(units_number, pos.units.currency), cost)
            new_posting  = incomplete_posting._replace(position=new_pos)

        elif missing == MissingType.COST_PER:
            pos = incomplete_posting.position
            assert pos.cost.currency == weight_currency, (
                "Internal error; residual currency different than missing currency.")
            units = pos.units
            cost = pos.cost
            number_per = (weight - (cost.number_total or ZERO)) / units.number
            new_cost = pos.cost._replace(number_per=number_per)
            new_pos = Position(units, new_cost)
            new_posting  = incomplete_posting._replace(position=new_pos)

        elif missing == MissingType.COST_TOTAL:
            pos = incomplete_posting.position
            assert pos.cost.currency == weight_currency, (
                "Internal error; residual currency different than missing currency.")
            units = pos.units
            cost = pos.cost
            number_total = (weight - cost.number_per * units.number)
            new_cost = pos.cost._replace(number_total=number_total)
            new_pos = Position(units, new_cost)
            new_posting  = incomplete_posting._replace(position=new_pos)

        elif missing == MissingType.PRICE:
            pos = incomplete_posting.position
            if pos.cost is not None:
                errors.append(InterpolationError(
                    incomplete_posting.meta,
                    "Cannot infer price for postings with units held at cost", None))
                return postings, errors, True
            else:
                price = incomplete_posting.price
                assert price.currency == weight_currency, (
                    "Internal error; residual currency different than missing currency.")
                units = pos.units
                new_price_number = abs(units.number / weight)
                new_posting  = incomplete_posting._replace(price=Amount(new_price_number,
                                                                        price.currency))

        else:
            assert False, "Internal error; Invalid missing type."

        if 0:
            # Convert the CostSpec instance into a corresponding Cost.
            pos = new_posting.position
            cost = pos.cost
            if cost is not None:
                units = pos.units.number
                number_per = cost.number_per
                number_total = cost.number_total
                if number_total is not None:
                    # Compute the per-unit cost if there is some total cost
                    # component involved.
                    cost_total = number_total
                    if number_per is not MISSING:
                        cost_total += number_per * units
                    unit_cost = cost_total / abs(units)
                else:
                    unit_cost = number_per
                new_cost = Cost(unit_cost, cost.currency, cost.date, cost.label)
                new_posting = new_posting._replace(
                    position=position.Position(pos.units, new_cost))

        # Replace the number in the posting.
        postings = list(postings)
        postings[index] = new_posting

        return postings, errors, True



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
