"""Full (new) booking implementation.

Problem description:

Interpolation and booking feed on each other, that is, numbers filled in from
interpolation might affect the booking process, and numbers derived from the
booking process may help carry out interpolation that would otherwise be
under-defined. Here's an example of interpolation helping the booking process:

Assume the ante-inventory of Assets:Investments contains two lots of shares of
HOOL, one at 100.00 USD and the other at 101.00 USD and apply this transaction:

    2015-09-30 *
      Assets:Investments   -10 HOOL {USD}
      Assets:Cash               1000 USD
      Income:Gains              -200 USD

Interpolation is unambiguously able to back out a cost of 100 USD / HOOL, which
would then result in an unambiguous booking result.

On the other hand, consider this transaction:

    2015-09-30 *
      Assets:Investments    -10 HOOL {USD}
      Assets:Cash               1000 USD
      Income:Gains

Now the interpolation cannot succeed. If the Assets:Investments accoujnt is
configured to use the FIFO method, the 10 oldest shares would be selected for
the cost, and we could then interpolate the capital gains correctly.

First observation: The second case is much more frequent than the first, and the
first is easily resolved manually by requiring a particular cost be specified.
Moreover, in many cases there isn't just a single lot of shares to be reduced
from and figuring out the correct set of shares given a target cost is an
underspecified problem.

Second observation: Booking can only be achieved for inventory reductions, not
for augmentations. Therefore, we should carry out booking on inventory
reductions and fail early if reduction is undefined there, and leave inventory
augmentations with missing numbers undefined, so that interpolation can fill
them in at a later stage.

Note that one case we'd like to but may not be able to handle is of a reduction
with interpolated price, like this:

    2015-09-30 *
      Assets:Investments        -10 HOOL {100.00 # USD}
      Expenses:Commission      9.95 USD
      Assets:Cash            990.05 USD

Therefore we choose to

1) Carry out booking first, on inventory reductions only, and leave inventory
   augmentations as they are, possibly undefined. The 'cost' attributed of
   booked postings are converted from CostSpec to Cost. Augmented postings with
   missing amounts are left as CostSpec instances in order to allow for
   interpolation of total vs. per-unit amount.

2) Compute interpolations on the resulting postings. Undefined costs for
   inventory augmentations may be filled in by interpolations at this stage (if
   possible).

3) Finally, convert the interpolated CostSpec instances to Cost instances.

Improving on this algorithm would require running a loop over the booking and
interpolation steps until all numbers are resolved or no more inference can
occur. We may consider that for later, as an experimental feature. My hunch is
that there are so few cases for which this would be useful that we won't bother
improving on the algorithm above.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections

from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.core.data import Transaction
from beancount.core.amount import Amount
from beancount.core.position import Position
from beancount.core.position import Cost
from beancount.core import position
from beancount.core import inventory
from beancount.core import interpolate
from beancount.utils import misc_utils


FullBookingError = collections.namedtuple('FullBookingError', 'source message entry')


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
            repl_postings = []
            for currency, postings in posting_groups.items():
                # Perform booking reductions.
                ### FIXME: Bring this in after unit-testing.
                ### book_reductions(postings, balances)

                # Interpolate missing numbers.
                new_postings, errors, interpolated = interpolate_group(postings,
                                                                       balances,
                                                                       currency)
                repl_postings.extend(new_postings)

            # Replace postings by interpolated ones.
            entry.postings[:] = repl_postings

            # Update running balances using the interpolated values.
            for posting in repl_postings:
                balance = balances[posting.account]
                balance.add_position(posting)

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
        units = posting.units
        cost = posting.cost
        price = posting.price

        # Extract and override the currencies locally.
        units_currency = (units.currency
                          if units is not MISSING and units is not None
                          else None)
        cost_currency = (cost.currency
                         if cost is not MISSING and cost is not None
                         else None)
        price_currency = (price.currency
                          if price is not MISSING and price is not None
                          else None)

        # First we apply the constraint that cost-currency and price-currency
        # must match, if there is both a cost and a price. This reduces the
        # space of possibilities somewhat.
        if cost_currency is MISSING and isinstance(price_currency, str):
            cost_currency = price_currency
        if price_currency is MISSING and isinstance(cost_currency, str):
            price_currency = cost_currency

        refer = Refer(index, units_currency, cost_currency, price_currency)

        if units is MISSING and price_currency is None:
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
        for rindex, refer in enumerate(refers):
            if refer.units_currency is MISSING:
                posting = entry.postings[refer.index]
                balance = balances.get(posting.account, None)
                if balance is None:
                    continue
                balance_currencies = balance.currencies()
                if len(balance_currencies) == 1:
                    refers[rindex] = refer._replace(units_currency=balance_currencies.pop())

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
            units = posting.units
            if units is MISSING:
                posting = posting._replace(units=Amount(MISSING, refer.units_currency))
            else:
                replace = False
                cost = posting.cost
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
                    posting = posting._replace(units=units, cost=cost, price=price)
            new_postings.append(posting)
        new_groups[currency] = new_postings
    return new_groups


def book_reductions(postings, balances):
    """Book inventory reductions against the ante balances.

    Args:
      postings: A list of postings.
      balances: A dict of account name to inventory contents.
    Returns:
      A pair of
        new_postings: A list of postings, with reductions resolved against their
          inventory balances.
        modified_balances: A dict of the update balances. This can be used to
          update the state of the global balances (which is left untouched).
    """
    empty = inventory.Inventory()
    new_postings = []
    for posting in postings:

        units = posting.units
        costspec = posting.cost
        balance = balances.get(posting.account, None)
        if (costspec is not None and
            balance is not None and
            balance.is_reduced_by(units)):
            cost_number = compute_cost_number(costspec, units.number)
            if cost_number is not MISSING:

                try:
                    balance = balances[posting.account]
                except KeyError:
                    balance = empty

                # FIXME: We need to create and invoke some sort of partial
                # matching from CostSpec here.
                posting = posting._replace(cost=position.Cost(cost_number,
                                                              costspec.currency,
                                                              costspec.date,
                                                              costspec.label))

        # FIXME: Do we need to update the balances here in the case it's not a
        # reduction?
        new_postings.append(posting)

    return new_postings, {}


def compute_cost_number(costspec, units_number):
    number_per = costspec.number_per
    number_total = costspec.number_total
    if MISSING in (number_per, number_total):
        return MISSING
    if number_total is not None:
        # Compute the per-unit cost if there is some total cost
        # component involved.
        cost_total = number_total
        if number_per is not None:
            cost_total += number_per * units_number
        unit_cost = cost_total / abs(units_number)
    else:
        unit_cost = number_per
    return unit_cost




class MissingType(misc_utils.Enum):
    """The type of missing number."""
    UNITS = 1
    COST_PER = 2
    COST_TOTAL = 3
    PRICE = 4


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
    for index, posting in enumerate(postings):
        units = posting.units
        cost = posting.cost
        price = posting.price

        # Identify incomplete postings.
        if units.number is MISSING:
            incomplete.append((MissingType.UNITS, index))
        if cost and cost.number_per is MISSING:
            incomplete.append((MissingType.COST_PER, index))
        if cost and cost.number_total is MISSING:
            incomplete.append((MissingType.COST_TOTAL, index))
        if price and price.number is MISSING:
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
            units = incomplete_posting.units
            cost = incomplete_posting.cost
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
                assert units.currency == weight_currency, (
                    "Internal error; residual currency different than missing currency.")
                units_number = weight
            new_pos = Position(Amount(units_number, units.currency), cost)
            new_posting = incomplete_posting._replace(units=new_pos.units,
                                                      cost=new_pos.cost)

        elif missing == MissingType.COST_PER:
            units = incomplete_posting.units
            cost = incomplete_posting.cost
            assert cost.currency == weight_currency, (
                "Internal error; residual currency different than missing currency.")
            number_per = (weight - (cost.number_total or ZERO)) / units.number
            new_cost = cost._replace(number_per=number_per)
            new_pos = Position(units, new_cost)
            new_posting = incomplete_posting._replace(units=new_pos.units,
                                                      cost=new_pos.cost)

        elif missing == MissingType.COST_TOTAL:
            units = incomplete_posting.units
            cost = incomplete_posting.cost
            assert cost.currency == weight_currency, (
                "Internal error; residual currency different than missing currency.")
            number_total = (weight - cost.number_per * units.number)
            new_cost = cost._replace(number_total=number_total)
            new_pos = Position(units, new_cost)
            new_posting = incomplete_posting._replace(units=new_pos.units,
                                                      cost=new_pos.cost)

        elif missing == MissingType.PRICE:
            units = incomplete_posting.units
            cost = incomplete_posting.cost
            if cost is not None:
                errors.append(InterpolationError(
                    incomplete_posting.meta,
                    "Cannot infer price for postings with units held at cost", None))
                return postings, errors, True
            else:
                price = incomplete_posting.price
                assert price.currency == weight_currency, (
                    "Internal error; residual currency different than missing currency.")
                new_price_number = abs(units.number / weight)
                new_posting = incomplete_posting._replace(price=Amount(new_price_number,
                                                                       price.currency))

        else:
            assert False, "Internal error; Invalid missing type."

        # Convert the CostSpec instance into a corresponding Cost.
        units = new_posting.units
        cost = new_posting.cost
        if cost is not None:
            units_number = units.number
            number_per = cost.number_per
            number_total = cost.number_total
            if number_total is not None:
                # Compute the per-unit cost if there is some total cost
                # component involved.
                cost_total = number_total
                if number_per is not MISSING:
                    cost_total += number_per * units_number
                unit_cost = cost_total / abs(units_number)
            else:
                unit_cost = number_per
            new_cost = Cost(unit_cost, cost.currency, cost.date, cost.label)
            new_posting = new_posting._replace(units=units, cost=new_cost)

        # Replace the number in the posting.
        postings = list(postings)
        postings[index] = new_posting

        return postings, errors, True
