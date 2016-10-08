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
import copy
import io
import logging
import pprint

from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.core.number import Decimal
from beancount.core.data import Transaction
from beancount.core.data import Booking
from beancount.core.data import Posting
from beancount.core.amount import Amount
from beancount.core.position import Position
from beancount.core.position import Cost
from beancount.core.position import CostSpec
from beancount.core import flags
from beancount.core import position
from beancount.core import inventory
from beancount.core import interpolate
from beancount.utils import misc_utils
from beancount.parser import printer
from beancount.parser import booking_simple as bs


def book(entries, options_map, booking_methods):
    """Interpolate missing data from the entries using the full historical algorithm.
    See the internal implementation _book() for details.
    This method only stripes some of the return values.

    See _book() for arguments and return values.
    """
    entries, errors, _ = _book(entries, options_map, booking_methods)
    return entries, errors


def _book(entries, options_map, booking_methods):
    """Interpolate missing data from the entries using the full historical algorithm.

    Args:
      incomplete_entries: A list of directives, with some postings possibly left
        with incomplete amounts as produced by the parser.
      options_map: An options dict as produced by the parser.
      booking_methods: A mapping of account name to their corresponding booking
        method.
    Returns:
      A triple of
        entries: A list of interpolated entries with all their postings completed.
        errors: New errors produced during interpolation.
        balances: A dict of account name and resulting balances.
    """
    new_entries = []
    errors = []
    balances = collections.defaultdict(inventory.Inventory)
    for entry in entries:
        if isinstance(entry, Transaction):
            # Group postings by currency.
            refer_groups, cat_errors = categorize_by_currency(entry, balances)
            if cat_errors:
                errors.extend(cat_errors)
                continue
            posting_groups = replace_currencies(entry.postings, refer_groups)

            # Resolve reductions to a particular lot in their inventory balance.
            repl_postings = []
            for currency, group_postings in posting_groups.items():
                # Important note: the group of 'postings' here is a subset of
                # that from entry.postings, and may include replicated
                # auto-postings. Never use entry.postings further on.

                # Perform booking reductions, that is, match postings which
                # reduce the ante-inventory of their accounts to an existing
                # position in the inventory against a possibly incomplete
                # CostSpec specification, and replace the postings' cost to the
                # fully-specified (with a date & label) existing Cost instance.
                # Note that 'balances' remains untouched.
                #
                # Also note that 'booked_postings' may include augmenting
                # postings whose 'cost' attribute has been left to a CostSpec
                # instance. Therefore, the postings held-at-cost may hold a
                # mixture of Cost and CostSpec instances. This is necessary to
                # let the interpolation do its magic on partially incomplete
                # CostSpec instances below.
                (booked_postings,
                 booking_errors) = book_reductions(entry, group_postings, balances,
                                                   booking_methods)

                # If there were any errors, skip this group of postings.
                if booking_errors:
                    errors.extend(booking_errors)
                    continue

                # Interpolate missing numbers from all postings. This
                # includes partially incomplete CostSpec instances remaining
                # on augmenting postings. After this interpolation, all
                # 'inter_postings' consists entirely of postings holding
                # instances of Cost.
                (inter_postings,
                 interpolation_errors,
                 interpolated) = interpolate_group(booked_postings, balances, currency)

                if interpolation_errors:
                    errors.extend(interpolation_errors)
                repl_postings.extend(inter_postings)

            # Replace postings by interpolated ones.
            entry = entry._replace(postings=repl_postings)

            # Update the running balances for each account using the final,
            # booked and interpolated values. Note that we could optimize away
            # some of this in book_reductions() but we choose not to do so, as a
            # sanity check that the direct aggregation of the final booked lots
            # will compute the same result as that during the book_reductions()
            # process.
            for posting in repl_postings:
                balance = balances[posting.account]
                balance.add_position(posting)

        new_entries.append(entry)

    return new_entries, errors, balances


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
      balances: A dict of currency to inventory contents before the transaction is
        applied.
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
        (index, units_currency, cost_currency, price_currency) = refer
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
                                "You may not have more than one auto-posting per currency",
                                entry))
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

    This essentially applies the findings of categorize_by_currency() to produce
    new postings with all currencies resolved.

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


# An error raised if we failed to reduce the inventory balance unambiguously.
ReductionError = collections.namedtuple('ReductionError', 'source message entry')


def book_reductions(entry, group_postings, balances,
                    booking_methods):
    """Book inventory reductions against the ante-balances.

    This function accepts a dict of (account, Inventory balance) and for each
    posting that is a reduction against its inventory, attempts to find a
    corresponding lot or list of lots to reduce the balance with.

    * For reducing lots, the CostSpec instance of the posting is replaced by a
      Cost instance.

    * For augmenting lots, the CostSpec instance of the posting is left alone,
      except for its date, which is inherited from the parent Transaction.

    Args:
      entry: An instance of Transaction. This is only used to refer to when
        logging errors.
      group_postings: A list of Posting instances for the group.
      balances: A dict of account name to inventory contents.
      booking_methods: A mapping of account name to their corresponding booking
        method enum.
    Returns:
      A pair of
        booked_postings: A list of booked postings, with reducing lots resolved
          against specific position in the corresponding accounts'
          ante-inventory balances. Note single reducing posting in the input may
          result in multiple postings in the output. Also note that augmenting
          postings held-at-cost will still refer to 'cost' instances of
          CostSpec, left to be interpolated later.
        errors: A list of errors, if there were any.
    """
    errors = []

    # A local copy of the balances dictionary which is updated just for the
    # duration of this function's updates, in order to take into account the
    # cumulative effect of all the postings inferred here
    local_balances = {}

    empty = inventory.Inventory()
    booked_postings = []
    for posting in group_postings:
        # Process a single posting.
        units = posting.units
        costspec = posting.cost
        account = posting.account

        # Note: We ensure there is no mutation on 'balances' to keep this
        # function without side-effects. Note that we may be able to optimize
        # performance later on by giving up this property.
        #
        # Also note that if there is no existing balance, then won't be any lot
        # reduction because none of the postings will be able to match against
        # any currencies of the balance.
        previous_balance = balances.get(account, None)
        balance = local_balances.setdefault(account, copy.copy(previous_balance))

        # Check if this is a lot held at cost.
        if costspec is None:
            # This posting is not held at cost; we do nothing.
            booked_postings.append(posting)
        else:
            # This posting is held at cost; figure out if it's a reduction or an
            # augmentation.
            #
            # FIXME: Remove the call to is_reduced_by() and do this in the
            # following loop itself.
            booking_method = booking_methods[account]
            if (booking_method is not Booking.NONE and
                balance is not None and balance.is_reduced_by(units)):
                # This posting is a reduction.

                # Match the positions.
                cost_number = compute_cost_number(costspec, units)
                matches = []
                for position in balance:
                    # Skip inventory contents of a different currency.
                    if (units.currency and
                        position.units.currency != units.currency):
                        continue
                    # Skip balance positions not held at cost.
                    if position.cost is None:
                        continue
                    if (cost_number is not None and
                        position.cost.number != cost_number):
                        continue
                    if (isinstance(costspec.currency, str) and
                        position.cost.currency != costspec.currency):
                        continue
                    if (costspec.date and
                        position.cost.date != costspec.date):
                        continue
                    if (costspec.label and
                        position.cost.label != costspec.label):
                        continue
                    matches.append(position)

                # Check for ambiguous matches.
                if len(matches) == 0:
                    errors.append(
                        ReductionError(entry.meta,
                                       'No position matches "{}"'.format(str(posting)),
                                       entry))
                    return [], errors  # This is irreconcilable, remove these postings.

                reduction_postings, ambi_errors = handle_ambiguous_matches(
                    entry, posting, matches, booking_method)
                if ambi_errors:
                    errors.extend(ambi_errors)
                    return [], errors

                # Add the reductions to the resulting list of booked postings.
                booked_postings.extend(reduction_postings)

                # Update the local balance in order to avoid matching against
                # the same postings twice when processing multiple postings in
                # the same transaction. Note that we only do this for postings
                # held at cost because the other postings may need interpolation
                # in order to be resolved properly.
                for posting in reduction_postings:
                    balance.add_position(posting)
            else:
                # This posting is an augmentation.
                #
                # Note that we do not convert the CostSpec instances to Cost
                # instances, because we want to let the subsequent interpolation
                # process able to interpolate either the cost per-unit or the
                # total cost, separately.

                # Put in the date of the parent Transaction if there is no
                # explicit date specified on the spec.
                if costspec.date is None:
                    dated_costspec = costspec._replace(date=entry.date)
                    posting = posting._replace(cost=dated_costspec)
                booked_postings.append(posting)

        # FIXME: Do we need to update the balances here in the case it's not a
        # reduction? What if we want to reduce off of positions added on other
        # legs of this transaction itself? See {f89b5b01e568}.
        # At the very least, document this.

    return booked_postings, errors


def handle_ambiguous_matches(entry, posting, matches, booking_method):
    """Handle ambiguous matches.

    Args:
      entry: The parent Transaction instance.
      posting: An instance of Posting, the reducing posting which we're
        attempting to match.
      matches: A list of matching Position instances from the ante-inventory.
        Those positions are known to already match the 'posting' spec.
      booking_methods: A mapping of account name to their corresponding booking
        method.
    Returns:
      A pair of
        booked_postings: A list of matched Posting instances, whose 'cost'
          attributes are ensured to be of type Cost.
        errors: A list of errors to be generated.
    """
    assert isinstance(booking_method, Booking), (
        "Invalid type: {}".format(booking_method))
    assert matches, "Internal error: Invalid call with no matches"

    postings = []
    errors = []
    insufficient = False
    if booking_method is Booking.STRICT:
        # In strict mode, we require at most a single matching posting.
        if len(matches) > 1:
            errors.append(
                ReductionError(entry.meta,
                               'Ambiguous matches for "{}": {}'.format(
                                   position.to_string(posting),
                                   ', '.join(position.to_string(match_posting)
                                             for match_posting in matches)),
                               entry))
        else:
            # Replace the posting's units and cost values.
            match = matches[0]
            sign = -1 if posting.units.number < ZERO else 1
            number = min(abs(match.units.number), abs(posting.units.number))
            match_units = Amount(number * sign, match.units.currency)
            postings.append(posting._replace(units=match_units, cost=match.cost))
            insufficient = (match_units.number != posting.units.number)

    elif booking_method in (Booking.FIFO, Booking.LIFO):
        # Each up the positions.
        sign = -1 if posting.units.number < ZERO else 1
        remaining = abs(posting.units.number)
        for match in sorted(matches, key=lambda p: p.cost and p.cost.date,
                            reverse=(booking_method == Booking.LIFO)):
            if remaining <= ZERO:
                break

            # If the inventory somehow ended up with mixed lots, skip this one.
            if match.units.number * sign > ZERO:
                continue

            # Compute the amount of units we can reduce from this leg.
            size = min(abs(match.units.number), remaining)
            postings.append(posting._replace(units=Amount(size * sign, match.units.currency),
                                             cost=match.cost))
            remaining -= size

        # If we couldn't eat up all the requested reduction, return an error.
        insufficient = (remaining > ZERO)

    elif booking_method is Booking.NONE:
        # This never needs to match against any existing positions... we
        # disregard the matches, there's never any error. Note that this never
        # gets called in practice, we want to treat NONE postings as
        # augmentations. Default behaviour is to return them with their original
        # CostSpec, and the augmentation code will handle signaling an error if
        # there is insufficient detail to carry out the conversion to an
        # instance of Cost.
        postings.append(posting)

        # Note that it's an interesting question whether a reduction on an
        # account with NONE method which happens to match a single position
        # ought to be matched against it. We don't allow it for now.

    elif booking_method is Booking.AVERAGE:
        errors.append(ReductionError(entry.meta, "AVERAGE method is not supported", entry))

    elif False: # DISABLED - This is the code for AVERAGE

        # If there is more than a single match we need to ultimately merge the
        # postings. Also, if the reducing posting provides a specific cost, we
        # need to update the cost basis as well. Both of these cases are carried
        # out by removing all the matches and readding them later on.
        if len(matches) == 1 and (
                not isinstance(posting.cost.number_per, Decimal) and
                not isinstance(posting.cost.number_total, Decimal)):
            # There is no cost. Just reduce the one leg. This should be the
            # normal case if we always merge augmentations and the user lets
            # Beancount deal with the cost.
            match = matches[0]
            sign = -1 if posting.units.number < ZERO else 1
            number = min(abs(match.units.number), abs(posting.units.number))
            match_units = Amount(number * sign, match.units.currency)
            postings.append(posting._replace(units=match_units, cost=match.cost))
            insufficient = (match_units.number != posting.units.number)
        else:
            # Merge the matching postings to a single one.
            merged_units = inventory.Inventory()
            merged_cost = inventory.Inventory()
            for match in matches:
                merged_units.add_amount(match.units)
                merged_cost.add_amount(interpolate.get_posting_weight(match))
            if len(merged_units) != 1 or len(merged_cost) != 1:
                errors.append(
                    ReductionError(
                        entry.meta,
                        'Cannot merge positions in multiple currencies: {}'.format(
                            ', '.join(position.to_string(match_posting)
                                      for match_posting in matches)), entry))
            else:
                if (isinstance(posting.cost.number_per, Decimal) or
                    isinstance(posting.cost.number_total, Decimal)):
                    errors.append(
                        ReductionError(
                            entry.meta,
                            "Explicit cost reductions aren't supported yet: {}".format(
                                position.to_string(posting)), entry))
                else:
                    # Insert postings to remove all the matches.
                    postings.extend(posting._replace(units=-match.units, cost=match.cost,
                                                     flag=flags.FLAG_MERGING)
                                    for match in matches)
                    units = merged_units[0].units
                    date = matches[0].cost.date  ## FIXME: Select which one, oldest or latest.
                    cost_units = merged_cost[0].units
                    cost = Cost(cost_units.number/units.number, cost_units.currency, date, None)

                    # Insert a posting to refill those with a replacement match.
                    postings.append(posting._replace(units=units, cost=cost,
                                                     flag=flags.FLAG_MERGING))

                    # Now, match the reducing request against this lot.
                    postings.append(posting._replace(units=posting.units, cost=cost))
                    insufficient = abs(posting.units.number) > abs(units.number)

    if insufficient:
        errors.append(
            ReductionError(entry.meta,
                           'Not enough lots to reduce "{}": {}'.format(
                               position.to_string(posting),
                               ', '.join(position.to_string(match_posting)
                                         for match_posting in matches)),
                           entry))

    return postings, errors


def compute_cost_number(costspec, units):

    """Given a CostSpec, return the cost number, if possible to compute.

    Args:
      costspec: A parsed instance of CostSpec.
      units: An instance of Amount for the units of the position.
    Returns:
      If it is not possible to calculate the cost, return None.
      Otherwise, returns a Decimal instance, the per-unit cost.
    """
    number_per = costspec.number_per
    number_total = costspec.number_total
    if MISSING in (number_per, number_total):
        return None
    if number_total is not None:
        # Compute the per-unit cost if there is some total cost
        # component involved.
        cost_total = number_total
        units_number = units.number
        if number_per is not None:
            cost_total += number_per * units_number
        unit_cost = cost_total / abs(units_number)
    elif number_per is None:
        return None
    else:
        unit_cost = number_per
    return unit_cost


def convert_costspec_to_cost(posting):
    """Convert an instance of CostSpec to Cost, if present on the posting.

    If the posting has no cost, it itself is just returned.

    Args:
      posting: An instance of Posting.
    Returns:
      An instance of Posting with a possibly replaced 'cost' attribute.
    """
    cost = posting.cost
    if isinstance(cost, position.CostSpec):
        if cost is not None:
            units_number = posting.units.number
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
            posting = posting._replace(units=posting.units, cost=new_cost)
    return posting


# FIXME: Refactor compute_cost_number() and convert_costspec_to_cost().


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

        # Identify incomplete parts of the Posting components.
        if units.number is MISSING:
            incomplete.append((MissingType.UNITS, index))

        if isinstance(cost, CostSpec):
            if cost and cost.number_per is MISSING:
                incomplete.append((MissingType.COST_PER, index))
            if cost and cost.number_total is MISSING:
                incomplete.append((MissingType.COST_TOTAL, index))
        else:
            # Check that a resolved instance of Cost never needs interpolation.
            #
            # Note that in theory we could support the interpolation of regular
            # per-unit costs in these if we wanted to; but because they're all
            # reducing postings that have been booked earlier, those never need
            # to be interpolated.
            if cost is not None:
                assert isinstance(cost.number, Decimal), (
                    "Internal error: cost has no number: {}".format(cost))

        if price and price.number is MISSING:
            incomplete.append((MissingType.PRICE, index))

    # The replacement posting for the incomplete posting of this group.
    new_posting = None

    if len(incomplete) == 0:
        # If there are no missing numbers, just convert the CostSpec to Cost and
        # return that.
        postings = [convert_costspec_to_cost(posting)
                    for posting in postings]

    elif len(incomplete) > 1:
        # If there is more than a single value to be interpolated, generate an
        # error and return no postings.
        _, posting_index = incomplete[0]
        errors.append(InterpolationError(
            postings[posting_index].meta,
            "Too many missing numbers for currency group '{}'".format(currency),
            None))
        postings = []

    else:
        # If there is a single missing number, calculate it and fill it in here.
        missing, index = incomplete[0]
        incomplete_posting = postings[index]

        # Convert augmenting postings' costs from CostSpec to corresponding Cost
        # instances, except for the incomplete posting.
        new_postings = [(posting
                         if posting is incomplete_posting
                         else convert_costspec_to_cost(posting))
                        for posting in postings]

        # Compute the balance of the other postings.
        residual = interpolate.compute_residual(posting
                                                for posting in new_postings
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

            if weight != ZERO:
                new_pos = Position(Amount(units_number, units.currency), cost)
                new_posting = incomplete_posting._replace(units=new_pos.units,
                                                          cost=new_pos.cost)
            else:
                new_posting = None

        elif missing == MissingType.COST_PER:
            units = incomplete_posting.units
            cost = incomplete_posting.cost
            assert cost.currency == weight_currency, (
                "Internal error; residual currency different than missing currency.")
            if units.number != ZERO:
                number_per = (weight - (cost.number_total or ZERO)) / units.number
                new_cost = cost._replace(number_per=number_per)
                new_pos = Position(units, new_cost)
                new_posting = incomplete_posting._replace(units=new_pos.units,
                                                          cost=new_pos.cost)
            else:
                new_posting = None

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
                new_price_number = abs(weight / units.number)
                new_posting = incomplete_posting._replace(price=Amount(new_price_number,
                                                                       price.currency))

        else:
            assert False, "Internal error; Invalid missing type."

        # Replace the number in the posting.
        if new_posting is not None:
            # Convert augmenting posting costs from CostSpec to a corresponding
            # Cost instance.
            new_postings[index] = convert_costspec_to_cost(new_posting)
        else:
            del new_postings[index]
        postings = new_postings

    assert all(not isinstance(posting.cost, CostSpec)
               for posting in postings)

    return postings, errors, (new_posting is not None)
