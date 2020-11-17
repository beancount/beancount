"""This module has code that can build a database of historical prices at
various times, from which unrealized capital gains and market value can be
deduced.

Prices are deduced from Price entries found in the file, or perhaps
created by scripts (for example you could build a script that will fetch
live prices online and create entries on-the-fly).
"""
__copyright__ = "Copyright (C) 2013-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections
from typing import Optional, Set

from beancount.core.number import ONE
from beancount.core.number import ZERO
from beancount.core.data import Price
from beancount.core.data import Currency
from beancount.core import data
from beancount.utils import misc_utils
from beancount.utils import bisect_key


def get_last_price_entries(entries, date):
    """Run through the entries until the given date and return the last
    Price entry encountered for each (currency, cost-currency) pair.

    Args:
      entries: A list of directives.
      date: An instance of datetime.date. If None, the very latest price
        is returned.
    Returns:
      A list of price entries.
    """
    price_entry_map = {}
    for entry in entries:
        if date is not None and entry.date >= date:
            break
        if isinstance(entry, Price):
            base_quote = (entry.currency, entry.amount.currency)
            price_entry_map[base_quote] = entry
    return sorted(price_entry_map.values(), key=data.entry_sortkey)


class PriceMap(dict):
    """A price map dictionary.

    The keys include both the set of forward (base, quote) pairs and their
    inverse. In order to determine which are the forward pairs, access the
    'forward_pairs' attribute

    Attributes:
      forward_pairs: A list of (base, quote) keys for the forward pairs.
    """
    __slots__ = ('forward_pairs',)


def build_price_map(entries):
    """Build a price map from a list of arbitrary entries.

    If multiple prices are found for the same (currency, cost-currency) pair at
    the same date, the latest date is kept and the earlier ones (for that day)
    are discarded.

    If inverse price pairs are found, e.g. USD in AUD and AUD in USD, the
    inverse that has the smallest number of price points is converted into the
    one that has the most price points. In that way they are reconciled into a
    single one.

    Args:
      entries: A list of directives, hopefully including some Price and/or
      Transaction entries.
    Returns:
      A dict of (currency, cost-currency) keys to sorted lists of (date, number)
      pairs, where 'date' is the date the price occurs at and 'number' a Decimal
      that represents the price, or rate, between these two
      currencies/commodities. Each date occurs only once in the sorted list of
      prices of a particular key. All of the inverses are automatically
      generated in the price map.
    """
    # Fetch a list of all the price entries seen in the ledger.
    price_entries = [entry
                     for entry in entries
                     if isinstance(entry, Price)]

    # Build a map of exchange rates between these units.
    # (base-currency, quote-currency) -> List of (date, rate).
    price_map = collections.defaultdict(list)
    for price in price_entries:
        base_quote = (price.currency, price.amount.currency)
        price_map[base_quote].append((price.date, price.amount.number))

    # Find pairs of inversed units.
    inversed_units = []
    for base_quote, values in price_map.items():
        base, quote = base_quote
        if (quote, base) in price_map:
            inversed_units.append(base_quote)

    # Find pairs of inversed units, and swallow the conversion with the smaller
    # number of rates into the other one.
    for base, quote in inversed_units:
        bq_prices = price_map[(base, quote)]
        qb_prices = price_map[(quote, base)]
        remove = ((base, quote)
                  if len(bq_prices) < len(qb_prices)
                  else (quote, base))
        base, quote = remove

        remove_list = price_map[remove]
        insert_list = price_map[(quote, base)]
        del price_map[remove]

        inverted_list = [(date, ONE/rate)
                         for (date, rate) in remove_list
                         if rate != ZERO]
        insert_list.extend(inverted_list)

    # Unzip and sort each of the entries and eliminate duplicates on the date.
    sorted_price_map = PriceMap({
        base_quote: list(misc_utils.sorted_uniquify(date_rates, lambda x: x[0], last=True))
        for (base_quote, date_rates) in price_map.items()})

    # Compute and insert all the inverted rates.
    forward_pairs = list(sorted_price_map.keys())
    for (base, quote), price_list in list(sorted_price_map.items()):
        # Note: You have to filter out zero prices for zero-cost postings, like
        # gifted options.
        sorted_price_map[(quote, base)] = [
            (date, ONE/price) for date, price in price_list
            if price != ZERO]

    sorted_price_map.forward_pairs = forward_pairs
    return sorted_price_map


def project(orig_price_map: PriceMap,
            from_currency: Currency,
            to_currency: Currency,
            base_currencies: Optional[Set[Currency]] = None) -> PriceMap:
    """Project all prices with a quote currency to another quote currency.

    Say you have a price for HOOL in USD and you'd like to convert HOOL to CAD.
    If there aren't any (HOOL, CAD) price pairs in the database it will remain
    unconverted. Projecting from USD to CAD will compute combined rates and
    insert corresponding prices over all base currencies (like HOOL). In this
    example, each of the (HOOL, USD) prices would see an inserted (HOOL, CAD)
    price inserted at the same date.

    It is common to make these projections when reducing inventories in a ledger
    that states multiple operating currency pairs, when for example, one wants
    to compute total value of a set of accounts in one of those currencies.

    Please note that:

    - Even if the target pair has existing entries, projection will still be
      applied. For example, is there exist some (HOOL, CAD) prices, the
      projection in the example above will still insert some new price points to
      it.

    - However, projected prices colliding existing ones at the same date will
      not override them.

    - Projection will fail to insert a new price if the conversion between to
      and from currencies has no existing prices (e.g. before its first price
      entry).

    - Perhaps most importantly, we only insert price points at dates where the
      base commodity has a price point. In other words, if we have prices for
      dates A and C and the rate changes between these dates at date B, we don't
      synthesize a new price at date B. A more accurate method to get projected
      prices that takes into account varying rates is to do multiple lookups.
      We'll eventually add a method to query it via a specified list of
      intermediate pairs. {c1bd24f8d4b7}

    Args:
      orig_price_map: An existing price map.
      from_currency: The quote currency with existing project points (e.g., USD).
      to_currency: The quote currency to insert price points for (e.g., CAD).
      base_currencies: An optional set of commodities to restrict the
        projections to (e.g., {HOOL}).
    Returns:
      A new price map, with the extra projected prices. The original price map
      is kept intact.
    """
    # If nothing is requested, return the original map.
    if from_currency == to_currency:
        return orig_price_map

    # Avoid mutating the input map.
    price_map = {key: list(value) for key, value in orig_price_map.items()}

    # Process the entire database (it's not indexed by quote currency).
    currency_pair = (from_currency, to_currency)
    for base_quote, prices in list(price_map.items()):
        # Filter just the currencies to convert.
        base, quote = base_quote
        if quote != from_currency:
            continue

        # Skip currencies not requested if a constraint has been provided.
        # {4bb702d82c8a}
        if base_currencies and base not in base_currencies:
            continue

        # Create a mapping of existing prices so we can avoid date collisions.
        existing_prices = ({date for date, _ in price_map[(base, to_currency)]}
                           if (base, to_currency) in price_map
                           else set())

        # Project over each of the prices.
        new_projected = []
        for date, price in prices:
            rate_date, rate = get_price(price_map, currency_pair, date)
            if rate is None:
                # There is no conversion rate at this time; skip projection.
                # {b2b23353275d}.
                continue
            if rate_date in existing_prices:
                # Skip collisions in date. {97a5703ac517}
                continue

            # Append the new rate.
            new_price = price * rate
            new_projected.append((date, new_price))

        # Make sure the resulting lists are sorted.
        if new_projected:
            projected = price_map.setdefault((base, to_currency), [])
            projected.extend(new_projected)
            projected.sort()

            inverted = price_map.setdefault((to_currency, base), [])
            inverted.extend((date, ZERO if rate == ZERO else ONE/rate)
                            for date, rate in new_projected)
            inverted.sort()

    return price_map


def normalize_base_quote(base_quote):
    """Convert a slash-separated string to a pair of strings.

    Args:
      base_quote: A pair of strings, the base currency to lookup, and the quote
        currency to lookup, which expresses which units the base currency is
        denominated in. This may also just be a string, with a '/' separator.
    Returns:
      A pair of strings.
    """
    if isinstance(base_quote, str):
        base_quote_norm = tuple(base_quote.split('/'))
        assert len(base_quote_norm) == 2, base_quote
        base_quote = base_quote_norm
    assert isinstance(base_quote, tuple), base_quote
    return base_quote


def _lookup_price_and_inverse(price_map, base_quote):
    """Lookup the (base, quote) tuple in the price map and its inverse.
    If not found, raise an appropriate exception.

    Note: this is meant to be an INTERNAL helper function, use the get_*
    functions to obtain values from a price_map object.

    Args:
      price_map: A price map, which is a dict of (base, quote) -> list of (date,
        number) tuples, as created by build_price_map.
      base_quote: A pair of strings, (base, quote) currencies.
        No normalization is done.
    Returns:
      A list of price-dates, if successful.
    Raises:
      KeyError: If the base_quote and its inverse both weren't able to be looked
        up.
    """
    try:
        return price_map[base_quote]
    except KeyError as exc:
        base, quote = base_quote
        prices = price_map.get((quote, base), None)
        if prices:
            return prices
        else:
            raise


def get_all_prices(price_map, base_quote):
    """Return a sorted list of all (date, number) price pairs.

    Args:
      price_map: A price map, which is a dict of (base, quote) -> list of (date,
        number) tuples, as created by build_price_map.
      base_quote: A pair of strings, the base currency to lookup, and the quote
        currency to lookup, which expresses which units the base currency is
        denominated in. This may also just be a string, with a '/' separator.
    Returns:
      A list of (date, Decimal) pairs, sorted by date.
    Raises:
      KeyError: If the base/quote could not be found.
    """
    base_quote = normalize_base_quote(base_quote)
    return _lookup_price_and_inverse(price_map, base_quote)


def get_latest_price(price_map, base_quote):
    """Return the latest price/rate from a price map for the given base/quote pair.
    This is often used to just get the 'current' price if you're looking at the
    entire set of entries.

    Args:
      price_map: A price map, which is a dict of (base, quote) -> list of (date,
        number) tuples, as created by build_price_map.
    Returns:
      A pair of (date, number), where 'date' is a datetime.date instance and
      'number' is a Decimal of the price, or rate, at that date. The date is the
      latest date which we have an available price for in the price map.
    """
    base_quote = normalize_base_quote(base_quote)

    # Handle the degenerate case of a currency priced into its own.
    base, quote = base_quote
    if quote is None or base == quote:
        return (None, ONE)

    # Look up the list and return the latest element. The lists are assumed to
    # be sorted.
    try:
        price_list = _lookup_price_and_inverse(price_map, base_quote)
    except KeyError:
        price_list = None
    if price_list:
        return price_list[-1]
    else:
        return None, None


def get_price(price_map, base_quote, date=None):
    """Return the price as of the given date.

    If the date is unspecified, return the latest price.

    Args:
      price_map: A price map, which is a dict of (base, quote) -> list of (date,
        number) tuples, as created by build_price_map.
      base_quote: A pair of strings, the base currency to lookup, and the quote
        currency to lookup, which expresses which units the base currency is
        denominated in. This may also just be a string, with a '/' separator.
      date: A datetime.date instance, the date at which we want the conversion
        rate.
    Returns:
      A pair of (datetime.date, Decimal) instance. If no price information could
      be found, return (None, None).
    """
    if date is None:
        return get_latest_price(price_map, base_quote)

    base_quote = normalize_base_quote(base_quote)

    # Handle the degenerate case of a currency priced into its own.
    base, quote = base_quote
    if quote is None or base == quote:
        return (None, ONE)

    try:
        price_list = _lookup_price_and_inverse(price_map, base_quote)
        index = bisect_key.bisect_right_with_key(price_list, date, key=lambda x: x[0])
        if index == 0:
            return None, None
        else:
            return price_list[index-1]
    except KeyError:
        return None, None
