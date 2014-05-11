"""This module has code that can build a database of historical prices at
various times, from which unrealized capital gains and market value can be
deduced.

Prices are deduced from Price entries found in the file, or perhaps
created by scripts (for example you could build a script that will fetch
live prices online and create entries on-the-fly).
"""
from collections import defaultdict

from beancount.core.amount import ONE
from beancount.core.data import Transaction, Price
from beancount.core import inventory
from beancount.utils import misc_utils


def get_price_entries(entries):
    """Extract explicit price entries and implicitly defined prices from a list of
    entries. Explicit price entries are simply included the the output. Prices
    from postings with costs or with prices from Transaction entries are
    synthesized as Price entries.

    Args:
      entries: A list of directives. We're interested only in the Price and
        Transaction instances.
    Returns:
      A list of Price instances.
    """
    price_entries = []
    total_balance = inventory.Inventory()
    for entry in entries:

        # Include price entries.
        if isinstance(entry, Price):
            price_entries.append(entry)

        elif isinstance(entry, Transaction):
            # Inspect all the postings in the transaction.
            for posting in entry.postings:
                # Check if the position is matching against an existing
                # position.
                reducing = total_balance.add_position(posting.position, True)

                # Add prices when they're explicitly specified on a posting. An
                # explicitly specified price may occur in a conversion, e.g.
                #      Asset:Account    100 USD @ 1.10 CAD
                # or, if a cost is also specified, as the current price of the
                # underlying instrument, e.g.
                #      Asset:Account    100 GOOG {564.20} @ {581.97} USD
                if posting.price:
                    entry = Price(entry.fileloc, entry.date,
                                  posting.position.lot.currency,
                                  posting.price)
                    price_entries.append(entry)

                # Add costs, when we're not matching against an existing
                # position. This happens when we're just specifying the cost,
                # e.g.
                #      Asset:Account    100 GOOG {564.20}
                elif posting.position.lot.cost is not None and not reducing:
                    # Other add prices .
                    entry = Price(entry.fileloc, entry.date,
                                  posting.position.lot.currency,
                                  posting.position.lot.cost)
                    price_entries.append(entry)

    return price_entries


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
    price_entries = get_price_entries(entries)

    # Build a map of exchange rates between these units.
    # (base-currency, quote-currency) -> List of (date, rate).
    price_map = defaultdict(list)
    for price in price_entries:
        base_quote = (price.currency, price.amount.currency)
        price_map[base_quote].append( (price.date, price.amount.number) )

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
                         for (date, rate) in remove_list]
        insert_list.extend(inverted_list)

    # Unzip and sort each of the entries and eliminate duplicates on the date.
    sorted_price_map = {
        base_quote: list(misc_utils.uniquify_last(date_rates, lambda x: x[0]))
        for (base_quote, date_rates) in price_map.items()}

    # Compute and insert all the inverted rates.
    for (base, quote), price_list in list(sorted_price_map.items()):
        sorted_price_map[(quote, base)] = [(date, ONE/price)
                                           for date, price in price_list]

    return sorted_price_map


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

    """
    base_quote = normalize_base_quote(base_quote)
    return price_map[base_quote]


def get_latest_price(price_map, base_quote):
    """Return the latest price/rate from a prica map for the given base/quote pair.
    This is often used to just get the 'current' price if you're looking at the
    entire set of entries.

    Args:
      price_map: A price map, which is a dict of (base, quote) -> list of (date,
        number) tuples, as created by build_price_map.
    Returns:
      A pair of (date, number), where 'date' is a datetime.date instancea dn
      'number' is a Decimal of the price, or rate, at that date. The date is the
      latest date which we have an available price for in the price map.
    """
    base, quote = normalize_base_quote(base_quote)

    # Handle the degenerate case of a currency priced into its own.
    if quote is None or base == quote:
        return (None, ONE)

    # Look up the list and return the latest element. The lists are assumed to
    # be sorted.
    price_list = price_map[(base, quote)]
    if price_list:
        return price_list[-1]
    else:
        return None


# Interpolation methods.
PREVIOUS, LINEAR = object(), object()

def get_price(price_map, base_quote, date=None, interpolation=PREVIOUS):
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
      interpolation: A method for interpolation of the rate, if the date sits
        between two price points. PREVIOUS returns the last available price point
        before the given date, and LINEAR performs linear interpolation between
        the two surroudning price points.
    Returns:
      A pair of (datetime.date, Decimal) instance.
    """
    if date is None:
        return get_latest_price(price_map, base_quote)

    base_quote = normalize_base_quote(base_quote)
    price_list = price_map[base_quote]
    index = misc_utils.bisect_right_with_key(price_list, date, key=lambda x: x[0])
    if index == 0:
        return None, None
    else:
        return price_list[index-1]
