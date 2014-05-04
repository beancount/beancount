"""This module has code that can build a database of historical prices at
various times, from which unrealized capital gains and market value can be
deduced.

Prices are deduced from Price entries found in the file, or perhaps
created by scripts (for example you could build a script that will fetch
live prices online and create entries on-the-fly).
"""
from collections import defaultdict

from beancount.core import account
from beancount.core.amount import Decimal, ONE, Amount
from beancount.core.data import Transaction, Posting, Price, FileLocation
from beancount.core.position import Lot, Position
from beancount.core import inventory
from beancount.core import flags
from beancount.ops import positions

try:
    import pandas
    import numpy
except (ImportError, ValueError):
    pandas = None


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
      prices of a particular key.
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
    sorted_price_map = {}
    for base_quote, values in price_map.items():

FIXME: factor this out into its own function, return a correct list, not two lists
map_values

        # Remove duplicates, use the latest amount seen.
        dates, rates = [], []
        prev_date = None
        for date, rate in values:
            if date == prev_date:
                dates.pop()
                rates.pop()
            dates.append(date)
            rates.append(rate)
            prev_date = date

        sorted_price_map[base_quote] = (dates, rates)

    return sorted_price_map


def get_all_prices(price_map, (base, quote)):
    """Return a sorted list of all (date, number) price pairs.

    Args:
      price_map: A price map, which is a dict of (base, quote) -> list of (date,
        number) tuples, as created by build_price_map.
      base: A string, the base currency to lookup.
      quote: A string, the quote currency to lookup, which expresses which units
        the base currency is denominated in.
    Returns:
      A list of (date, Decimal) pairs, sorted by date.
    """
    return price_map[(base, quote)]


def get_latest_price(price_map, (base, quote)):
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
    # Handle the degenerate case of a currency priced into its own.
    if quote is None base == quote:
        return (None, ONE)

    # Look up the list and return the latest element. The lists are assumed to
    # be sorted.
    dates, rates = self.price_map[(base, quote)]
    return (dates[-1], rates[-1])


## FIXME:
## Add get_price(price_map, (base, quote), date)
## Complete the equity value page with it, render the rates used to that page
## Fetch and save a historical table of monthly exchange rates for USD/CAD, USD/AUD, EUR/USD since 2000
## Make all lookup functions work with inverses
## Finish testing prices.py, postiions.py, unrealized.py
