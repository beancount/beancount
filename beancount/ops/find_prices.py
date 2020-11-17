"""A library of codes create price fetching jobs from strings and files.
"""
__copyright__ = "Copyright (C) 2015-2016,2020  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core import data
from beancount.ops import summarize


def find_currencies_at_cost(entries, date=None):
    """Return all currencies that were held at cost at some point.

    This returns all of them, even if not on the books at a particular point in
    time. This code does not look at account balances.

    Args:
      entries: A list of directives.
      date: A datetime.date instance.
    Returns:
      A list of (base, quote) currencies.
    """
    currencies = set()
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        if date and entry.date >= date:
            break
        for posting in entry.postings:
            if posting.cost is not None and posting.cost.number is not None:
                currencies.add((posting.units.currency, posting.cost.currency))
    return currencies


def find_currencies_converted(entries, date=None):
    """Return currencies from price conversions.

    This function looks at all price conversions that occurred until some date
    and produces a list of them. Note: This does not include Price directives,
    only postings with price conversions.

    Args:
      entries: A list of directives.
      date: A datetime.date instance.
    Returns:
      A list of (base, quote) currencies.
    """
    currencies = set()
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        if date and entry.date >= date:
            break
        for posting in entry.postings:
            price = posting.price
            if posting.cost is not None or price is None:
                continue
            currencies.add((posting.units.currency, price.currency))
    return currencies


def find_currencies_priced(entries, date=None):
    """Return currencies seen in Price directives.

    Args:
      entries: A list of directives.
      date: A datetime.date instance.
    Returns:
      A list of (base, quote) currencies.
    """
    currencies = set()
    for entry in entries:
        if not isinstance(entry, data.Price):
            continue
        if date and entry.date >= date:
            break
        currencies.add((entry.currency, entry.amount.currency))
    return currencies


def find_balance_currencies(entries, date=None):
    """Return currencies relevant for the given date.

    This computes the account balances as of the date, and returns the union of:
    a) The currencies held at cost, and
    b) Currency pairs from previous conversions, but only for currencies with
       non-zero balances.

    This is intended to produce the list of currencies whose prices are relevant
    at a particular date, based on previous history.

    Args:
      entries: A list of directives.
      date: A datetime.date instance.
    Returns:
      A set of (base, quote) currencies.
    """
    # Compute the balances.
    currencies = set()
    currencies_on_books = set()
    balances, _ = summarize.balance_by_account(entries, date)
    for _, balance in balances.items():
        for pos in balance:
            if pos.cost is not None:
                # Add currencies held at cost.
                currencies.add((pos.units.currency, pos.cost.currency))
            else:
                # Add regular currencies.
                currencies_on_books.add(pos.units.currency)

    # Create currency pairs from the currencies which are on account balances.
    # In order to figure out the quote currencies, we use the list of price
    # conversions until this date.
    converted = (find_currencies_converted(entries, date) |
                 find_currencies_priced(entries, date))
    for cbase in currencies_on_books:
        for base_quote in converted:
            base, quote = base_quote
            if base == cbase:
                currencies.add(base_quote)

    return currencies
