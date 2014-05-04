"""This module has code that can build a database of historical prices at
various times, from which unrealized capital gains and market value can be
deduced.

Prices are deduced from Price entries found in the file, or perhaps
created by scripts (for example you could build a script that will fetch
live prices online and create entries on-the-fly).
"""
from collections import defaultdict

from beancount.core import account
from beancount.core.amount import Decimal, Amount
from beancount.core.data import Transaction, Posting, Price, FileLocation
from beancount.core.position import Lot, Position
from beancount.core.inventory import Inventory
from beancount import utils
from beancount.core import realization
from beancount.core import flags

try:
    import pandas
    import numpy
except (ImportError, ValueError):
    pandas = None


class PriceDatabase(object):
    """An in-memory price database.

    This object defines a container of historical of prices for various
    currencies, denominated in a specific cost currency.
    """
    def __init__(self, price_list):
        """Constructor from tuples.

        Args:
          price_list: A list of (date, number, currency, cost-currency)
            tuples.
        Returns:
          A new instance of PriceDatabase. The returned object is meant to be
          read-only, not modified further.
        """
        ##self.price_map = build_price_database(entries)





    def __getitem__(self, base_quote):
        return self.price_map.__getitem__(key)

    # FIXME: Not sure if I need this in the end.
    def __iter__(self):
        return iter(self.price_map)


    def get_latest_price(self, base, quote):
        if base == quote:
            return (None, Decimal('1'))

        dates, rates = self.price_map[(base, quote)]
        return (dates[-1], rates[-1])

    def get_prices(self, base, quote):
        return self.price_map[(base, quote)]


def build_price_database(entries):

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

        inverted_list = [(date, Decimal('1')/rate)
                         for (date, rate) in remove_list]
        insert_list.extend(inverted_list)

    # Unzip and sort each of the entries and eliminate duplicates on the date.
    sorted_price_map = {}
    for base_quote, values in price_map.items():
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


def get_price_entries(entries):
    price_entries = []
    total_balance = Inventory()
    for entry in entries:

        if isinstance(entry, Price):
            price_entries.append(entry)

        elif isinstance(entry, Transaction):
            for posting in entry.postings:
                reducing = total_balance.add_position(posting.position, True)
                if posting.price:
                    # Add prices when they're explicit.
                    entry = Price(entry.fileloc, entry.date,
                                  posting.position.lot.currency,
                                  posting.price)
                    price_entries.append(entry)

                elif posting.position.lot.cost is not None and not reducing:
                    # Other add prices when we're not booking/reducing.
                    entry = Price(entry.fileloc, entry.date,
                                  posting.position.lot.currency,
                                  posting.position.lot.cost)
                    price_entries.append(entry)

    return price_entries


def get_latest_prices(entries):
    """Return a dictionary of the latest prices from the list of entries."""

    prices = {}
    for entry in utils.filter_type(entries, Price):
        key = (entry.currency, entry.amount.currency)
        prices[key] = entry
    return prices


def get_latest_positions(entries):
    """Get a dictionary of the latest positions by account."""

    # Realize the accounts into a tree (because we want the positions by-qaccount).
    real_accounts = realization.realize(entries)

    # For each account, look at the list of positions and build a list.
    positions = []
    for real_account in real_accounts:
        for position in real_account.balance.get_positions():
            if position.lot.cost or position.lot.lot_date:
                posdict = {'account': real_account.fullname,
                           'number': position.number,
                           'currency': position.lot.currency,
                           'cost_number': position.lot.cost.number,
                           'cost_currency': position.lot.cost.currency}
                cost = position.get_cost()
                assert cost.number == posdict['number'] * posdict['cost_number']
            else:
                posdict = {'account': real_account.fullname,
                           'number': position.number,
                           'currency': position.lot.currency,
                           'cost_number': None,
                           'cost_currency': None}
            positions.append(posdict)

    return positions


def get_priced_positions(entries):
    """Get a list of positions, groups by (account, currency, cost_currency),
    with the latest prices fetched and dated.
    Returns:
      A dict of (account, currency, cost_currency) -> position dict.
    """

    # Get the latest prices from the entries.
    prices_db = PriceDatabase(entries)

    # Get the full list of positions.
    positions = get_latest_positions(entries)

    # Group by account and currencies, and filter those which have an associated
    # cost.
    grouped_positions = defaultdict(list)
    for position in positions:
        if position['cost_number'] is not None:
            key = (position['account'],
                   position['currency'],
                   position['cost_currency'])
        else:
            key = (position['account'],
                   position['currency'],
                   None)
        grouped_positions[key].append(position)

    # For each group, add the price to the dataframe.
    for (account, currency, cost_currency), position_list in grouped_positions.items():
        if not cost_currency:
            continue

        # Get the latest price.
        price_date, price_number = prices_db.get_latest_price(currency, cost_currency)

        for position in position_list:
            position['price_number'] = price_number
            position['price_date'] = price_date

    # Flatten the grouped positions.
    flat_positions = [position
                      for position_list in grouped_positions.values()
                      for position in position_list]

    return grouped_positions, flat_positions


def unrealized_gains(entries, subaccount_name, account_types):
    """A function that inserts entries that represent unrealized gains, at the end
    of the available history. Returns a new list of entries, with the new gains
    inserted."""

    if not entries:
        return entries

    new_entries = []
    latest_date = entries[-1].date

    # Work through the list of priced positions.
    priced_positions, _ = get_priced_positions(entries)
    for (account_name, currency, cost_currency), position_list in priced_positions.items():

        if not cost_currency:
            continue

        # Compute the total number of units and book value of the position.
        total_units = Decimal()
        market_value = Decimal()
        book_value = Decimal()
        for position in position_list:
            number = position['number']
            total_units  += number
            market_value += number * position['price_number']
            book_value   += number * position['cost_number']

        pnl = market_value - book_value

        # Note: the price_number and price_date should be the same for all these
        # positions; use the latest one in the list.
        price_number = position['price_number']
        price_date = position['price_date']

        # Create a new transaction to account for this difference in gain.
        fileloc = FileLocation('<unrealized_gains>', 0)
        narration = "Unrealized gains for {} in {} (price: {}, as of {})".format(
            currency, cost_currency, price_number, price_date)
        entry = Transaction(fileloc, latest_date, flags.FLAG_UNREALIZED, None, narration, None, None, [])

        # Add the gain/loss as a subaccount to the asset account.
        if subaccount_name:
            asset_account = account.join(account_name, subaccount_name)
        else:
            asset_account = account_name

        # Note: don't set a price because we don't want these to end up in Conversions.
        #price = Amount(price_number, cost_currency)
        entry.postings.append(
            Posting(entry, asset_account,
                    Position(Lot(cost_currency, None, None), pnl),
                    None,
                    None))

        # Book this as income, converting the account name to be the same, but as income.
        # Note: this is a rather convenient but arbitraty choice--maybe it would be best to let
        # the user decide to what account to book it, but I don't a nice way to let the user
        # specify this.
        income_account_name = account_name.replace(account_types.assets,
                                                   account_types.income)
        if subaccount_name:
            income_account = account.join(income_account_name, subaccount_name)
        else:
            income_account = income_account_name

        entry.postings.append(
            Posting(entry, income_account,
                    Position(Lot(cost_currency, None, None), -pnl),
                    Amount(price_number, cost_currency),
                    None))

        new_entries.append(entry)

    return entries + new_entries


def get_positions_as_dataframe(entries):
    """Return a dataframe with a detailed list of positions."""

    if pandas is None:
        return None

    _, flat_positions = get_priced_positions(entries)


    # TODO(blais): Convert this to avoid the Pandas dependency.

    dataframe = pandas.DataFrame.from_records(
        flat_positions, columns=['account', 'number', 'currency', 'cost_number', 'price_number', 'cost_currency', 'price_date'])

    dataframe['number'] = dataframe['number'].astype(numpy.float)
    dataframe['cost_number'] = dataframe['cost_number'].astype(numpy.float)
    dataframe['price_number'] = dataframe['price_number'].astype(numpy.float)

    dataframe['book_value'] = dataframe['number'] * dataframe['cost_number']
    dataframe['market_value'] = dataframe['number'] * dataframe['price_number']
    dataframe['pnl'] = dataframe['market_value'] - dataframe['book_value']

    return dataframe
