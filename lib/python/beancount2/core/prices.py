"""This module has code that can build a database of historical prices at
various times, from which unrealized capital gains and market value can be
deduced.

Prices are deduced from Price entries found in the file, or perhaps
created by scripts (for example you could build a script that will fetch
live prices online and create entries on-the-fly).
"""
from collections import defaultdict

from beancount2.core import data
from beancount2.core.data import Transaction, Posting, Price, Amount, Decimal, FileLocation, Lot
from beancount2.core.inventory import Inventory, Position
from beancount2 import utils
from beancount2.core import summarize
from beancount2.core import realization



class PriceDatabase(object):

    def __init__(self, entries):
        self.price_map = build_price_database(entries)

    def get_latest_price(self, base, quote):
        dates, rates = self.price_map[(base, quote)]
        return (dates[-1], rates[-1])


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

    # Realize the accounts into a tree (because we want the positions by-qaccount).
    real_accounts = realization.realize(entries, do_check=False)

    # For each account, look at the list of positions and build a list.
    positions = []
    for real_account in real_accounts.values():
        for position in real_account.balance.get_positions():
            if position.lot.cost or position.lot.lot_date:
                posdict = {'account': real_account.account,
                           'number': position.number,
                           'currency': position.lot.currency,
                           'cost_number': position.lot.cost.number,
                           'cost_currency': position.lot.cost.currency}
                cost = position.get_cost()
                assert cost.number == posdict['number'] * posdict['cost_number']
            else:
                posdict = {'account': real_account.account,
                           'number': position.number,
                           'currency': position.lot.currency,
                           'cost_number': None,
                           'cost_currency': None}
            positions.append(posdict)

    return positions


def unrealized_gains(entries):
    """A function that inserts entries that represent unrealized gains, at the end
    of the available history. Returns a new list of entries, with the new gains
    inserted."""

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
            grouped_positions[key].append(position)

    # For each group, synthesize entries for unrealized gains.
    new_entries = []
    for (account, currency, cost_currency), position_list in grouped_positions.items():

        # Get the latest price.
        date, price_number = prices_db.get_latest_price(currency, cost_currency)

        # Compute the total number of units and book value of the position.
        total_units = Decimal()
        book_value = Decimal()
        for position in position_list:
            number = position['number']
            total_units += number
            book_value += number * position['cost_number']

            # print('M', number * price_number)
            # print('C', number * position['cost_number'])
            # print(' ', number * (price_number - position['cost_number']))

        market_value = total_units * price_number
        pnl = market_value - book_value

        # Create a new transaction to account for this difference in gain.
        fileloc = FileLocation('<unrealized_gains>', 0)
        narration = "Unrealized gains for {} in {}".format(currency, cost_currency)
        entry = Transaction(fileloc, date, data.FLAG_UNREALIZED, None, narration, None, None, [])

        # FIXME: This obviously needs review.
        pnl_subaccount = 'PnL'
        asset_account = data.account_from_name(':'.join([account.name, pnl_subaccount]))
        income_account = data.account_from_name(':'.join([account.name.replace('Assets', 'Income'), pnl_subaccount]))

        entry.postings.append(
            Posting(entry, asset_account,
                    Position(Lot(cost_currency, None, None), pnl),
                    Amount(price_number, cost_currency),
                    None))

        entry.postings.append(
            Posting(entry, income_account,
                    Position(Lot(cost_currency, None, None), -pnl),
                    Amount(price_number, cost_currency),
                    None))

        new_entries.append(entry)

    return entries + new_entries


    # if 0:
    #     import pandas
    #     positions_dataframe = pandas.DataFrame(positions, columns=['number', 'currency',
    #                                                                'cost_number', 'cost_currency', 'account'])
    #     #positions_dataframe = positions_dataframe.sort(['currency', 'cost_currency'])
    #     positions_dataframe = positions_dataframe.sort(['account'])
    #     print(positions_dataframe.to_string())
    #     # print(positions_dataframe.groupby('currency').sum().to_string())
