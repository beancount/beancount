"""Compute unrealized gains.
"""
from collections import defaultdict

from beancount.core import account
from beancount.core.amount import Decimal, ONE, Amount
from beancount.core.data import Transaction, Posting, Price, FileLocation
from beancount.core.position import Lot, Position
from beancount.core import inventory
from beancount.core import flags
from beancount.ops import positions
from beancount.ops import prices


def unrealized_gains(entries, subaccount_name, account_types):
    """A function that inserts entries that represent unrealized gains, at the end
    of the available history. Returns a new list of entries, with the new gains
    inserted."""

    if not entries:
        return entries

    new_entries = []
    latest_date = entries[-1].date

    # Get the latest prices from the entries.
    price_map = prices.build_price_map(entries)

    # Work through the list of priced positions.
    priced_positions, _ = positions.get_priced_positions(entries, price_map)
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
