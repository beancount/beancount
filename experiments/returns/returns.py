#!/usr/bin/env python3
"""Test out portfolio returns calculation.

http://furius.ca/beancount/doc/portfolio-returns
"""
import argparse
import copy
import re

from beancount.core import amount
from beancount import loader
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import account_types
from beancount.core import inventory
from beancount.core import complete
from beancount.ops import prices


def find_external_flows(entries, related_regexp):
    """Find the dates of external flow transactions.

    Args:
      entries: A list of directives.
      related_regexp: A regular expression string that defines the set of
        related accounts.
    Returns:
      A tuple of a list of transactions with external flows and a list of
      datetime.date instnaces.
    """
    external_entries = []
    dates = set()
    match = re.compile(related_regexp).match
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        if (any(match(posting.account) for posting in entry.postings) and
            any(not match(posting.account) for posting in entry.postings)):

            external_entries.append(entry)
            dates.add(entry.date)

    return external_entries, sorted(dates)


def compute_balance_for_accounts(entries, match_account, boundary_entry):
    """Compute the balance for a subste of accounts before and after an entry.

    Args:
      entries: A list of directives.
      match_account: A predicate on an account name that will determine
        whether we include this account's cost in our balance.
      boundary_entry: A directive which is present in the list of entries. If
        this is set to None, we compute the balance over all entries.
    Returns:
      Two Inventory instances: one computed before applying the boundary entry
      and one computed after.
    """

    balance_before = inventory.Inventory()
    for entry in entries:
        if entry is boundary_entry:
            break
        if not isinstance(entry, data.Transaction):
            continue
        for posting in entry.postings:
            if match_account(posting.account):
                balance_before.add_position(posting.position)

    balance_after = copy.copy(balance_before)
    if boundary_entry is not None:
        for posting in boundary_entry.postings:
            if match_account(posting.account):
                balance_after.add_position(posting.position)

    return balance_before, balance_after


def get_inventory_market_value(balance, date, price_map):
    """Compute the market value of the inventory in a currency at a date.

    This function converts all the positions to their market value, in their
    respective cost currencies.

    Args:
      balance: An instance of Inventory.
      date: A datetime.date instance, the date at which to market the instruments.
        If the date provided is None, the inventory is valued at the latest market
        prices.
      price_map: A price map, as created by beancount.ops.prices.
    Returns:
      An inventory of market values per currency.
    """
    new_balance = inventory.Inventory()
    for position in balance.get_positions():
        lot = position.lot
        cost_currency = lot.cost.currency if lot.cost else None
        if cost_currency:
            base_quote = (lot.currency, cost_currency)
            price_date, price_number = prices.get_price(price_map, base_quote, date)
            new_amount = amount.Amount(position.number * price_number, cost_currency)
        else:
            new_amount = amount.Amount(position.number, lot.currency)
        new_balance.add(new_amount)
    return new_balance


def inventory_value(balance):
    """Convert an inventory in a single number.

    Args:
      balance: An instnace of Inventory, assumed to contain either zero or
        one positions.
    Returns:
      A Decimal number.
    """
    num_positions = len(balance)
    if num_positions == 0:
        market_value = 0
    elif num_positions == 1:
        position = balance.get_positions()[0]
        market_value = position.number
    else:
        raise ValueError(
            "Market value could not be reduced to a single currency: {}".format(
                market_inventory_before))
    return market_value


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument('filename', help='Ledger filename')

    parser.add_argument('-r', '--related', action='store',
                        default='.*:ScotiaBank:.*',
                        help="Related accounts.")

    opts = parser.parse_args()

    entries, errors, options_map = loader.load(opts.filename)

    # Find and print the lis tof external entries.
    external_entries, dates = find_external_flows(entries, opts.related)

    # Create a predicate for the Assets accounts.
    acc_types = options.get_account_types(options_map)
    match = re.compile(opts.related).match
    is_asset_account = lambda account_: (
        match(account_) and
        account_types.get_account_type(account_) == acc_types.assets)

    # Verify that external flow entries only affect balance sheet accounts and
    # not income or expenses accounts.
    for entry in external_entries:
        for posting in entry.postings:
            if (match(posting.account) and
                not account_types.get_account_type(posting.account) == acc_types.assets):

                raise ValueError(
                    "External flow may not affect non-asset accounts: {}".format(entry))

    # Build a price database.
    price_map = prices.build_price_map(entries)

    # A list of (date-begin, date-end, return) for each period without external flows.
    periods = []

    # Process the periods without external flows.
    date_previous = None
    balance_previous = None
    value_previous = amount.ZERO
    for entry in external_entries + [None]:
        date = entry.date if entry else None

        # Compute the inventories before and after the external flow.
        balance_before, balance_after = compute_balance_for_accounts(entries,
                                                                     is_asset_account,
                                                                     entry)
        # Compute the market value before the external flow.
        inventory_before = get_inventory_market_value(balance_before, date, price_map)
        value_before = inventory_value(inventory_before)

        # Compute the market value after the external flow.
        inventory_after = get_inventory_market_value(balance_after, date, price_map)
        value_after = inventory_value(inventory_after)

        print(',--------------------------------')
        print(' {} -> {}'.format(date_previous, date))
        print(' previous : {} : {}'.format(value_previous, balance_previous))
        print(' before   : {} : {}'.format(value_before, balance_before))
        print(' after    : {} : {}'.format(value_after, balance_after))
        if value_previous == amount.ZERO:
            if value_before == amount.ZERO:
                period_return = 1.0
            else:
                raise ValueError("Portfolio with zero value grew to non-zero value.")
        else:
            period_return = float(value_before)/float(value_previous)

        print(' return   : {:.5f}'.format(period_return))
        print('`--------------------------------')

        if entry:
            printer.print_entry(entry)

        periods.append(period_return)

        date_previous = date
        balance_previous = balance_after
        value_previous = value_after


    total_return = 1.0
    for period_return in periods:
        total_return *= period_return
    print('Total Return: {:.5f}'.format(total_return))


if __name__ == '__main__':
    main()
