#!/usr/bin/env python3
"""Split expenses of a Beancount ledger between multiple people.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import collections
import logging
import re
import functools

from beancount.core.amount import D
from beancount.core.amount import ZERO
from beancount import loader
from beancount.core import data
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import realization
from beancount.core import account_types
from beancount.core import display_context
from beancount.core import flags
from beancount.ops import prices
from beancount.parser import printer
from beancount.parser import options


def reduce_currency(balances, currency, price_map):
    """Return the given balances to a single currency.

    Args:
      balances: An Inventory of balances.
      currency: A currency string.
    Returns:
      An instance of Amount.
    """
    total = ZERO
    for pos in balances:
        if pos.lot.cost:
            logging.error("Lot has an unexpected cost; skipping.")
            continue
        if pos.lot.currency == currency:
            delta = pos.number
        else:
            price_date, price_rate = prices.get_price(price_map,
                                                      (pos.lot.currency, currency))
            delta = pos.number * price_rate
        total += delta
    return total


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename',
                        help='A Beancount input filename')

    parser.add_argument('person_patterns', nargs='+',
                        help='A list of pattenrs for non-shared accounts, one per person')

    parser.add_argument('-r', '--reconciliation', action='store',
                        dest='reconciliation_account',
                        default='Assets:Reconciliation',
                        help="Reconciliation account")

    args = parser.parse_args()

    # Compile all the user-provided regular expression patterns upfront to
    # detect errors before beginning.
    num_persons = len(args.person_patterns)
    divisor = 1/D(num_persons)
    try:
        patterns_map = {pattern: re.compile(pattern)
                        for pattern in args.person_patterns}
    except re.error as exc:
        parser.error("Invalid pattern: '{}'".format(exc))

    # Parse the input file and derive various objects from it.
    entries, errors, options_map = loader.load_file(args.filename)
    account_order = functools.partial(account_types.get_account_sort_key,
                                      options.get_account_types(options_map))
    ##dcontext = options_map['display_context']
    price_map = prices.build_price_map(entries)

    # Create accumulators for each relevant buckets.
    balances = collections.defaultdict(list)

    dcontext = display_context.DisplayContext()
    real_root = realization.realize(entries)
    for real_account in realization.iter_children(real_root, False):
        # Only process leaf-accounts.
        if real_account.balance.is_empty():
            continue

        # Figure out whose account that is for.
        for pattern, regexp in patterns_map.items():
            if regexp.search(real_account.account):
                found = pattern
                break
        else:
            found = None

        # If this matches a specific person, book the entire amount to this
        # person.
        ##print(real_account.account, real_account.balance, found or 'Split')
        if found:
            balances[found].append((real_account.account, real_account.balance))

            for pos in real_account.balance:
                dcontext.update(pos.number, pos.lot.currency)

        # Otherwise, split the amount among all the persons.
        else:
            split_balance = real_account.balance * divisor

            for pos in split_balance:
                dcontext.update(pos.number, pos.lot.currency)

            for pattern in patterns_map.keys():
                balances[pattern].append((real_account.account, split_balance))

    # Figure out which currency we're going to resolve this to.
    main_currency = options_map['operating_currency'][0]

    # Print out the list of balances for each.
    reconciliation_date = entries[-1].date
    output_entries = []
    for pattern in args.person_patterns:
        # Create a reconciliation transaction that moves pending amounts to
        # their correct expense accounts.
        reconciliation = data.Transaction({},
                                          reconciliation_date,
                                          flags.FLAG_OKAY,
                                          pattern,
                                          "Final reconciliation for {}".format(pattern),
                                          None, None, [])
        output_entries.append(reconciliation)

        balance_list = balances[pattern]
        total = inventory.Inventory()
        for account, balance in sorted(balance_list, key=lambda k: account_order(k[0])):
            total += balance
            for pos in balance:
                reconciliation.postings.append(
                    data.Posting(reconciliation, account, pos, None, None, None))
        total = -total

        for pos in total:
            reconciliation.postings.append(
                data.Posting(reconciliation, args.reconciliation_account, pos,
                             None, None, None))

        # Create a conversion entry that reconciles into the main operating
        # currency.
        conversion = data.Transaction({},
                                      reconciliation_date,
                                      flags.FLAG_OKAY,
                                      pattern,
                                      "Final conversion",
                                      None, None, [])
        output_entries.append(conversion)

        balance_amount = ZERO
        for pos in total:
            if pos.lot.currency == main_currency:
                continue

            price_date, price_rate = prices.get_price(price_map,
                                                      (main_currency, pos.lot.currency))
            dcontext.update(price_rate, pos.lot.currency)
            delta_currency = pos.number / price_rate
            conversion.postings.append(
                data.Posting(conversion, args.reconciliation_account, -pos,
                             None, None, None))
            balance_amount += delta_currency

        pos_conversion = position.Position(position.Lot(main_currency, None, None),
                                           -balance_amount)
        conversion.postings.append(
            data.Posting(conversion, args.reconciliation_account, -pos_conversion,
                         amount.Amount(price_rate, pos.lot.currency), None, None))

    print('plugin "beancount.ops.auto_accounts"')
    printer.print_entries(output_entries, dcontext)


if __name__ == '__main__':
    main()
