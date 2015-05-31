#!/usr/bin/env python3
"""Split expenses of a Beancount ledger between multiple people.

This plugin is given a list of names. It assumes that any Expenses account whose
components do not include any of the given names are to be split between the
members. It goes through all the transactions and converts all such postings
into multiple postings, one for each member.

For example, given the names 'Martin' and 'Caroline', the following transaction:

  2015-02-01 * "Aqua Viva Tulum - two nights"
     Income:Caroline:CreditCard      -269.00 USD
     Expenses:Accommodation

Will be converted to this:

  2015-02-01 * "Aqua Viva Tulum - two nights"
    Income:Caroline:CreditCard       -269.00 USD
    Expenses:Accommodation:Martin     134.50 USD
    Expenses:Accommodation:Caroline   134.50 USD

After these transformations, all account names should include the name of a
member. You can generate reports for a particular person by filtering postings
to accounts with a component by their name.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import collections
import copy
import logging
import re
import functools

from beancount.core.amount import D
from beancount.core.amount import ZERO
from beancount import loader
from beancount.core import data
from beancount.core import amount
from beancount.core import account
from beancount.core import position
from beancount.core import inventory
from beancount.core import realization
from beancount.core import getters
from beancount.core import account_types
from beancount.core import display_context
from beancount.core import flags
from beancount.ops import prices
from beancount.parser import printer
from beancount.parser import options


__plugins__ = ('split_expenses',)


def split_expenses(entries, options_map, config):
    # Validate and sanitize configuration.
    if isinstance(config, str):
        members = config.split()
    elif isinstance(config, (tuple, list)):
        members = config
    else:
        raise RuntimeError("Invalid plugin configuration: configuration for split_expenses "
                           "should be a string or a sequence.")

    acctypes = options.get_account_types(options_map)
    def is_expense_account(account):
        return account_types.get_account_type(account) == acctypes.expenses

    # A predicate to quickly identify if an account contains the name of a
    # member.
    is_individual_account = re.compile('|'.join(map(re.escape, members))).search

    # Existing and previously unseen accounts.
    new_accounts = set()

    # Filter the entries and transform transactions.
    new_entries = []
    for entry in entries:
        if isinstance(entry, data.Transaction):
            new_postings = []
            for posting in entry.postings:
                if (is_expense_account(posting.account) and
                    not is_individual_account(posting.account)):

                    # Split this posting into multiple postings.
                    split_position = copy.copy(posting.position)
                    split_position.number /= len(members)

                    for member in members:
                        # Mark the account as new if never seen before.
                        subaccount = account.join(posting.account, member)
                        new_accounts.add(subaccount)

                        # Add a new posting for each member, to a new account
                        # with the name of this member.
                        new_postings.append(
                            posting._replace(account=subaccount,
                                             position=split_position))
                else:
                    new_postings.append(posting)

            # Modify the entry in-place, replace its postings.
            entry = data.entry_replace(entry, postings=new_postings)

        new_entries.append(entry)

    # Create Open directives for new subaccounts if necessary.
    oc_map = getters.get_account_open_close(entries)
    open_date = entries[0].date
    meta = data.new_metadata('<split_expenses>', 0)
    open_entries = []
    for new_account in new_accounts:
        if new_account not in oc_map:
            entry = data.Open(meta, open_date, new_account, None, None)
            open_entries.append(entry)

    return open_entries + new_entries, []


def reduce_currency(balances, currency, price_map):
    """Return the given balances to a single currency at the market price.

    Args:
      balances: An Inventory of balances.
      currency: A currency string.
      price_map: A price mapping as created by build_price_map().
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

    parser.add_argument('members', nargs='+',
                        help=('A list of person names, '
                              'which should match components in account names'))

    parser.add_argument('-r', '--reconciliation', action='store',
                        dest='reconciliation_account',
                        default='Assets:Reconciliation',
                        help="Reconciliation account")

    args = parser.parse_args()

    # Parse the input file and derive various objects from it.
    entries, errors, options_map = loader.load_file(args.members)
    account_order = functools.partial(account_types.get_account_sort_key,
                                      options.get_account_types(options_map))
    price_map = prices.build_price_map(entries)

    divisor = 1/D(len(args.members))

    # Create accumulators for each relevant bucket.
    balances = collections.defaultdict(list)

    dcontext = display_context.DisplayContext()
    real_root = realization.realize(entries)
    for real_account in realization.iter_children(real_root, False):
        # Only process leaf-accounts.
        if real_account.balance.is_empty():
            continue

        # Figure out whose account that is for.
        for member in args.members:
            if re.search(member, real_account.account):
                found = member
                break
        else:
            found = None

        # If this matches a specific person, book the entire amount to this
        # person.
        if found:
            balances[found].append((real_account.account, real_account.balance))

            for pos in real_account.balance:
                dcontext.update(pos.number, pos.lot.currency)

        # Otherwise, split the amount among all the persons.
        else:
            split_balance = real_account.balance * divisor

            for pos in split_balance:
                dcontext.update(pos.number, pos.lot.currency)

            for member in args.members:
                balances[member].append((real_account.account, split_balance))

    # Figure out which currency we're going to resolve this to.
    main_currency = options_map['operating_currency'][0]

    # Print out the list of balances for each.
    reconciliation_date = entries[-1].date
    output_entries = []
    for member in args.members:
        account_reconciliation = account.join(args.reconciliation_account, member)

        # Create a reconciliation transaction that moves pending amounts to
        # their correct expense accounts.
        reconciliation = data.Transaction({},
                                          reconciliation_date,
                                          flags.FLAG_OKAY,
                                          member,
                                          "Final reconciliation for {}".format(member),
                                          None, None, [])
        output_entries.append(reconciliation)

        balance_list = balances[member]
        total = inventory.Inventory()
        for account_, balance in sorted(balance_list, key=lambda k: account_order(k[0])):
            total += balance
            for pos in balance:
                reconciliation.postings.append(
                    data.Posting(reconciliation, account_, pos, None, None, None))
        total = -total

        for pos in total:
            reconciliation.postings.append(
                data.Posting(reconciliation, account_reconciliation, pos,
                             None, None, None))

        # Create a conversion entry that reconciles into the main operating
        # currency.
        conversion = data.Transaction({},
                                      reconciliation_date,
                                      flags.FLAG_OKAY,
                                      member,
                                      "Final conversion",
                                      None, None, [])
        output_entries.append(conversion)

        balance_amount = ZERO
        for pos in total:
            if pos.lot.currency == main_currency:
                continue

            price_date, price_rate = prices.get_price(price_map,
                                                      (pos.lot.currency, main_currency))
            dcontext.update(price_rate.quantize(D('0.00001')), main_currency)
            delta_currency = pos.number * price_rate
            conversion.postings.append(
                data.Posting(conversion, account_reconciliation, -pos,
                             amount.Amount(price_rate, main_currency), None, None))
            balance_amount += delta_currency

        pos_conversion = position.Position(position.Lot(main_currency, None, None),
                                           -balance_amount)
        conversion.postings.append(
            data.Posting(conversion, account_reconciliation, -pos_conversion,
                         None, None, None))

    print('plugin "beancount.ops.auto_accounts"')
    printer.print_entries(output_entries, dcontext)


if __name__ == '__main__':
    main()



# FIXME: Add a function to convert to a single currency.
# FIXME: Add an export to "Transaction" format, in order to insert the final balance.
