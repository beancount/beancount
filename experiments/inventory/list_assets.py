#!/usr/bin/env python3
"""Aggregate and list assets and produce a report of them.

This script is designed to produce a summary of a user's assets and liabilities
and open accounts, in order to include in a contract, loan application, or will.
It should eventually replace the beancount.projects.will script with a more
structure and appropriate output.

The main premise here is that we have two pieces of data:

1. The user's assets and liabilities listed in a Beancount ledger file, and

2. A database of institution and account information to pull details from for
    each of the active user accounts and to include in the report.

"""
__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import copy
import logging

from google.protobuf import text_format

from beancount import loader
from beancount.parser import options
from beancount.core.number import D
from beancount.core import account_types
from beancount.core import realization
from beancount.core import prices
from beancount.core import convert
from beancount.core import amount
from beancount.core import account
from beancount.core import getters
from beancount.utils import encryption

from experiments.list_assets import assets_pb2


def prune_closed_accounts(real_root, ocmap):
    """Prune closed accounts and their parents.

    Args:
      real_root: An instance of RealAccount.
      ocmap: An open-close mapping.
    Returns:
      A pruned version of the RealAccount tree (the original one is left alone,
      unmutated.
    """
    real_result = copy.copy(real_root)
    real_result.clear()

    for name, real_acc in real_root.items():
        real_acc = prune_closed_accounts(real_acc, ocmap)
        if real_acc is None:
            continue
        real_result[name] = real_acc

    is_empty = len(real_root) == 0 and real_root.balance.is_empty()
    if real_root.account in ocmap:
        _, close = ocmap[real_root.account]
        if close is not None and is_empty:
            return None
    else:
        if is_empty:
            return None
    return real_result


def read_assets(filename, currency, reduce_accounts, quantization):
    """Read a Beancount file and produce a list of assets.

    Args:
      filename: A string, the path to the Beancount file to read.
      currency: A string, the currency to convert all the contents to.
      reduce_accounts: A set of account names to be aggregated.
      quantization: A Decimal instance, to quantize all the resulting amounts.
    Returns:
      A list of (account-name, number-balance), numbers being assumed to be in
      the requested currency.
    """

    # Read the Beancount input file.
    entries, _, options_map = loader.load_file(filename,
                                               log_errors=logging.error)
    acctypes = options.get_account_types(options_map)
    price_map = prices.build_price_map(entries)
    ocmap = getters.get_account_open_close(entries)

    # Compute aggregations.
    real_root = realization.realize(entries, compute_balance=True)

    # Reduce accounts which have been found in details (mutate the tree in-place).
    for account in reduce_accounts:
        real_acc = realization.get(real_root, account)
        real_acc.balance = realization.compute_balance(real_acc)
        real_acc.clear()

    # Prune all the closed accounts and their parents.
    real_root = prune_closed_accounts(real_root, ocmap)

    # Produce a list of accounts and their balances reduced to a single currency.
    acceptable_types = (acctypes.assets, acctypes.liabilities)
    accounts = []
    for real_acc in realization.iter_children(real_root):
        atype = account_types.get_account_type(real_acc.account)
        if atype not in acceptable_types:
            continue

        try:
            _, close = ocmap[real_acc.account]
            if close is not None:
                continue
        except KeyError:
            #logging.info("Account not there: {}".format(real_acc.account))
            if real_acc.account not in reduce_accounts and real_acc.balance.is_empty():
                continue

        value_inv = real_acc.balance.reduce(lambda x: convert.get_value(x, price_map))
        currency_inv = value_inv.reduce(convert.convert_position, currency, price_map)
        amount = currency_inv.get_currency_units(currency)
        accounts.append((real_acc.account, amount.number.quantize(quantization)))

    # Reduce this list of (account-name, balance-number) sorted by reverse amount order.
    accounts.sort(key=lambda x: x[1], reverse=True)
    return accounts


def read_details(filename):
    """Read the encrypted details file and create an index of its contents.

    Args:
      filename: A string, the name of the filename to read.
    Returns:
      A pair of the Details object containing the database and a mapping of
      Beancount account name to its (institution, account) pair.
    """
    # Read the encrypted details file.
    if encryption.is_encrypted_file(filename):
        contents = encryption.read_encrypted_file(filename)
    else:
        with open(args.details_filename, 'rb') as infile:
            contents = infile.read()

    # Read the ASCII protobuf database.
    details = assets_pb2.Details()
    text_format.Merge(contents, details)

    # Create a mapping of account name to (institution, account) pairs.
    mapping = {}
    for institution in details.institution:
        for acc in institution.account:
            assert acc.beancount not in mapping, (
                "Account name {} is not unique!".format(acc.beancount))
            mapping[acc.beancount] = (institution, acc)

    return details, mapping


def find_parent(mapping, account_name):
    """Find the deepest parent account present in a given dict or set.

    Note that this begins and includes the given account name itself.

    Args:
      mapping: A dict or set instance.
      account_name: A string, the name of an account.
    Returns:
      The first parent account name found.
    Raises:
      KeyError: If none of the parents of 'account_name' can be found in
      'mapping.'
    """
    while account_name:
        if account_name in mapping:
            return account_name
        else:
            account_name = account.parent(account_name)
    raise KeyError


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('beancount_filename', help='Beancount input filename')
    parser.add_argument('details_filename', help='File-based database of details')

    parser.add_argument('-c', '--currency', action='store', default='USD',
                        help="The single currency to convert to for sorting")

    parser.add_argument('-q', '--digits', action='store', default='0.01',
                        help="An example price with the desired number of output digits")

    args = parser.parse_args()

    # Read the encrypted details file.
    details, mapping = read_details(args.details_filename)

    # Read the list of assets.
    quantization = D(args.digits)
    assets = read_assets(args.beancount_filename, args.currency, set(mapping), quantization)

    # For each of the assets, find and copy their corresponding details and fill
    # in the balance amounts.
    report = assets_pb2.Details()
    institutions = {}
    for account_name, balance in assets:
        try:
            account_name = find_parent(mapping, account_name)
            institution, account = mapping[account_name]
        except KeyError:
            logging.warn("Details for account %s (%s) not found", account_name, balance)
            continue

        try:
            new_institution = institutions[id(institution)]
        except KeyError:
            new_institution = institutions[id(institution)] = report.institution.add()
            new_institution.CopyFrom(institution)
            new_institution.ClearField('account')
            new_institution.ClearField('beancount')

        new_account = new_institution.account.add()
        new_account.CopyFrom(account)
        new_account.ClearField('beancount')
        new_account.balance = balance
        new_account.currency = args.currency

    print(report)


if __name__ == '__main__':
    main()
