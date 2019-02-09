#!/usr/bin/env python3
"""Reconcile Mompoo and Dadpoo's balances.
"""

import argparse
import logging

from beancount import loader
from beancount.core import inventory
from beancount.core import data
from beancount.core.number import D
from beancount.core.number import ZERO


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename', help='Child ledger filename')

    parser.add_argument('-r', '--ratio', action='store', type=D, default=D('0.6'),
                        help="Dadpoo to Mompoo ratio.")

    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)
    balances = {
        'Income:Dadpoo': inventory.Inventory(),
        'Income:Mompoo': inventory.Inventory()}
    for entry in data.filter_txns(entries):
        for posting in entry.postings:
            balance = balances.get(posting.account, None)
            if balance is not None:
                balance.add_position(posting)
    dad_actual = -balances['Income:Dadpoo'].get_only_position().units.number
    mom_actual = -balances['Income:Mompoo'].get_only_position().units.number
    #assert dad_actual.currency == mom_actual.currency == 'USD'
    total_amount = dad_actual + mom_actual
    dad_expected = total_amount * args.ratio
    mom_expected = total_amount - dad_expected
    debtor, creditor = 'Dadpoo', 'Mompoo'
    diff = abs(dad_expected - dad_actual).quantize(D('0.01'))
    print("Total contributions:           {:20.2f}".format(total_amount))
    print()
    print("Dadpoo expected contribution:  {:20.2f}".format(dad_expected))
    print("Dadpoo actual contribution:    {:20.2f}".format(dad_actual))
    print()
    print("Mompoo expected contribution:  {:20.2f}".format(mom_expected))
    print("Mompoo actual contribution:    {:20.2f}".format(mom_actual))
    print()
    if (dad_expected - dad_actual) < ZERO:
        debtor, creditor = creditor, debtor
    print("{} OWES {}: {:10.2f}".format(debtor, creditor, diff))


if __name__ == '__main__':
    main()
