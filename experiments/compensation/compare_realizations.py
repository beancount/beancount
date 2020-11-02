#!/usr/bin/env python3
"""Compare the different methods for computing realizations.

These have to be reconciled eventually.
"""
__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import collections
import datetime
import logging
import logging
import pprint
import time

from dateutil import rrule
from dateutil.parser import parse
import matplotlib; matplotlib.use("Qt5Agg")
from matplotlib import pyplot
import numpy

from beancount import loader
from beancount.core import data
from beancount.core import inventory
from beancount.core import realization
from beancount.ops import holdings
from beancount.ops import summarize


EXTRAPOLATE_WORTHS = 1000000, 1500000, 2000000, 2500000, 3000000, 4000000, 5000000, 6000000


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount input filename')
    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)

    root_account = realization.realize(entries)
    balances, _ = summarize.balance_by_account(entries, compress_unbooked=True)

    realization_account_names = {
        real_account.account
        for real_account in realization.iter_children(root_account)
        if not real_account.balance.is_empty()}

    summarize_account_names = {
        account
        for account, balance in balances.items()
        if not balance.is_empty()}

    if (realization_account_names - summarize_account_names):
        pprint.pprint(realization_account_names - summarize_account_names)
    if (summarize_account_names - realization_account_names):
        pprint.pprint(summarize_account_names - realization_account_names)

    for real_account in sorted(list(realization.iter_children(root_account)),
                               key=lambda ra: ra.account):
        summarize_balance = balances.get(real_account.account, inventory.Inventory())

        if summarize_balance == real_account.balance:
            continue

        print(real_account.account)
        print("    realization")
        for pos in real_account.balance:
            print("      {}".format(pos))
        print("    summarization")
        for pos in summarize_balance:
            print("      {}".format(pos))
        print()




if __name__ == '__main__':
    main()
