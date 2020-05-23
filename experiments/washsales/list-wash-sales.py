#!/usr/bin/env python3
"""List all the lots with wash sales.

With this script I'm able to make the numbers reported by MS on the 1099 match
mine, except for rounding error.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import collections
import datetime
import itertools
import logging
import os
import re
import sys
from os import path

from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core import data
from beancount.core import inventory
from beancount.parser import printer
from beancount.reports import table
from beancount.utils import misc_utils
from beancount.utils import date_utils
from beancount import loader


LotSale = collections.namedtuple(
    'LotSale', ('no ref date_buy date_sell days_held term inst units cost price '
                'totcost totprice comm proc pnl wash adj'))

fieldspec = [
    ('no', 'No'),
    ('ref', 'Reference'),
    ('date_buy', 'Acquisition Date'),
    ('date_sell', 'Sale Date'),
    ('days_held', 'Days Held'),
    ('term', 'Tax Term'),
    ('inst', 'Instrument'),
    ('units', 'Shares'),
    ('cost', 'Share Cost'),
    ('price', 'Selling Price'),
    ('totcost', 'Cost Basis'),
    ('totprice', 'Market Value'),
    ('comm', 'Commission'),
    ('proc', 'Proceeds'),
    ('pnl', 'Gain'),
    ('wash', 'Washed?'),
    ('adj', 'Adjustment'),
]


def aggregate_sales(lots):
    return [aggregate_lot_sales(glots)
            for _, glots in misc_utils.groupby(
                    lambda lot: (lot.ref, lot.term), lots).items()]

def aggregate_lot_sales(sublots):
    """Aggregate a list of LotSale instances, matching the 1099's."""
    if len(sublots) == 1:
        agglot = sublots[0]
    else:
        agglot = sublots[0]
        for lot in sublots[1:]:

            if (isinstance(agglot.date_buy, datetime.date) and
                agglot.date_buy == lot.date_buy):
                date_buy = lot.date_buy
            else:
                date_buy = 'VARIOUS'

            if (isinstance(agglot.date_sell, datetime.date) and
                agglot.date_sell == lot.date_sell):
                date_sell = lot.date_sell
            else:
                date_sell = 'VARIOUS'

            if agglot.days_held == lot.days_held:
                days_held = lot.days_held
            else:
                days_held = 'VARIOUS'

            if agglot.term == lot.term:
                term = lot.term
            else:
                term = 'VARIOUS'

            agglot = agglot._replace(
                date_buy=date_buy,
                date_sell=date_sell,
                days_held=days_held,
                term=term,
                units=agglot.units + lot.units,
                totcost=agglot.totcost + lot.totcost,
                totprice=agglot.totprice + lot.totprice,
                comm=agglot.comm + lot.comm,
                proc=agglot.proc + lot.proc,
                pnl=agglot.pnl + lot.pnl,
                wash='W' if agglot.wash or lot.wash else '',
                adj=(agglot.adj or ZERO) + (lot.adj or ZERO))
    return agglot._replace(adj=agglot.adj or '')


def expand_sales_legs(entries, account, start, end, calculate_commission):
    # Expand each of the sales legs.
    balances = collections.defaultdict(inventory.Inventory)
    sales = []
    for txn in data.filter_txns(entries):
        # If we got to the end of the period, bail out.
        if txn.date >= end:
            break

        # Accumulate the balances before the start date.
        if txn.date < start:
            for posting in txn.postings:
                if re.match(account, posting.account):
                    balance = balances[posting.account]
                    balance.add_position(posting)
            continue

        # Fallthrough: we're not in the period. Process the matching postings.

        # Find reducing postings (i.e., for each lot).
        txn_sales = []
        for posting in txn.postings:
            if re.match(account, posting.account):
                balance = balances[posting.account]
                reduced_position, booking = balance.add_position(posting)
                # Set the cost on the posting from the reduced position.
                # FIXME: Eventually that'll happen automatically during the full
                # booking stage.
                if booking == inventory.Booking.REDUCED:
                    posting = posting._replace(cost=reduced_position.cost)

                # If the postings don't have a reference number, ignore them.
                if 'ref' not in txn.meta:
                    continue

                if (posting.cost and
                    posting.units.number < ZERO):
                    if not posting.price:
                        logging.error("Missing price on %s", posting)
                    txn_sales.append(data.TxnPosting(txn, posting))

        if txn_sales and calculate_commission:
            # Find total commission.
            for posting in txn.postings:
                if re.search('Commission', posting.account):
                    commission = posting.units.number
                    break
            else:
                commission = ZERO

            # Compute total number of units.
            tot_units = sum(sale.posting.units.number
                            for sale, _ in txn_sales)

            # Assign a proportion of the commission to each of the sales by
            # inserting it into its posting metadata. This will be processed below.
            for sale, _ in txn_sales:
                fraction = sale.posting.units.number / tot_units
                sale.posting.meta['commission'] = fraction * commission

        sales.extend(txn_sales)
    return sales


def create_detailed_table(sales, calculate_commission):
    """Convert into a table of data, full detail of very single log."""
    Q = D('0.01')
    lots = []
    total_loss = collections.defaultdict(D)
    total_gain = collections.defaultdict(D)
    total_adj = collections.defaultdict(D)

    # If no mssb number has been assigned explicitly, assign a random one. I
    # need to figure out how to find those numbers again.
    auto_mssb_number = itertools.count(start=1000000 + 1)

    for sale in sales:
        try:
            sale_no = sale.txn.meta['mssb']
        except KeyError:
            sale_no = next(auto_mssb_number)
        ref = sale.txn.meta['ref']

        units = sale.posting.units
        totcost = (-units.number * sale.posting.cost.number).quantize(Q)
        totprice = (-units.number * sale.posting.price.number).quantize(Q)

        commission_meta = sale.posting.meta.get('commission', None)
        if commission_meta is None:
            commission = ZERO
        else:
            if calculate_commission:
                commission = commission_meta
            else:
                # Fetch the commission that was inserted by the commissions plugin.
                commission = commission_meta.get_only_position().units.number
        commission = commission.quantize(Q)

        pnl = (totprice - totcost - commission).quantize(Q)
        is_wash = sale.posting.meta.get('wash', False)

        # Ensure the key in all the dicts.
        (total_gain[units.currency], total_loss[units.currency], total_adj[units.currency])
        if totprice > totcost:
            total_gain[units.currency] += pnl
        else:
            total_loss[units.currency] += pnl
        if is_wash:
            total_adj[units.currency] += pnl
            code = 'W'
            adj = -pnl
        else:
            code = ''
            adj = ''

        days_held = (sale.txn.date - sale.posting.cost.date).days
        term = 'LONG' if days_held >= 365 else 'SHORT'
        lot = LotSale(sale_no,
                      ref,
                      sale.posting.cost.date,
                      sale.txn.date,
                      days_held,
                      term,
                      units.currency,
                      -units.number.quantize(Q),
                      sale.posting.cost.number.quantize(Q),
                      sale.posting.price.number.quantize(Q),
                      totcost,
                      totprice,
                      commission,
                      totprice - commission,
                      pnl,
                      code,
                      adj)
        lots.append(lot)
    Totals = collections.namedtuple('Totals', 'loss gain adj')
    totals = Totals(total_loss, total_gain, total_adj)
    return lots, table.create_table(lots, fieldspec), totals


def create_summary_table(totals):
    summary_fields = list(enumerate(['Currency', 'Gain', 'Loss', 'Net', 'Adj/Wash']))
    summary = []
    gain = ZERO
    loss = ZERO
    adj = ZERO
    for currency in sorted(totals.adj.keys()):
        gain += totals.gain[currency]
        loss += totals.loss[currency]
        adj += totals.adj[currency]
        summary.append((currency,
                        totals.gain[currency],
                        totals.loss[currency],
                        totals.gain[currency] + totals.loss[currency],
                        totals.adj[currency]))
    summary.append(('*', gain, loss, gain + loss, adj))
    return table.create_table(summary, summary_fields)


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('report', choices=['detail', 'aggregate', 'summary'],
                        help='Type of report')
    parser.add_argument('filename',
                        help='Beancount input file')
    parser.add_argument('account',
                        help='Account name')

    parser.add_argument('--start', type=date_utils.parse_date_liberally,
                        help="Start date")
    parser.add_argument('--end', type=date_utils.parse_date_liberally,
                        help="End date; if not set, at the end of star'ts year")

    parser.add_argument('-o', '--output', action='store',
                        help="Output directory of all the reports, in txt and csv formats")

    args = parser.parse_args()

    calculate_commission = False

    # Setup date interval.
    if args.start is None:
        args.start = datetime.date(datetime.date.today().year, 1, 1)
    if args.end is None:
        args.end = datetime.date(args.start.year + 1, 1, 1)

    entries, errors, options_map = loader.load_file(args.filename)

    # Create the list of sales.
    sales = expand_sales_legs(entries, args.account, args.start, args.end,
                              calculate_commission)

    # Produce a detailed table.
    lots, tab_detail, totals = create_detailed_table(sales, calculate_commission)

    # Aggregate by transaction in order to be able to cross-check against the
    # 1099 forms.
    agglots = aggregate_sales(lots)
    tab_agg = table.create_table(sorted(agglots, key=lambda lot: (lot.ref, lot.no)),
                                 fieldspec)

    # Create a summary table of P/L.
    tab_summary = create_summary_table(totals)

    # Render all the reports to an output directory.
    if args.output:
        os.makedirs(args.output, exist_ok=True)
        for name, tab in [('detail', tab_detail),
                          ('aggregate', tab_agg),
                          ('summary', tab_summary)]:
            for fmt in 'txt', 'csv':
                with open(path.join(args.output,
                                    '{}.{}'.format(name, fmt)), 'w') as outfile:
                    table.render_table(tab, outfile, fmt)

    # Rendering individual reports to the console.
    if args.report == 'detail':
        print('Detail of all lots')
        print('=' * 48)
        table.render_table(tab_detail, sys.stdout, 'txt')
        print()
    elif args.report == 'aggregate':
        print('Aggregated by trade & Reference Number (to Match 1099/Form8459)')
        print('=' * 48)
        table.render_table(tab_agg, sys.stdout, 'txt')
        print()
    elif args.report == 'summary':
        print('Summary')
        print('=' * 48)
        table.render_table(tab_summary, sys.stdout, 'txt')
        print()


if __name__ == '__main__':
    main()
