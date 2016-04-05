#!/usr/bin/env python3
"""List all the lots with wash sales.

With this script I'm able to make the numbers reported by MS on the 1099 match
mine, except for rounding error.

TODO:

* Move the allocation of commissions from this script to a plugin.

"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import collections
import datetime
import logging
import os
import re
import sys
from os import path

from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.core import data
from beancount.parser import printer
from beancount.reports import table
from beancount.utils import misc_utils
from beancount.utils import date_utils
from beancount import loader


LotSale = collections.namedtuple(
    'LotSale', 'no ref date inst units cost price totcost totprice comm proc pnl wash adj')


def aggregate_sales(sublots):
    """Agreggate a list of LotSale instances."""
    if len(sublots) == 1:
        agglot = sublots[0]
    else:
        agglot = sublots[0]
        for lot in sublots[1:]:
            agglot = agglot._replace(
                units=agglot.units + lot.units,
                totcost=agglot.totcost + lot.totcost,
                totprice=agglot.totprice + lot.totprice,
                comm=agglot.comm + lot.comm,
                proc=agglot.proc + lot.proc,
                pnl=agglot.pnl + lot.pnl,
                wash='W' if agglot.wash or lot.wash else '',
                adj=(agglot.adj or ZERO) + (lot.adj or ZERO))
    return agglot._replace(adj=agglot.adj or '')


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount input file')
    parser.add_argument('account', help='Account name')

    parser.add_argument('--start', type=date_utils.parse_date_liberally,
                        help="Start date")
    parser.add_argument('--end', type=date_utils.parse_date_liberally,
                        help="End date; if not set, at the end of star'ts year")

    parser.add_argument('-o', '--output', action='store',
                        help="Output directory for the CSV files")

    args = parser.parse_args()

    calculate_commission = False

    # Setup date interval.
    if args.start is None:
        args.start = datetime.date(datetime.date.today().year, 1, 1)
    if args.end is None:
        args.end = datetime.date(args.start.year + 1, 1, 1)

    entries, errors, options_map = loader.load_file(args.filename)

    # Find transactions with postings matching the account.
    txns = [txn for txn in entries
            if (isinstance(txn, data.Transaction) and
                args.start <= txn.date < args.end and
                any(re.match(args.account, posting.account)
                    for posting in txn.postings))]

    # Expand each of the sales legs.
    sales = []
    for txn in txns:
        if 'ref' not in txn.meta:
            continue

        # Find reducing postings (i.e., for each lot).
        txn_sales = []
        for posting in txn.postings:
            if (re.match(args.account, posting.account) and
                posting.cost and
                posting.units.number < ZERO):
                if not posting.price:
                    logging.error("Missing price on %s", posting)
                txn_sales.append(data.TxnPosting(txn, posting))

        if calculate_commission:
            # Find total commission.
            for posting in txn.postings:
                if re.search('Commission', posting.account):
                    commission = posting.units.number
                    break
            else:
                commission = ZERO

            # Compute total number of units.
            tot_units = sum(sale.posting.units.number
                            for sale in txn_sales)

            # Assign a proportion of the commission to each of the sales by
            # inserting it into its posting metadata. This will be processed below.
            for sale in txn_sales:
                fraction = sale.posting.units.number / tot_units
                sale.posting.meta['commission'] = fraction * commission

        sales.extend(txn_sales)

    # Convert into a table of data, full detail of very single log.
    Q = D('0.01')
    lots = []
    total_loss = collections.defaultdict(D)
    total_gain = collections.defaultdict(D)
    total_adj = collections.defaultdict(D)
    for sale in sales:
        sale_no = sale.txn.meta['mssb']
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
                commission = commission_meta[0].units.number
        commission = commission.quantize(Q)

        pnl = (totprice - totcost - commission).quantize(Q)
        is_wash = sale.posting.meta.get('wash', False)
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
        lot = LotSale(sale_no,
                      ref,
                      sale.txn.date,
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
    tab_detail = table.create_table(lots)

    # Aggregate by transaction in order to be able to cross-check against the
    # 1099 forms.
    agglots = [aggregate_sales(lots)
               for _, lots in misc_utils.groupby(
                       lambda lot: (lot.no, lot.ref), lots).items()]
    tab_agg = table.create_table(sorted(agglots, key=lambda lot: (lot.ref, lot.no)))

    # Write out a summary of P/L.
    summary_fields = list(enumerate(['Currency', 'Gain', 'Loss', 'Net', 'Adj/Wash']))
    summary = []
    gain = ZERO
    loss = ZERO
    adj = ZERO
    for currency in sorted(total_adj.keys()):
        gain += total_gain[currency]
        loss += total_loss[currency]
        adj += total_adj[currency]
        summary.append((currency,
                        total_gain[currency],
                        total_loss[currency],
                        total_gain[currency] + total_loss[currency],
                        total_adj[currency]))
    summary.append(('*', gain, loss, gain + loss, adj))
    tab_summary = table.create_table(summary, summary_fields)

    # Render to the console.
    print('Detail of all lots')
    print('=' * 48)
    table.render_table(tab_detail, sys.stdout, 'txt')
    print()

    print('Aggregated by trade & reference number')
    print('=' * 48)
    table.render_table(tab_agg, sys.stdout, 'txt')
    print()

    print('Summary')
    print('=' * 48)
    table.render_table(tab_summary, sys.stdout, 'txt')

    # Write out CSV files.
    if args.output:
        with open(path.join(args.output, 'wash-sales-detail.csv'), 'w') as file:
            table.render_table(tab_detail, file, 'csv')
        with open(path.join(args.output, 'wash-sales-aggregate.csv'), 'w') as file:
            table.render_table(tab_agg, file, 'csv')


if __name__ == '__main__':
    main()
