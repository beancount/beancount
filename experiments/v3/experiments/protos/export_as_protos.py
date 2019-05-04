#!/usr/bin/env python3
"""Export all the processed directives as proto records.

This is an experiment to generate a realistic dataset for a forked query client
as a new, separate project. The dream is that this is all that Beancount would
become: a parser, booking engine and data source provider for a query/api thing.
"""
__copyright__ = "Copyright (C) 2019  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import logging
import datetime

from beancount import loader
from beancount.parser import printer
from beancount.core.number import Decimal
from beancount.core import data
from beancount.core import amount
from beancount.core import position

from experiments.protos import beancount_pb2 as pb

import riegeli


def copy_decimal(din: Decimal, dout: pb.Decimal):
    dout.strvalue = str(din)

def copy_date(date: datetime.date, pbdate: pb.Date):
    pbdate.year = date.year
    pbdate.month = date.month
    pbdate.day = date.day


def copy_meta(meta: dict, pbmeta: pb.Meta):
    if meta is None:
        return
    for key, value in sorted(meta.items()):
        item = pbmeta.kv.add()
        item.key = key
        item.value = str(value) # FIXME: TODO - convert to type


def copy_amount(amt: amount.Amount, pbamt: pb.Amount):
    copy_decimal(amt.number, pbamt.number)
    pbamt.currency = amt.currency


def copy_cost(cost: position.Cost, pbcost: pb.Cost):
    copy_decimal(cost.number, pbcost.number)
    pbcost.currency = cost.currency
    copy_date(cost.date, pbcost.date)
    if cost.label:
        pbcost.label = cost.label


def copy_posting(posting: data.Posting, pbpost: pb.Posting):
    copy_meta(posting.meta, pbpost.meta)
    if posting.flag:
        pbpost.flag = posting.flag.encode('utf8')
    pbpost.account = posting.account
    if posting.units is not None:
        copy_amount(posting.units, pbpost.units)
    if posting.cost is not None:
        copy_cost(posting.cost, pbpost.cost)
    if posting.price is not None:
        copy_amount(posting.price, pbpost.price)


def convert_Transaction(entry: data.Transaction) -> pb.Directive:
    pbent = pb.Directive()
    txn = pbent.txn
    copy_meta(entry.meta, pbent.meta)
    copy_date(entry.date, pbent.date)
    if entry.flag:
        txn.flag = entry.flag.encode('utf8')
    if entry.payee:
        txn.payee = entry.payee
    txn.narration = entry.narration
    if entry.tags:
        txn.tags.extend(entry.tags)
    if entry.links:
        txn.links.extend(entry.links)
    for posting in entry.postings:
        pbpost = txn.postings.add()
        copy_posting(posting, pbpost)
    return pbent


def convert_Open(entry: data.Open) -> pb.Directive:
    pbent = pb.Directive()
    open = pbent.open
    copy_meta(entry.meta, pbent.meta)
    copy_date(entry.date, pbent.date)
    open.account = entry.account
    if entry.currencies:
        open.currencies.extend(entry.currencies)
    # TODO(blais): Add enum
    return pbent


def convert_Close(entry: data.Close) -> pb.Directive:
    pbent = pb.Directive()
    close = pbent.close
    copy_meta(entry.meta, pbent.meta)
    copy_date(entry.date, pbent.date)
    close.account = entry.account
    return pbent


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Ledger filename')
    args = parser.parse_args()

    with open('/tmp/ledger.rieg', 'wb') as outfile:
        with riegeli.RecordWriter(outfile) as writer:
            entries, errors, options_map = loader.load_file(args.filename)
            for entry in entries:
                if isinstance(entry, data.Transaction):
                    pbent = convert_Transaction(entry)
                elif isinstance(entry, data.Open):
                    pbent = convert_Open(entry)
                elif isinstance(entry, data.Close):
                    pbent = convert_Close(entry)
                else:
                    pbent = None

                if pbent is not None:
                    #print(type(txn))
                    #print(txn)
                    writer.write_message(pbent)

                if 0:
                    print('-' * 100)
                    printer.print_entry(entry)
                    print(txn)
                    print()


if __name__ == '__main__':
    main()
