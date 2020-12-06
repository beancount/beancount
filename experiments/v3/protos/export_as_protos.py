#!/usr/bin/env python3
"""Export all the processed directives as proto records.

This is an experiment to generate a realistic dataset for a forked query client
as a new, separate project. The dream is that this is all that Beancount would
become: a parser, booking engine and data source provider for a query/api thing.
"""
__copyright__ = "Copyright (C) 2019  Martin Blais"
__license__ = "GNU GPLv2"

from decimal import Decimal
from os import path
from typing import Optional
import argparse
import datetime
import functools
import itertools
import logging
import os
import re
import time

import riegeli

from google.protobuf.pyext import _message

from beancount import loader
from beancount.ccore import data_pb2 as pb
from beancount.ccore import date_pb2 as db
from beancount.ccore import number_pb2 as nb
from beancount.core import amount
from beancount.core import data
from beancount.core import position
from beancount.core.number import MISSING
from beancount.cparser import extmodule
from beancount.cparser import grammar
from beancount.cparser import options_pb2 as ob
from beancount.cparser import parser_pb2 as qb
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import printer


def copy_decimal(din: Decimal, dout: nb.Number):
    dout.exact = str(din)

def copy_date(date: datetime.date, pbdate: db.Date):
    pbdate.year = date.year
    pbdate.month = date.month
    pbdate.day = date.day


def copy_meta(meta: dict, pbmeta: pb.Meta, pbloc: pb.Location):
    if meta is None:
        return
    for key, value in meta.items():
        if key == "filename":
            pbloc.filename = value
        elif key == "lineno":
            pbloc.lineno = value
        else:
            item = pbmeta.kv.add()
            item.key = key
            if isinstance(value, bool):
                item.value.boolean = value
            elif isinstance(value, datetime.date):
                item.value.date.year = value.year
                item.value.date.month = value.month
                item.value.date.day = value.day
            elif isinstance(value, Decimal):
                item.value.number.exact = str(value)
            else:
                assert isinstance(value, str), type(value)
                if re.match(amount.CURRENCY_RE + "$", value):
                    item.value.currency = str(value)
                else:
                    item.value.text = str(value)


def copy_amount(amt: amount.Amount, pbamt: pb.Amount):
    if amt is MISSING:
        return
    if amt.number is not MISSING:
        copy_decimal(amt.number, pbamt.number)
    if amt.currency is not MISSING:
        pbamt.currency = amt.currency


def copy_cost(cost: position.Cost, pbcost: pb.Cost):
    if cost.number not in {MISSING, None}:
        copy_decimal(cost.number, pbcost.number)
    if cost.currency is not None:
        pbcost.currency = cost.currency
    if cost.date is not None:
        copy_date(cost.date, pbcost.date)
    if cost.label:
        pbcost.label = cost.label


def copy_cost_spec(cost: position.CostSpec, pbcost: qb.CostSpec):
    if cost.number_per not in {MISSING, None}:
        copy_decimal(cost.number_per, pbcost.number_per)
    if cost.number_total not in {MISSING, None}:
        copy_decimal(cost.number_total, pbcost.number_total)
    if cost.currency not in {None, MISSING}:
        pbcost.currency = cost.currency
    if cost.date is not None:
        copy_date(cost.date, pbcost.date)
    if cost.label:
        pbcost.label = cost.label


def copy_posting(posting: data.Posting, pbpost: pb.Posting):
    copy_meta(posting.meta, pbpost.meta, pbpost.location)
    if posting.flag:
        pbpost.flag = posting.flag.encode('utf8')
    pbpost.account = posting.account
    if posting.units is not None:
        copy_amount(posting.units, pbpost.units)
    if posting.cost is not None:
        if isinstance(posting.cost, position.CostSpec):
            copy_cost_spec(posting.cost, pbpost.cost_spec)
        elif isinstance(posting.cost, position.Cost):
            copy_cost(posting.cost, pbpost.cost_spec)

    if posting.price is not None:
        copy_amount(posting.price, pbpost.price)


def convert_Transaction(entry: data.Transaction) -> pb.Directive:
    pbdir = pb.Directive()
    txn = pbdir.transaction
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    if entry.tags:
        pbdir.tags.extend(sorted(entry.tags))
    if entry.links:
        pbdir.links.extend(sorted(entry.links))

    if entry.flag:
        txn.flag = entry.flag.encode('utf8')
    if entry.payee:
        txn.payee = entry.payee
    if entry.narration:
        txn.narration = entry.narration
    for posting in entry.postings:
        pbpost = txn.postings.add()
        copy_posting(posting, pbpost)
    return pbdir


def convert_Open(entry: data.Open) -> pb.Directive:
    pbdir = pb.Directive()
    open = pbdir.open
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    open.account = entry.account
    if entry.currencies:
        open.currencies.extend(entry.currencies)
    if entry.booking:
        open.booking = ob.Booking.Value(entry.booking.name)
    return pbdir


def convert_Close(entry: data.Close) -> pb.Directive:
    pbdir = pb.Directive()
    close = pbdir.close
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    close.account = entry.account
    return pbdir


def convert_Commodity(entry: data.Commodity) -> pb.Directive:
    pbdir = pb.Directive()
    comm = pbdir.commodity
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    comm.currency = entry.currency
    return pbdir


def convert_Event(entry: data.Event) -> pb.Directive:
    pbdir = pb.Directive()
    event = pbdir.event
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    event.type = entry.type
    event.description = entry.description
    return pbdir


def convert_Note(entry: data.Note) -> pb.Directive:
    pbdir = pb.Directive()
    note = pbdir.note
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    note.account = entry.account
    note.comment = entry.comment
    return pbdir


def convert_Query(entry: data.Query) -> pb.Directive:
    pbdir = pb.Directive()
    query = pbdir.query
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    query.name = entry.name
    query.query_string = entry.query_string
    return pbdir


def convert_Price(entry: data.Price) -> pb.Directive:
    pbdir = pb.Directive()
    price = pbdir.price
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    price.currency = entry.currency
    copy_decimal(entry.amount.number, price.amount.number)
    price.amount.currency = entry.amount.currency
    return pbdir


def convert_Balance(entry: data.Balance) -> pb.Directive:
    pbdir = pb.Directive()
    balance = pbdir.balance
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    balance.account = entry.account
    copy_decimal(entry.amount.number, balance.amount.number)
    balance.amount.currency = entry.amount.currency
    if entry.tolerance:
        copy_decimal(entry.tolerance, balance.tolerance)
    if entry.diff_amount:
        copy_decimal(entry.diff_amount.number, balance.diff_amount.number)
        balance.diff_amount.currency = entry.diff_amount.currency
    return pbdir


def convert_Pad(entry: data.Pad) -> pb.Directive:
    pbdir = pb.Directive()
    pad = pbdir.pad
    copy_meta(entry.meta, pbdir.meta, pbdir.location)
    copy_date(entry.date, pbdir.date)
    pad.account = entry.account
    pad.source_account = entry.source_account
    return pbdir


def export_v2_data(filename: str, output_filename: str, num_directives: Optional[int]):
    if output_filename.endswith(".pbtxt"):
        output = open(output_filename, 'w')
        writer = None
        def write(message):
            print(message, file=output)
    else:
        output = open(output_filename, 'wb')
        writer = riegeli.RecordWriter(output)
        write = writer.write_message

    #entries, errors, options_map = loader.load_file(filename)
    entries, errors, options_map = parser.parse_file(filename)
    entries = data.sorted(entries)

    if num_directives:
        entries = itertools.islice(entries, num_directives)
    for entry in entries:
        if isinstance(entry, data.Transaction):
            pbdir = convert_Transaction(entry)
        elif isinstance(entry, data.Open):
            pbdir = convert_Open(entry)
        elif isinstance(entry, data.Close):
            pbdir = convert_Close(entry)
        elif isinstance(entry, data.Commodity):
            pbdir = convert_Commodity(entry)
        elif isinstance(entry, data.Event):
            pbdir = convert_Event(entry)
        elif isinstance(entry, data.Note):
            pbdir = convert_Note(entry)
        elif isinstance(entry, data.Query):
            pbdir = convert_Query(entry)
        elif isinstance(entry, data.Price):
            pbdir = convert_Price(entry)
        elif isinstance(entry, data.Balance):
            pbdir = convert_Balance(entry)
        elif isinstance(entry, data.Pad):
            pbdir = convert_Pad(entry)
        else:
            pbdir = None

        if pbdir is not None:
            write("#---")
            write("# {}".format(pbdir.location.lineno))
            write("#")
            write(pbdir)
            write("")

        if 0:
            print('-' * 80)
            printer.print_entry(entry)
            print(txn)
            print()

    if hasattr(writer, "close"):
        writer.close()
    output.close()


# _SORT_ORDER = {
#     extmodule.BodyCase.kOpen: -2,
#     extmodule.BodyCase.kBalance: -1,
#     extmodule.BodyCase.kDocument: 1,
#     extmodule.BodyCase.kClose: 2,
# }
#
# def entry_sortkey_v3(dir: pb.Directive):
#     type_order = _SORT_ORDER.get(extmodule.GetDirectiveType(dir), 0)
#     return (dir.date, type_order, dir.location.lineno)


def export_v3_data(filename: str, output_filename: str, num_directives: Optional[int]):
    t1 = time.time()
    ledger = extmodule.parse(filename)
    t2 = time.time()
    print((t2 - t1)*1000)
    # directives = extmodule.SortDirectives(ledger.directives)
    # t3 = time.time()
    # print((t3 - t2)*1000)
    directives = ledger.directives
    with open(output_filename, "w") as outfile:
        pr = functools.partial(print,  file=outfile)
        if num_directives:
            directives = itertools.islice(directives, num_directives)
        for directive in directives:
            extmodule.DowngradeToV2(directive);
            pr("#---")
            pr("# {}".format(directive.location.lineno))
            pr("#")
            pr(directive)
            pr()


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename',
                        help="Ledger input filename.")
    parser.add_argument('output', help=(
        "Output filename (if .pbtxt, output as text-formatted protos. "
        "Otherwise write to a riegeli file."))

    parser.add_argument('--num_directives', type=int, default=None,
                        help="Number of entries to print")

    args = parser.parse_args()

    #os.environ["BEANCOUNT_DISABLE_LOAD_CACHE"] = "1"
    t1 = time.time()
    export_v2_data(args.filename, args.output + ".v2.pbtxt", args.num_directives)
    t2 = time.time()
    export_v3_data(args.filename, args.output + ".v3.pbtxt", args.num_directives)
    t3 = time.time()
    print("Export to v2: {:,.0f}ms".format((t2-t1)*1000))
    print("Export to v3: {:,.0f}ms".format((t3-t2)*1000))


if __name__ == '__main__':
    main()
