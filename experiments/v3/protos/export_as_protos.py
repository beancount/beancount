#!/usr/bin/env python3
"""Export all the processed directives as proto records.

This is an experiment to generate a realistic dataset for a forked query client
as a new, separate project. The dream is that this is all that Beancount would
become: a parser, booking engine and data source provider for a query/api thing.
"""
__copyright__ = "Copyright (C) 2019  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import functools
import itertools
import logging
import datetime
from os import path
from decimal import Decimal

import riegeli

from beancount import loader
from beancount.parser import parser
from beancount.parser import printer
from beancount.core import data
from beancount.core import amount
from beancount.core import position
from beancount.core.number import MISSING

from beancount.ccore import data_pb2 as pb
from beancount.ccore import date_pb2 as db
from beancount.ccore import number_pb2 as nb
from beancount.cparser import parser_pb2 as qb

from beancount.cparser import extmodule
from beancount.cparser import grammar
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
    for key, value in sorted(meta.items()):
        if key == "filename":
            pbloc.filename = value
        elif key == "lineno":
            pbloc.lineno = value
        else:
            item = pbmeta.kv.add()
            item.key = key
            if isinstance(value, bool):
                item.value.boolean = value
            else:
                item.value.text = str(value)
                # FIXME: TODO - convert to more types.


def copy_amount(amt: amount.Amount, pbamt: pb.Amount):
    if amt is not MISSING:
        copy_decimal(amt.number, pbamt.number)
    if amt is not MISSING:
        pbamt.currency = amt.currency


def copy_cost(cost: position.Cost, pbcost: pb.Cost):
    if cost.number is not MISSING:
        copy_decimal(cost.number, pbcost.number)
    if cost.currency is not None:
        pbcost.currency = cost.currency
    if cost.date is not None:
        copy_date(cost.date, pbcost.date)
    if cost.label:
        pbcost.label = cost.label


def copy_cost_spec(cost: position.CostSpec, pbcost: qb.CostSpec):
    if cost.number_per is not MISSING:
        copy_decimal(cost.number_per, pbcost.number_per)
    if cost.number_total is not MISSING:
        copy_decimal(cost.number_total, pbcost.number_total)
    if cost.currency is not None:
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
        pbdir.tags.extend(entry.tags)
    if entry.links:
        pbdir.links.extend(entry.links)

    if entry.flag:
        txn.flag = entry.flag.encode('utf8')
    if entry.payee:
        txn.payee = entry.payee
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
    # TODO(blais): Add enum
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


def export_v2_data(filename: str, output_filename: str, num_directives: int):
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

    for entry in itertools.islice(entries, num_directives):
        if isinstance(entry, data.Transaction):
            pbdir = convert_Transaction(entry)
        elif isinstance(entry, data.Open):
            pbdir = convert_Open(entry)
        elif isinstance(entry, data.Close):
            pbdir = convert_Close(entry)
        elif isinstance(entry, data.Commodity):
            pbdir = convert_Commodity(entry)
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


_SORT_ORDER = {
    extmodule.BodyCase.kOpen: -2,
    extmodule.BodyCase.kBalance: -1,
    extmodule.BodyCase.kDocument: 1,
    extmodule.BodyCase.kClose: 2,
}


def entry_sortkey_v3(dir: pb.Directive):
    type_order = _SORT_ORDER.get(extmodule.GetDirectiveType(dir), 0)
    return (dir.date, type_order, dir.location.lineno)


def export_v3_data(filename: str, output_filename: str, num_directives: int):
    ledger = extmodule.parse(filename)
    directives = sorted(ledger.directives, key=entry_sortkey_v3)
    with open(output_filename, "w") as outfile:
        pr = functools.partial(print,  file=outfile)
        for directive in itertools.islice(directives, num_directives):
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

    parser.add_argument('--num_directives', type=int, default=1000,
                        help="Number of entries to print")

    args = parser.parse_args()

    export_v2_data(args.filename, args.output + ".v2.pbtxt", args.num_directives)
    export_v3_data(args.filename, args.output + ".v3.pbtxt", args.num_directives)


if __name__ == '__main__':
    main()
