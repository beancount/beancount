from typing import List, Iterator
from visidata import VisiData, Sheet, Progress
from visidata import ItemColumn, Column, ColumnAttr, SubColumnAttr
import visidata
import beancount as bn
import sys


# Note: guess_beancount is implicit in naming files with ext `.beancount`.


@VisiData.api
def open_beancount(vd, p):
    return BeancountSheet(p.base_stem, source=p)


def iter_postings(entries: List[bn.Directives]) -> Iterator[bn.TxnPosting]:
    """Iterate over all the postings data."""
    for entry in bn.filter_txns(entries):
        for posting in entry.postings:
            yield bn.TxnPosting(entry, posting)


def get_cond_attr(obj, attr):
    return None if obj is None else getattr(obj, attr)


def get_location(col, row):
    meta = row.posting.meta
    if meta:
        return '{:s}:{:d}:'.format(meta['filename'], meta['lineno'])


class BeancountSheet(Sheet):
    rowtype = "posting"  # rowdef: foolib.Bar object

    columns = [
        Column(
            "txnid",
            type=visidata.str,
            getter=lambda col, row: id(row.txn),
        ),
        Column(
            "location",
            type=visidata.str,
            getter=get_location,
            width=0,
        ),
        Column(
            "date",
            type=visidata.date,
            getter=lambda col, row: row.txn.date,
        ),
        Column(
            "account",
            type=visidata.str,
            getter=lambda col, row: row.posting.account,
        ),
        Column(
            "number",
            type=visidata.float,
            getter=lambda col, row: row.posting.units.number,
        ),
        Column(
            "currency",
            type=visidata.str,
            getter=lambda col, row: row.posting.units.currency,
        ),
        Column(
            "cnumber",
            type=visidata.float,
            getter=lambda col, row: get_cond_attr(row.posting.cost, "number"),
        ),
        Column(
            "ccurrency",
            type=visidata.str,
            getter=lambda col, row: get_cond_attr(row.posting.cost, "currency"),
        ),
        Column(
            "cdate",
            type=visidata.date,
            getter=lambda col, row: get_cond_attr(row.posting.cost, "date"),
            width=0,
        ),
        Column(
            "clabel",
            type=visidata.date,
            getter=lambda col, row: get_cond_attr(row.posting.cost, "label"),
            width=0,
        ),
        Column(
            "pnumber",
            type=visidata.float,
            getter=lambda col, row: get_cond_attr(row.posting.price, "number"),
            width=0,
        ),
        Column(
            "pcurrency",
            type=visidata.str,
            getter=lambda col, row: get_cond_attr(row.posting.price, "currency"),
            width=0,
        ),
        Column(
            "tags",
            type=visidata.str,
            getter=lambda col, row: ",".join(row.txn.tags) if row.txn.tags else None,
        ),
        Column(
            "links",
            type=visidata.str,
            getter=lambda col, row: ",".join(row.txn.links) if row.txn.links else None,
            width=0,
        ),
    ]

    def iterload(self):
        entries, options, errors = bn.load_file(str(self.source))
        approx_num_entries = len(entries) * 2
        for tp in Progress(iter_postings(entries), total=approx_num_entries):
            yield tp


# TODO: Add custom aggregators.


"""
Adding custom aggregators
Aggregators allow you to gather the rows within a single column, and interpret them using descriptive statistics. VisiData comes pre-loaded with a default set like mean, stdev, and sum.

To add your own custom aggregator name, add the following to your .visidatarc.

vd.aggregator('name', func, type=float)

Where func is a function of the form:

def func(list):     return value

The type parameter is optional. It allows you to define the default type of the aggregated column.

Here is an example, that adds an aggregator for numpy's internal rate of return module.

import numpy as np vd.aggregator('irr', np.irr, type=float)

Bonus: How to choose which aggregators are columns within the DescribeSheet?

Any numeric aggregator can be added!

Supply a space-separated list of aggregator names to options.describe_aggrs in your .visidatarc.
"""
