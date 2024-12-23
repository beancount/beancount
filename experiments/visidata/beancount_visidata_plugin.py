"""Visidata support for Beancount files."""

__copyright__ = "Copyright (C) 2024  Martin Blais"
__license__ = "GNU GPLv2"

import subprocess
from functools import partial
from typing import Iterator

import visidata
from visidata import Column  # noqa: F401
from visidata import ColumnAttr  # noqa: F401
from visidata import ItemColumn  # noqa: F401
from visidata import Progress  # noqa: F401
from visidata import Sheet  # noqa: F401
from visidata import SubColumnAttr  # noqa: F401
from visidata import VisiData  # noqa: F401

import beancount as bn

# Note: guess_beancount is implicit in naming files with ext `.beancount`.


@VisiData.api
def open_beancount(vd, p):
    return BeancountSheet(p.base_stem, source=p)


def iter_postings(entries: list[bn.Directives]) -> Iterator[bn.TxnPosting]:
    """Iterate over all the postings data."""
    for entry in bn.filter_txns(entries):
        for posting in entry.postings:
            yield bn.TxnPosting(entry, posting)


def get_cond_attr(obj, attr):
    return None if obj is None else getattr(obj, attr)


def get_meta(attr, col, row):
    meta = row.posting.meta
    if meta:
        return meta.get(attr, None)


class BeancountSheet(Sheet):
    rowtype = "posting"  # rowdef: foolib.Bar object

    columns = [
        Column(
            "txnid",
            type=visidata.str,
            getter=lambda col, row: hex(id(row.txn))[2:],
        ),
        Column(
            "filename",
            type=visidata.str,
            getter=partial(get_meta, "filename"),
            width=0,
        ),
        Column(
            "lineno",
            type=visidata.str,
            getter=partial(get_meta, "lineno"),
            width=0,
        ),
        Column(
            "date",
            type=visidata.date,
            getter=lambda col, row: row.txn.date,
        ),
        Column(
            "flag",
            type=visidata.str,
            getter=lambda col, row: row.txn.flag,
        ),
        Column(
            "payee",
            type=visidata.str,
            getter=lambda col, row: row.txn.payee,
        ),
        Column(
            "narration",
            type=visidata.str,
            getter=lambda col, row: row.txn.narration,
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


def open_cursor(tp):
    meta = tp.posting.meta
    filename = meta["filename"]
    lineno = meta["lineno"]
    subprocess.call(
        [
            "en",  # Emacs launcher for this tmux session
            "--eval",
            f'(progn (find-file "{filename}") (goto-line {lineno}))',
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


BeancountSheet.addCommand(
    "^O",
    "beancount-open",
    "beancount_visidata_plugin.open_cursor(cursorRow)",
    "Open the source Beancount file with the cursor at the position for the row",
)


# TODO: Add custom aggregators.
# TODO: Add a way to create a balance column from the current sheet.
# TODO: Add a way to perform group-by's from the current sheet.


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
