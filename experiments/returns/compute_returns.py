#!/usr/bin/env python3
"""Calculate my true returns, including dividends and real costs.

Notes:

- The calculation without dividends only accounts for cash dividends, not
  reinvested dividends.

TODO(blais): Compare to a benchmark portfolio with the same cash flows.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"


# pylint: disable=wrong-import-order,wrong-import-position

from dateutil.relativedelta import relativedelta
from os import path
from pprint import pprint
from typing import Any, Dict, List, Set, Tuple, Optional
from functools import partial
import argparse
import collections
import copy
import datetime
import enum
import fnmatch
import io
import itertools
import json
import logging
import multiprocessing
import os
import re
import subprocess
import tempfile
import time
import typing

import numpy as np
#import numpy_financial as npf
from scipy.optimize import fsolve

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn
seaborn.set()

from beancount import loader
from beancount.core import account as accountlib
from beancount.core import account_types as acctypes
from beancount.core import display_context
from beancount.core import convert
from beancount.core import data
from beancount.core import getters
from beancount.core import prices
from beancount.core.number import ZERO
from beancount.core.amount import Amount
from beancount.core.inventory import Inventory
from beancount.core.inventory import Position
from beancount.parser import options
from beancount.parser import printer


# Basic type aliases.
Account = str
Currency = str
Date = datetime.date
Array = np.ndarray  # pylint: disable=invalid-name


# The date at which we evaluate this.
TODAY = Date.today()


# Al list of dated cash flows. This is the unit that this program operates in,
# the sanitized time-series that allows us to compute returns.
CashFlow = typing.NamedTuple("CashFlow", [
    ("date", Date),
    ("amount", Amount),     # The amount of the cashflow.
    ("is_dividend", bool),  # True if the flow is a dividend.
    ("balance", Inventory), # Balance after this cash flow.
])


CurrencyPair = Tuple[Currency, Currency]


class Pricer:
    """A price database that remembers the queried prices and dates."""

    def __init__(self, price_map: prices.PriceMap):
        self.price_map = price_map
        self.required_prices = collections.defaultdict(set)

    def get_value(self, pos, date):
        """Return the value and save the conversion rate."""
        price_dates = []
        price = convert.get_value(pos, self.price_map, date, price_dates)

        # Add prices found to the list of queried ones.
        for found_date, found_rate in price_dates:
            self.required_prices[(pos.units.currency, date)].add(
                (price.currency, found_date, found_rate))

        return price

    def convert_amount(self, amount, target_currency, date):
        # TODO(blais): Save the amount here too.
        return convert.convert_amount(amount, target_currency, self.price_map, date=date)


def find_accounts(entries: data.Entries,
                  options_map: data.Options,
                  start: Date) -> List[Account]:
    """Return a list of account names from the balance sheet which either aren't
    closed or are closed now but were still open at the given start date.
    """
    commodities = getters.get_commodity_directives(entries)
    open_close_map = getters.get_account_open_close(entries)
    atypes = options.get_account_types(options_map)
    return sorted(
        account
        for account, (_open, _close) in open_close_map.items()
        if (accountlib.leaf(account) in commodities and
            acctypes.is_balance_sheet_account(account, atypes) and
            not acctypes.is_equity_account(account, atypes) and
            (_close is None or _close.date > start)))


def prune_entries(entries: data.Entries) -> data.Entries:
    """Prune the list of entries to exclude all transactions that include a
    commodity name in at least one of its postings. This speeds up the
    recovery process by removing the majority of non-trading transactions."""
    commodities = getters.get_commodity_directives(entries)
    regexp = re.compile(r"\b({})\b".format("|".join(commodities.keys()))).search
    return [entry
            for entry in entries
            if (isinstance(entry, (data.Open, data.Commodity)) or
                (isinstance(entry, data.Transaction) and
                 any(regexp(posting.account) for posting in entry.postings)))]


def transactions_for_account(entries: data.Entries,
                             account: Account) -> data.Entries:
    """Get the list of transactions affecting an investment account."""

    # Main matcher that will pull in related transactions.
    accounts_regexp = re.sub("[A-Za-z]+:", "(.*):", account, 1) + "(:Dividends?)?$"

    # Check that no other account has the leaf component in its name.
    # accounts = set(posting.account
    #                for entry in data.filter_txns(entries)
    #                for posting in entry.postings)
    # leaf = accountlib.leaf(account)
    # for acc in accounts:
    #     if not re.match(accounts_regexp, acc) and re.match(r"\b{}\b".format(leaf), acc):
    #         print("XXX", acc)

    # Figure out the total set of accounts seed in those transactions.
    return [entry
            for entry in data.filter_txns(entries)
            if any(re.match(accounts_regexp, posting.account)
                   for posting in entry.postings)]


class Cat(enum.Enum):
    """Posting categorization.

    This is used to produce unique templates to categorize each transaction. A
    template is a set of the categories below, which each of a transaction's
    postings are classified with.
    """

    # The account holding the commodity.
    ASSET = 1

    # Transfers from other assets, cash accounts, or employer matches or
    # contributions.
    CASH = 2

    # Income accounts.
    PNL = 3
    INTEREST = 4
    DIVIDEND = 5

    # Misc adjustment accounts.
    ROUNDING = 6

    # Trades in other commodities.
    OTHERASSET = 7
    OTHERDIVIDEND = 8

    # Commissions, fees and other expenses.
    EXPENSES = 9

    # Currency conversion transactions.
    CONVERSIONS = 10

    # Mirror tracking accounts in other currencies.
    TRACKING = 11

    # Uncategorized.
    UNKNOWN = 12


IGNORE_CATEGORIES = {Cat.ROUNDING, Cat.TRACKING}


# pylint: disable=too-many-branches
def categorize_accounts(account: Account,
                        accounts: Set[Account],
                        atypes: tuple) -> Dict[Account, Cat]:
    """Categorize the type of accounts for a particular stock. Our purpose is to
    make the types of postings generic, so they can be categorized and handled
    generically later on.

    The patterns used in this file depend on the particular choices of account
    names in my chart of accounts, and for others to use this, this needs to be
    specialized somewhat.
    """
    accpath = accountlib.join(*accountlib.split(account)[1:-1])
    currency = accountlib.leaf(account)

    accounts = set(accounts)
    catmap = {}
    def move(acc, category):
        if acc in accounts:
            accounts.remove(acc)
            catmap[acc] = category

    # The account itself.
    move(account, Cat.ASSET)

    # The adjacent cash account.
    move(accountlib.join(atypes.assets, accpath, "Cash"), Cat.CASH)

    # The adjacent P/L and interest account.
    move(accountlib.join(atypes.income, accpath, "PnL"), Cat.PNL)
    move(accountlib.join(atypes.income, accountlib.parent(accpath), "PnL"), Cat.PNL)
    move(accountlib.join(atypes.income, accpath, "Interest"), Cat.INTEREST)

    # The associated dividend account.
    move(accountlib.join(atypes.income, accpath, currency, "Dividend"), Cat.DIVIDEND)

    # Rounding error account.
    move("Equity:RoundingError", Cat.ROUNDING)
    move("Expenses:Losses", Cat.ROUNDING)
    move("Income:US:MSSB:RoundingVariance", Cat.ROUNDING)

    for acc in list(accounts):

        # Employer match and corresponding tracking accounts in IRAUSD.
        if re.match("({}|{}):.*:Match401k".format(atypes.assets, atypes.expenses), acc):
            move(acc, Cat.TRACKING)
        elif re.search(r"\b(Vested|Unvested)", acc):
            move(acc, Cat.TRACKING)

        # Dividends for other stocks.
        elif re.search(r":Dividends?$", acc):
            move(acc, Cat.OTHERDIVIDEND)

        # Direct contribution from employer.
        elif re.match("{}:.*:Match401k$".format(atypes.income), acc):
            move(acc, Cat.CASH)
        elif re.search(":GSURefund$", acc):
            move(acc, Cat.CASH)

        # Expenses accounts.
        elif acc.startswith(atypes.expenses):
            move(acc, Cat.EXPENSES)
        elif acc.endswith(":Commissions"):  # Income..Commissions
            move(acc, Cat.EXPENSES)

        # Currency accounts.
        elif acc.startswith("Equity:CurrencyAccounts"):
            move(acc, Cat.CONVERSIONS)

        # Other cash or checking accounts.
        elif acc.endswith(":Cash"):
            move(acc, Cat.CASH)
        elif acc.endswith(":Checking"):
            move(acc, Cat.CASH)
        elif acc.endswith("Receivable"):
            move(acc, Cat.CASH)

        # Other stock.
        elif re.match("{}:[A-Z_.]+$".format(accountlib.parent(acc)), acc):
            move(acc, Cat.OTHERASSET)

        else:
            print("ERROR: Unknown account: {}".format(acc))
            move(acc, Cat.UNKNOWN)

    return catmap


def compute_transaction_signature(catmap: Dict[Account, Cat],
                                  entry: data.Directive) -> Tuple[Cat]:
    """Compute a unique signature for each transaction.
    Also annotates (mutates) each posting with its category."""
    categories = set()
    for posting in entry.postings:
        category = catmap[posting.account]
        posting.meta["category"] = category
        if category not in IGNORE_CATEGORIES:
            categories.add(category)
    sigtuple = tuple(sorted(categories, key=lambda item: item.value))
    return "_".join(s.name for s in sigtuple)


_KNOWN_SIGNATURES = {
    (Cat.ASSET, Cat.CASH): "Purchase or sale",
    (Cat.ASSET, Cat.CASH, Cat.EXPENSES): "Purchase or sale with commission",

    (Cat.ASSET, Cat.CASH, Cat.PNL): "Purchase or sale and profit",
    (Cat.ASSET, Cat.CASH, Cat.PNL, Cat.EXPENSES): ("Purchase or sale with commission "
                                                   "and profit"),
    (Cat.ASSET, Cat.EXPENSES): "Fee paid from liquidation",

    (Cat.ASSET, Cat.PNL): "Cost basis adjustment (with P/L)",
    (Cat.ASSET, Cat.PNL, Cat.EXPENSES): "Fee from liquidation (with P/L)",
    (Cat.ASSET,): "Conversion",

    (Cat.ASSET, Cat.DIVIDEND): "Dividend reinvested",

    (Cat.CASH, Cat.DIVIDEND): "Dividend payment",
    (Cat.CASH, Cat.PNL, Cat.DIVIDEND): "Dividend payment and gains distribution",

    # This is a split to two stocks (Google), we'll have to do something
    # special, a single transaction.
    (Cat.ASSET, Cat.OTHERASSET): "Exchange of stock/symbol",
}
KNOWN_SIGNATURES = {"_".join(s.name for s in sig): desc
                    for sig, desc in _KNOWN_SIGNATURES.items()}



_signature_registry = {}
def register(*categories):
    """Registers a handler for a particular template/signature transaction."""
    def decorator(func):
        key = "_".join(s.name for s in categories)
        _signature_registry[key] = func
        return func
    return decorator


def produce_cash_flows(entry: data.Directive) -> List[CashFlow]:
    """Produce cash flows using the signature of the transaction."""
    sig = entry.meta["signature"]
    handler = _signature_registry[sig]
    return handler(entry)


@register(Cat.ASSET)
def handle_empty(_: data.Directive) -> List[CashFlow]:
    "Assets exchanges create no flows."
    return []


@register(Cat.ASSET, Cat.CASH)
@register(Cat.ASSET, Cat.CASH, Cat.EXPENSES)
@register(Cat.ASSET, Cat.CASH, Cat.PNL)
@register(Cat.ASSET, Cat.CASH, Cat.PNL, Cat.EXPENSES)
def handle_one_asset(entry: data.Directive) -> List[CashFlow]:
    "Regular purchases or sales."
    flows = []
    for posting in entry.postings:
        category = posting.meta["category"]
        if category in IGNORE_CATEGORIES:
            pass
        elif category in {Cat.ASSET, Cat.PNL, Cat.INTEREST}:
            pass
        elif category == Cat.CASH:
            assert not posting.cost
            flows.append(CashFlow(entry.date, posting.units, False, None))
        elif category == Cat.EXPENSES:
            # Expenses are already accounted for by the cash leg.
            pass
        else:
            raise ValueError("Unsupported category: {}".format(category))
    return flows


@register(Cat.CASH, Cat.DIVIDEND)
@register(Cat.CASH, Cat.PNL, Cat.DIVIDEND)
def handle_dividends(entry: data.Directive) -> List[CashFlow]:
    "Dividends received, sometimes with P/L for LT or ST gains."
    flows = []
    for posting in entry.postings:
        category = posting.meta["category"]
        if category in IGNORE_CATEGORIES:
            pass
        elif category in {Cat.PNL, Cat.DIVIDEND}:
            pass
        elif category == Cat.CASH:
            assert not posting.cost
            flows.append(CashFlow(entry.date, posting.units, True, None))
        else:
            raise ValueError("Unsupported category: {}".format(category))
    return flows


@register(Cat.ASSET, Cat.DIVIDEND)
def handle_dividend_reinvestments(_: data.Directive) -> List[CashFlow]:
    """This remains internal, the money is moved to more of the asset.
    Note that because of this, it would make it very difficult to remove the
    dividend from the performance of this asset. The total returns should
    still be calculated correctly though."""
    return []


@register(Cat.ASSET, Cat.EXPENSES)
def handle_fee_from_liquidation(_: data.Directive) -> List[CashFlow]:
    """Fees paid purely from sales (with expenses). No in or out flows, value is reduced."""
    return []


@register(Cat.ASSET, Cat.PNL)
@register(Cat.ASSET, Cat.PNL, Cat.EXPENSES)
def handle_cost_basis_adjustments(_: data.Directive) -> List[CashFlow]:
    """No cash is disbursed for these adjustments, just a change in basis. This
    affects tax only. There are no associated cash flows."""
    return []


@register(Cat.ASSET, Cat.OTHERASSET)
def handle_failing(entry: data.Directive) -> List[CashFlow]:
    """This is for the GOOG/GOOGL stock exchange."""
    flows = []
    for posting in entry.postings:
        category = posting.meta["category"]
        if category == Cat.ASSET:
            flows.append(CashFlow(entry.date, -convert.get_weight(posting),
                                  False, None))
        elif category == Cat.OTHERASSET:
            pass
        else:
            raise ValueError("Unsupported category: {}".format(category))
    return flows


def net_present_value(irr: float, cash_flows: Array, years: Array):
    """Net present value; objective function for optimizer."""
    return np.sum(cash_flows / (1. + irr) ** years)


def compute_irr(dated_flows: List[CashFlow],
                pricer: Pricer,
                target_currency: Currency) -> float:
    """Compute the irregularly spaced IRR."""

    # Array of cash flows, converted to USD.
    usd_flows = []
    for flow in dated_flows:
        usd_amount = pricer.convert_amount(flow.amount, target_currency, date=flow.date)
        usd_flows.append(float(usd_amount.number))
    cash_flows = np.array(usd_flows)

    # Array of time in years.
    years = []
    for flow in dated_flows:
        years.append((flow.date - TODAY).days / 365)
    years = np.array(years)

    # Start with something reasonably normal.
    estimated_irr = 0.20

    # Solve for the root of the NPV equation.
    irr, unused_infodict, unused_ier, unused_mesg = fsolve(
        net_present_value, x0=estimated_irr, args=(cash_flows, years),
        full_output=True)
    return irr.item()


Returns = typing.NamedTuple("Returns", [
    ('groupname', str),
    ('first_date', Date),
    ('last_date', Date),
    ('years', float),
    ("total", float),
    ("exdiv", float),
    ("div", float),
    ("flows", List[CashFlow]),
])


def compute_returns(flows: List[CashFlow],
                    pricer: Pricer,
                    target_currency: Currency) -> Returns:
    """Compute the returns from a list of cash flows."""
    if not flows:
        return Returns("?", TODAY, TODAY, 0, 0, 0, 0, [])
    flows = sorted(flows, key=lambda cf: cf.date)
    irr = compute_irr(flows, pricer, target_currency)

    flows_exdiv = [flow for flow in flows if not flow.is_dividend]
    irr_exdiv = compute_irr(flows_exdiv, pricer, target_currency)

    first_date = flows[0].date
    last_date = flows[-1].date
    years = (last_date - first_date).days / 365
    return Returns("?", first_date, last_date, years,
                   irr, irr_exdiv, (irr - irr_exdiv),
                   flows)


def copy_and_normalize(entry: data.Transaction) -> data.Transaction:
    """Copy entries and make sure all postings have valid metadata."""
    entry = copy.deepcopy(entry)
    postings = []
    for posting in entry.postings:
        if posting.meta is None:
            posting = posting._replace(meta={})
        postings.append(posting)
    return entry._replace(postings=postings)


# All flow information associated with an account.
AccountData = typing.NamedTuple("AccountData", [
    ('account', Account),
    ('currency', Currency),
    ('cost_currency', Currency),
    ('commodity', data.Commodity),
    ("cash_flows", List[CashFlow]),
    ('transactions', data.Entries),
    ('catmap', Dict[Account, Cat]),
])


def process_account_entries(entries: data.Entries,
                            options_map: data.Options,
                            account: Account) -> AccountData:
    """Process a single account."""
    logging.info("Processing account: %s", account)

    # Extract the relevant transactions.
    transactions = transactions_for_account(entries, account)
    if not transactions:
        logging.warning("No transactions for %s; skipping.", account)
        return transactions, None, None

    # Categorize the set of accounts encountered in the filtered transactions.
    seen_accounts = {posting.account
                     for entry in transactions
                     for posting in entry.postings}
    atypes = options.get_account_types(options_map)
    catmap = categorize_accounts(account, seen_accounts, atypes)

    # Process each of the transactions, adding derived values as metadata.
    cash_flows = []
    balance = Inventory()
    decorated_transactions = []
    for entry in transactions:

        # Update the total position in the asset we're interested in.
        positions = []
        for posting in entry.postings:
            category = catmap[posting.account]
            if category is Cat.ASSET:
                balance.add_position(posting)
                positions.append(posting)

        # Compute the signature of the transaction.
        entry = copy_and_normalize(entry)
        signature = compute_transaction_signature(catmap, entry)
        entry.meta["signature"] = signature
        entry.meta["description"] = KNOWN_SIGNATURES[signature]

        # Compute the cash flows associated with the transaction.
        flows = produce_cash_flows(entry)
        entry.meta['cash_flows'] = flows

        cash_flows.extend(flow._replace(balance=copy.deepcopy(balance))
                          for flow in flows)
        decorated_transactions.append(entry)

    currency = accountlib.leaf(account)

    cost_currencies = set(cf.amount.currency for cf in cash_flows)
    assert len(cost_currencies) == 1, str(cost_currencies)
    cost_currency = cost_currencies.pop()

    commodity_map = getters.get_commodity_directives(entries)
    comm = commodity_map[currency]

    return AccountData(account, currency, cost_currency, comm, cash_flows,
                       decorated_transactions, catmap)


def find_balance_before(cash_flows: List[CashFlow],
                        date: Date) -> Tuple[Inventory, int]:
    """Return the balance just before the given date in the sorted list of cash flows."""
    balance = Inventory()
    for index, flow in enumerate(cash_flows):
        if flow.date >= date:
            break
        balance = flow.balance
    else:
        index = len(cash_flows)
    return balance, index


def truncate_and_merge_cash_flows(
        pricer: Pricer,
        cash_flows_list: List[List[CashFlow]],
        date_start: Optional[Date] = None,
        date_end: Optional[Date] = None) -> List[CashFlow]:
    """Truncate and merge a list of cash flows for processing, inserting initial
    and/or final cash flows from balances if necessary."""

    truncated_flows = []
    for cash_flows in cash_flows_list:
        cash_flows = list(cash_flows)

        # Truncate cash flows before the given interval.
        if date_start is not None:
            balance, index = find_balance_before(cash_flows, date_start)
            if index > 0:
                cash_flows = cash_flows[index:]
            if not balance.is_empty():
                cost_balance = balance.reduce(pricer.get_value, date_start)
                cost_position = cost_balance.get_only_position()
                if cost_position:
                    cash_flows.insert(
                        0, CashFlow(date_start, -cost_position.units, False, balance))

        # Truncate cash flows after the given interval.
        if date_end is None:
            # If no end date is specified, use today.
            date_end = TODAY
        balance, index = find_balance_before(cash_flows, date_end)
        if index < len(cash_flows):
            cash_flows = cash_flows[:index]
        if not balance.is_empty():
            cost_balance = balance.reduce(pricer.get_value, date_end)
            cost_position = cost_balance.get_only_position()
            if cost_position:
                cash_flows.append(
                    CashFlow(date_end, cost_position.units, False, balance))

        truncated_flows.extend(cash_flows)

    return sorted(truncated_flows, key=lambda cf: cf.date)


def get_account_groups(entries: data.Entries,
                       account_data: List[AccountData],
                       render_open: bool,
                       render_closed: bool) -> Dict[str, List[AccountData]]:
    """Logically group accounts for reporting."""
    groups = collections.defaultdict(list)
    open_close_map = getters.get_account_open_close(entries)
    for accdata in account_data:
        opn, cls = open_close_map[accdata.account]
        assert opn
        is_open = cls is None and not accdata.cash_flows[-1].balance.is_empty()
        if not (render_open if is_open else render_closed):
            continue
        prefix = "open" if is_open else "closed"
        group = "{}.{}".format(prefix, accdata.currency)
        groups[group].append(accdata)
    return dict(groups)


IRR_FORMAT = "{:32}: {:6.2%} ({:6.2%} ex-div, {:6.2%} div)"


def write_account_file(dcontext: display_context.DisplayContext,
                       account_data: AccountData,
                       filename: str):
    """Write out a file with details, for inspection and debugging."""

    logging.info("Writing details file: %s", filename)
    epr = printer.EntryPrinter(dcontext=dcontext, stringify_invalid_types=True)
    with open_with_mkdir(filename) as outfile:
        fprint = partial(print, file=outfile)
        fprint(";; -*- mode: beancount; coding: utf-8; fill-column: 400 -*-")

        # Print front summary section.
        fprint("* Summary\n")
        fprint("Account: {}".format(account_data.account))

        final_cf = account_data.cash_flows[-1]
        units_balance = final_cf.balance.reduce(convert.get_units)
        fprint("Balance: {}".format(units_balance))

        # Print out those details.
        fprint("** Category map\n")
        fprint()
        pprint(account_data.catmap, stream=outfile)
        fprint("\n\n")

        fprint("** Transactions\n")
        for entry in account_data.transactions:
            fprint(epr(entry))
        fprint("\n\n")

        fprint("** Cash flows\n")
        for flow in account_data.cash_flows:
            fprint(flow)
        fprint("\n\n")


STYLE = """
@media print {
    @page { margin: 0in; }
    body { margin: 0.2in; }
}

body, table { font: 9px Noto Sans, sans-serif; }

p { margin: 0.2em; }

table { border-collapse: collapse; }
table td, table th { border: thin solid black; }

table.full { width: 100%; }

/* p { margin-bottom: .1em } */
"""

RETURNS_TEMPLATE_PRE = """
<html>
  <head>
    <title>{title}</title>
    <link href="https://fonts.googleapis.com/css2?family=Noto+Sans&display=swap" rel="stylesheet">

    <style>
      {style}
    </style>
  <head>
    <body>
      <h1>{title}</h1>
"""
RETURNS_TEMPLATE_POST = """
    </body>
</html>
"""


Table = typing.NamedTuple("Table", [("header", List[str]),
                                    ("rows", List[List[Any]])])

def render_table(table: Table,
                 floatfmt: Optional[str] = None,
                 classes: Optional[str] = None) -> str:
    """Render a simple data table to HTML."""
    oss = io.StringIO()
    fprint = partial(print, file=oss)
    fprint('<table class="{}">'.format(" ".join(classes or [])))
    fprint('<tr>')
    for heading in table.header:
        fprint("<th>{}</th>".format(heading))
    fprint('</tr>')
    for row in table.rows:
        fprint('<tr>')
        for value in row:
            if isinstance(value, float) and floatfmt:
                value = floatfmt.format(value)
            fprint("<td>{}</td>".format(value))
        fprint('</tr>')
    fprint("</table>")
    return oss.getvalue()


# A named date interval: (name, start date, end date).
Interval = Tuple[str, Date, Date]


def compute_returns_table(pricer: Pricer,
                          target_currency: Currency,
                          cash_flows_list: List[List[CashFlow]],
                          intervals: List[Interval]):
    """Compute a table of sequential returns."""
    header = ["Return"]
    rows = [["Total"], ["Ex-div"], ["Div"]]
    for intname, date1, date2 in intervals:
        header.append(intname)
        cash_flows = truncate_and_merge_cash_flows(pricer, cash_flows_list,
                                                   date1, date2)
        if 0:
            print("===;", intname, date1, "->", date2)
            for cf in cash_flows:
                print(cf.date, cf.amount)
            print()

        returns = compute_returns(cash_flows, pricer, target_currency)
        rows[0].append(returns.total)
        rows[1].append(returns.exdiv)
        rows[2].append(returns.div)
    return Table(header, rows)


def write_returns_pdf(pricer: Pricer,
                      account_data: List[AccountData],
                      title: str,
                      pdf_filename: str,
                      target_currency: Optional[Currency] = None) -> subprocess.Popen:
    """Write out returns for a combined list of account account_data.."""

    logging.info("Writing returns file for %s: %s", title, pdf_filename)
    with tempfile.TemporaryDirectory() as tmpdir:
        indexfile = write_returns_html(pricer, account_data, title, tmpdir, target_currency)
        command = ["google-chrome", "--headless", "--disable-gpu",
                   "--print-to-pdf={}".format(pdf_filename), indexfile]
        subprocess.check_call(command, stderr=open("/dev/null", "w"))
        assert path.exists(pdf_filename)
        logging.info("Done: file://%s", pdf_filename)


def write_returns_html(pricer: Pricer,
                       account_data: List[AccountData],
                       title: str,
                       dirname: str,
                       target_currency: Optional[Currency] = None) -> subprocess.Popen:
    """Write out returns report to a directory with files in it."""

    logging.info("Writing returns dir for %s: %s", title, dirname)
    os.makedirs(dirname, exist_ok=True)
    with open(path.join(dirname, "index.html"), "w") as indexfile:
        fprint = partial(print, file=indexfile)
        fprint(RETURNS_TEMPLATE_PRE.format(style=STYLE, title=title))

        if not target_currency:
            cost_currencies = set(r.cost_currency for r in account_data)
            target_currency = cost_currencies.pop()
            assert not cost_currencies, (
                "Incompatible cost currencies {} for accounts {}".format(
                    cost_currencies, ",".join([r.account for r in account_data])))

        #fprint("<h2>Accounts</h2>")
        fprint("<p>Cost Currency: {}</p>".format(target_currency))
        for ad in account_data:
            fprint("<p>Account: {} ({})</p>".format(ad.account,
                                                    ad.commodity.meta["name"]))

        # TOOD(blais): Prices should be plot separately, by currency.
        # fprint("<h2>Prices</h2>")
        # pairs = set((r.currency, r.cost_currency) for r in account_data)
        # plots = plot_prices(dirname, pricer.price_map, pairs)
        # for _, filename in sorted(plots.items()):
        #     fprint('<img src={} style="width: 100%"/>'.format(filename))

        fprint("<h2>Cash Flows</h2>")
        cash_flows_list = [ad.cash_flows for ad in account_data]
        cash_flows = truncate_and_merge_cash_flows(pricer, cash_flows_list)
        transactions = data.sorted([txn for ad in account_data for txn in ad.transactions])

        returns = compute_returns(cash_flows, pricer, target_currency)
        # Note: This is where the vast majority of the time is spent.
        plots = plot_flows(dirname, pricer.price_map,
                           cash_flows, transactions, returns.total)
        fprint('<img src={} style="width: 100%"/>'.format(plots["flows"]))
        fprint('<img src={} style="width: 100%"/>'.format(plots["cumvalue"]))

        fprint("<h2>Returns</h2>")
        fprint(render_table(Table(["Total", "Ex-Div", "Div"],
                                  [[returns.total, returns.exdiv, returns.div]]),
                            floatfmt="{:.2%}"))

        # Compute table of returns over intervals.
        table = compute_returns_table(pricer, target_currency, cash_flows_list,
                                      get_calendar_intervals(TODAY))
        fprint("<p>", render_table(table, floatfmt="{:.1%}", classes=["full"]), "</p>")

        table = compute_returns_table(pricer, target_currency, cash_flows_list,
                                      get_cumulative_intervals(TODAY))
        fprint("<p>", render_table(table, floatfmt="{:.1%}", classes=["full"]), "</p>")

        fprint(RETURNS_TEMPLATE_POST)

    return indexfile.name


def set_axis(ax_, date_min, date_max):
    """Setup X axis for dates."""

    years = mdates.YearLocator()
    years_fmt = mdates.DateFormatter('%Y')
    months = mdates.MonthLocator()

    ax_.xaxis.set_major_locator(years)
    ax_.xaxis.set_major_formatter(years_fmt)
    ax_.xaxis.set_minor_locator(months)

    datemin = np.datetime64(date_min, 'Y')
    datemax = np.datetime64(date_max, 'Y') + np.timedelta64(1, 'Y')
    ax_.set_xlim(datemin, datemax)

    ax_.format_xdata = mdates.DateFormatter('%Y-%m-%d')
    ax_.format_ydata = "{:,}".format
    ax_.grid(True)


def plot_prices(output_dir: str,
                price_map: prices.PriceMap,
                pairs: List[CurrencyPair]) -> Dict[str, str]:
    """Render one or more plots of prices."""

    # Group by quote currencies.
    outplots = {}
    series = collections.defaultdict(list)
    for c, qc in pairs:
        series[qc].append(c)

    fig, axs = plt.subplots(len(series), 1, sharex=True, figsize=[10, 2 * len(series)])
    if len(series) == 1: axs = [axs]
    for index, (qc, currencies) in enumerate(sorted(series.items())):
        ax = axs[index]
        for currency in currencies:
            price_points = prices.get_all_prices(price_map, (currency, qc))

            # Render cash flows.
            dates = [date for date, _ in price_points]
            prices_ = [float(price) for _, price in price_points]

            set_axis(ax, dates[0], dates[-1])
            ax.plot(dates, prices_, linewidth=0.3)
            ax.scatter(dates, prices_, s=1.2)

    fig.autofmt_xdate()
    fig.tight_layout()
    filename = outplots["price"] = path.join(output_dir, "price.svg")
    plt.savefig(filename)
    plt.close(fig)

    return outplots


def plot_flows(output_dir: str,
               price_map: prices.PriceMap,
               flows: List[CashFlow],
               transactions: data.Entries,
               returns_rate: float) -> Dict[str, str]:
    """Produce plots from cash flows and returns, and more."""

    # Render cash flows.
    outplots = {}
    dates = [f.date for f in flows]
    dates_exdiv = [f.date for f in flows if not f.is_dividend]
    dates_div = [f.date for f in flows if f.is_dividend]
    #amounts = np.array([f.amount.number for f in flows])
    amounts_exdiv = np.array([f.amount.number for f in flows if not f.is_dividend])
    amounts_div = np.array([f.amount.number for f in flows if f.is_dividend])

    fig, axs = plt.subplots(2, 1, sharex=True, figsize=[10, 4],
                            gridspec_kw={'height_ratios': [3, 1]})
    for ax in axs:
        set_axis(ax, dates[0], dates[-1])
        ax.axhline(0, color='#000', linewidth=0.2)
        ax.vlines(dates_exdiv, 0, amounts_exdiv, linewidth=3, color='#000', alpha=0.7)
        ax.vlines(dates_div, 0, amounts_div, linewidth=3, color='#0A0', alpha=0.7)
    axs[1].set_yscale('symlog')

    axs[0].set_title("Cash Flows")
    axs[1].set_title("Cash Flows (log)")
    fig.autofmt_xdate()
    fig.tight_layout()
    filename = outplots["flows"] = path.join(output_dir, "flows.svg")
    plt.savefig(filename)
    plt.close(fig)

    # Render cumulative cash flows, with returns growth.
    date_min = dates[0] - datetime.timedelta(days=1)
    date_max = dates[-1]
    num_days = (date_max - date_min).days
    dates_all = [dates[0] + datetime.timedelta(days=x) for x in range(num_days)]
    gamounts = np.zeros(num_days)
    rate = (1 + returns_rate) ** (1./365)
    for flow in flows:
        remaining_days = (date_max - flow.date).days
        amt = -float(flow.amount.number)
        if remaining_days > 0:
            gflow = amt * (rate ** np.arange(0, remaining_days))
            gamounts[-remaining_days:] += gflow
        else:
            gamounts[-1] += amt

    fig, ax = plt.subplots(figsize=[10, 4])
    ax.set_title("Cumulative value")
    set_axis(ax, dates[0], dates[-1])
    lw = 0.8
    ax.axhline(0, color='#000', linewidth=lw)

    #ax.scatter(dates_all, gamounts, color='#000', alpha=0.2, s=1.0)
    ax.plot(dates_all, gamounts, color='#000', alpha=0.7, linewidth=lw)

    # Overlay value of assets over time.
    value_dates, value_values = compute_portfolio_values(price_map, transactions)
    ax.plot(value_dates, value_values, color='#00F', alpha=0.5, linewidth=lw)
    ax.scatter(value_dates, value_values, color='#00F', alpha=lw, s=2)

    ax.legend(["Amortized value from flows", "Market value"], fontsize="xx-small")
    fig.autofmt_xdate()
    fig.tight_layout()
    filename = outplots["cumvalue"] = path.join(output_dir, "cumvalue.svg")
    plt.savefig(filename)
    plt.close(fig)

    return outplots


def compute_portfolio_values(price_map: prices.PriceMap,
                             transactions: data.Entries) -> Tuple[List[Date], List[float]]:
    """Compute a serie of portfolio values over time."""

    # Infer the list of required prices.
    currency_pairs = set()
    for entry in transactions:
        for posting in entry.postings:
            if posting.meta["category"] is Cat.ASSET:
                if posting.cost:
                    currency_pairs.add((posting.units.currency, posting.cost.currency))

    first = lambda x: x[0]
    price_dates = sorted(itertools.chain(
        ((date, None)
         for pair in currency_pairs
         for date, _ in prices.get_all_prices(price_map, pair)),
        ((entry.date, entry)
         for entry in transactions)), key=first)

    # Iterate computing the balance.
    value_dates = []
    value_values = []
    balance = Inventory()
    for date, group in itertools.groupby(price_dates, key=first):
        # Update balances.
        for _, entry in group:
            if entry is None:
                continue
            for posting in entry.postings:
                if posting.meta["category"] is Cat.ASSET:
                    balance.add_position(posting)

        # Convert to market value.
        value_balance = balance.reduce(convert.get_value, price_map, date)
        cost_balance = value_balance.reduce(convert.convert_position, "USD", price_map)
        pos = cost_balance.get_only_position()
        value = pos.units.number if pos else ZERO

        # Add one data point.
        value_dates.append(date)
        value_values.append(value)

    return value_dates, value_values


def write_transactions_by_type(output_signatures: str,
                               account_data: AccountData,
                               dcontext: display_context.DisplayContext):
    """Write files of transactions by signature, for debugging."""

    # Build signature map.
    signature_map = collections.defaultdict(list)
    for accdata in account_data:
        for entry in accdata.transactions:
            signature_map[entry.meta['signature']].append(entry)

    # Render them to files, for debugging.
    for sig, sigentries in signature_map.items():
        sigentries = data.sorted(sigentries)

        filename = "{}.org".format(sig)
        with open_with_mkdir(path.join(output_signatures, filename)) as catfile:
            fprint = partial(print, file=catfile)
            fprint(";; -*- mode: beancount; coding: utf-8; fill-column: 400 -*-")

            description = KNOWN_SIGNATURES[sig]
            fprint("description: {}".format(description))
            fprint("number_entries: {}".format(len(sigentries)))
            fprint()

            epr = printer.EntryPrinter(dcontext=dcontext,
                                       stringify_invalid_types=True)
            for entry in sigentries:
                fprint(epr(entry))
                fprint()


def write_price_directives(filename: str, pricer: Pricer, days_price_threshold: int):
    """Write a list of required price directives as a Beancount file."""
    price_entries = []
    for (currency, required_date), found_dates in sorted(pricer.required_prices.items()):
        assert len(found_dates) == 1
        cost_currency, actual_date, rate = found_dates.pop()
        days_late = (required_date - actual_date).days
        if days_late < days_price_threshold:
            continue
        price = data.Price({}, required_date, currency, Amount(rate, cost_currency))
        price_entries.append(price)
    with open_with_mkdir(filename) as prfile:
        printer.print_entries(price_entries, file=prfile)


def get_calendar_intervals(date: Date) -> List[Interval]:
    """Return a list of date pairs for sequential intervals."""
    intervals = [
        (str(year), Date(year, 1, 1), Date(year + 1, 1, 1))
        for year in range(TODAY.year - 15, TODAY.year)]
    intervals.append(
        (str(TODAY.year), Date(TODAY.year, 1, 1), date))
    return intervals


def get_cumulative_intervals(date: Date) -> List[Interval]:
    """Return a list of date pairs for sequential intervals."""
    return [
        ("15_years_ago", Date(date.year - 15, 1, 1), date),
        ("10_years_ago", Date(date.year - 10, 1, 1), date),
        ("5_years_ago", Date(date.year - 5, 1, 1), date),
        ("4_years_ago", Date(date.year - 4, 1, 1), date),
        ("3_years_ago", Date(date.year - 3, 1, 1), date),
        ("2_years_ago", Date(date.year - 2, 1, 1), date),
        ("1_year_ago", Date(date.year - 1, 1, 1), date),
        ("ytd", Date(date.year, 1, 1), date),
        ("rolling_6_months_ago", date - relativedelta(months=6), date),
        ("rolling_3_months_ago", date - relativedelta(months=3), date),

        # ("15_years_ago", date - relativedelta(years=15), date),
        # ("10_years_ago", date - relativedelta(years=10), date),
        # ("5_years_ago", date - relativedelta(years=5), date),
        # ("4_years_ago", date - relativedelta(years=4), date),
        # ("3_years_ago", date - relativedelta(years=3), date),
        # ("2_years_ago", date - relativedelta(years=2), date),
        # ("1_year_ago", date - relativedelta(years=1), date),
        # ("6_months_ago", date - relativedelta(months=6), date),
        # ("3_months_ago", date - relativedelta(months=3), date),
    ]


def open_with_mkdir(filename: str):
    """Open in write mode ensuring the underlying directory exists."""
    os.makedirs(path.dirname(filename), exist_ok=True)
    return open(filename, "w")


def read_groups(filename: str,
                account_data: List[AccountData]) -> Dict[str, List[AccountData]]:
    """Read a list of additional account groups to aggregate to."""
    with open(filename) as groupfile:
        obj = json.load(groupfile)
    assert isinstance(obj, dict)
    account_map = {ad.account: ad for ad in account_data}

    groups = {}
    for group_name, account_list in obj.items():
        adlist = groups[group_name] = []
        for an in account_list:
            # Support globbing patterns in account names.
            if "*" in an:
                for ann in fnmatch.filter(account_map, an):
                    adlist.append(account_map[ann])
            else:
                adlist.append(account_map[an])

    return groups


def main():
    """Top-level function."""
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('ledger',
                        help="Beancount ledger file")
    parser.add_argument('output',
                        help="Output directory to write all output files to.")
    parser.add_argument('accounts', nargs='*',
                        help=("Name of specific accounts to analyze. "
                              "Default is all accounts."))

    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose mode')

    parser.add_argument('-c', '--target-currency', action='store', default='USD',
                        help="The target currency to convert flows to.")

    parser.add_argument('-d', '--days-price-threshold', action='store', type=int,
                        default=5,
                        help="The number of days to tolerate price latency.")

    parser.add_argument('-s', '--start-date', action='store',
                        type=datetime.date.fromisoformat,
                        default=Date(TODAY.year - 10, TODAY.month, TODAY.day),
                        help=("Accounts already closed before this date will not be "
                              "included in reporting."))

    parser.add_argument('--pdf', '--pdfs', action='store_true',
                        help="Render as PDFs. Default is HTML directories.")

    parser.add_argument('-A', '--render-accounts', action='append', default=[],
                        help=("A list of accounts whose returns to render explicitly."))
    parser.add_argument('-G', '--groups', '--additional-groups', action='store',
                        help=("A JSON filename containing a list of additional account "
                              "groups to compute returns for."))
    parser.add_argument('-O', '--render-open', action='store_true',
                        help="Render open accounts.")
    parser.add_argument('-C', '--render-closed', action='store_true',
                        help="Render closed accounts.")
    parser.add_argument('-T', '--render-total', action='store_true',
                        help="Render total return.")

    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')
        logging.getLogger('matplotlib.font_manager').disabled = True

    output_accounts = path.join(args.output, "account")
    output_groups = path.join(args.output, "groups")

    # Load the example file.
    logging.info("Reading ledger: %s", args.ledger)
    entries, _, options_map = loader.load_file(args.ledger)
    dcontext = options_map['dcontext']
    pricer = Pricer(prices.build_price_map(entries))

    # Find out the list of accounts to be included.
    account_list = args.accounts or find_accounts(entries, options_map, args.start_date)

    # Prune the list of entries for performance.
    pruned_entries = prune_entries(entries)

    # Process all the accounts.
    account_data = [process_account_entries(pruned_entries, options_map, account)
                    for account in account_list]
    account_data_map = {ad.account: ad for ad in account_data}

    # Write out a details file for each account for debugging.
    for accdata in account_data:
        basename = path.join(output_accounts, accdata.account.replace(":", "_"))
        write_account_file(dcontext, accdata, basename + ".org")

    # Output transactions for each type (for debugging).
    output_signatures = path.join(args.output, "signature")
    write_transactions_by_type(output_signatures, account_data, dcontext)

    # List of groups of AccountData to render.
    groups = collections.defaultdict(list)
    groups_currency = {}

    # Compute returns for the full set of selected accounts.
    if args.render_total:
        groups["ALL"] = account_data
        groups_currency["ALL"] = args.target_currency

    # Process open and closed individual accounts to produce.
    groups.update(get_account_groups(entries, account_data,
                                     args.render_open, args.render_closed))

    # Group assets by currency or by explicit grouping.
    if args.groups:
        groups.update(read_groups(args.groups, account_data))

    # Render specific accounts explicitly.
    if args.render_accounts:
        for account in args.render_accounts:
            key = account.replace(":", "_")
            groups[key].append(account_data_map[account])

    # Write out a returns file for every account.
    multiprocessing.set_start_method('fork')
    os.makedirs(output_groups, exist_ok=True)
    calls = []
    for group_name, adlist in sorted(groups.items()):
        assert isinstance(adlist, list)
        assert all(isinstance(ad, AccountData) for ad in adlist)
        if args.pdf:
            filename = path.join(output_groups, "{}.pdf".format(group_name))
            calls.append((write_returns_pdf, (pricer, adlist, group_name, filename,
                                              groups_currency.get(group_name, None))))
        else:
            dirname = path.join(output_groups, group_name)
            calls.append((write_returns_html, (pricer, adlist, group_name, dirname,
                                               groups_currency.get(group_name, None))))
    if False:
        with multiprocessing.Pool(5) as pool:
            asyns = []
            for func, fargs in calls:
                asyns.append(pool.apply_async(func, fargs))
            for asyn in asyns:
                asyn.wait()
                assert asyn.successful()
    else:
        for func, fargs in calls:
            func(*fargs)

    # Output required price directives (to be filled in the source ledger by
    # fetching prices).
    write_price_directives(path.join(args.output, "prices.beancount"),
                           pricer, args.days_price_threshold)


if __name__ == '__main__':
    main()
