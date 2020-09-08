#!/usr/bin/env python3
"""Calculate my true returns, including dividends and real costs.

TODO(blais): Compare to a benchmark portfolio with the same cash flows.
TODO(blais): Estimate after-tax returns, including tax on dividends.
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

from google.protobuf import text_format

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

from returns_config_pb2 import Config
from returns_config_pb2 import InvestmentConfig
from returns_config_pb2 import ReportConfig
from returns_config_pb2 import Investment


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
        """Convert an amount to a specific currency."""
        # TODO(blais): Save the amount here too.
        return convert.convert_amount(amount, target_currency, self.price_map, date=date)


def prune_entries(entries: data.Entries, config: Config) -> data.Entries:
    """Prune the list of entries to exclude all transactions that include a
    commodity name in at least one of its postings. This speeds up the
    recovery process by removing the majority of non-trading transactions."""

    commodities = set(aconfig.currency for aconfig in config.investments.investment)
    accounts = set()
    for aconfig in config.investments.investment:
        accounts.add(aconfig.asset_account)
        accounts.update(aconfig.dividend_accounts)
        accounts.update(aconfig.match_accounts)

    return [entry
            for entry in entries
            if ((isinstance(entry, data.Commodity) and
                 entry.currency in commodities) or
                (isinstance(entry, data.Open) and
                 entry.account in accounts) or
                (isinstance(entry, data.Transaction) and
                 any(posting.account in accounts
                     for posting in entry.postings)))]


def extract_transactions_for_account(entries: data.Entries,
                                     config: Investment) -> data.Entries:
    """Get the list of transactions affecting an investment account."""
    match_accounts = set([config.asset_account])
    match_accounts.update(config.dividend_accounts)
    match_accounts.update(config.match_accounts)
    return [entry
            for entry in data.filter_txns(entries)
            if any(posting.account in match_accounts
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

    # Dividend income account.
    DIVIDEND = 5

    # Any other account (more specific ones come below).
    OTHER = 13

    # Income accounts.
    PNL = 3
    INTEREST = 4

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


def categorize_accounts_general(config: Investment,
                                accounts: Set[Account]) -> Dict[Account, Cat]:
    """Categorize the type of accounts for a particular stock. Our purpose is to
    make the types of postings generic, so they can be categorized and handled
    generically later on.
    """
    catmap = {}
    for account in accounts:
        if account == config.asset_account:
            cat = Cat.ASSET
        elif account in config.dividend_accounts:
            cat = Cat.DIVIDEND
        elif account in config.cash_accounts:
            cat = Cat.CASH
        else:
            # Potentially includes other assets.
            cat = Cat.OTHER
        catmap[account] = cat
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


def produce_cash_flows_general(entry: data.Directive,
                               catmap: Dict[Account, Cat]) -> List[CashFlow]:
    """Produce cash flows using the signature of the transaction."""

    for posting in entry.postings:
        posting.meta["category"] = catmap[posting.account]

    has_dividend = any(posting.meta["category"] == Cat.DIVIDEND
                       for posting in entry.postings)

    flows = []
    for posting in entry.postings:
        category = posting.meta["category"]
        if category == Cat.CASH:
            assert not posting.cost
            cf = CashFlow(entry.date, posting.units, has_dividend, None)
            posting.meta["flow"] = cf
            flows.append(cf)
        elif category == Cat.OTHER:
            # If the account deposits other assets (as evidenced by a cost
            # basis), count those as outflows.
            if posting.cost:
                cf = CashFlow(entry.date, convert.get_weight(posting), False, None)
                posting.meta["flow"] = cf
                flows.append(cf)
    return flows


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
            cf = CashFlow(entry.date, posting.units, False, None)
            posting.meta["flow"] = cf
            flows.append(cf)
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
            cf = CashFlow(entry.date, posting.units, True, None)
            posting.meta["flow"] = cf
            flows.append(cf)
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
            pass
        elif category == Cat.OTHERASSET:
            cf = CashFlow(entry.date, convert.get_weight(posting), False, None)
            posting.meta["flow"] = cf
            flows.append(cf)
        else:
            raise ValueError("Unsupported category: {}".format(category))
    return flows


def net_present_value(irr: float, cash_flows: Array, years: Array):
    """Net present value; objective function for optimizer."""
    return np.sum(cash_flows / (1. + irr) ** years)


def compute_irr(dated_flows: List[CashFlow],
                pricer: Pricer,
                target_currency: Currency,
                end_date: Date) -> float:
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
        years.append((flow.date - end_date).days / 365)
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
                    target_currency: Currency,
                    end_date: Date) -> Returns:
    """Compute the returns from a list of cash flows."""
    if not flows:
        return Returns("?", TODAY, TODAY, 0, 0, 0, 0, [])
    flows = sorted(flows, key=lambda cf: cf.date)
    irr = compute_irr(flows, pricer, target_currency, end_date)

    flows_exdiv = [flow for flow in flows if not flow.is_dividend]
    irr_exdiv = compute_irr(flows_exdiv, pricer, target_currency, end_date)

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
                            config: Investment) -> AccountData:
    """Process a single account."""
    account = config.asset_account
    logging.info("Processing account: %s", account)

    # Extract the relevant transactions.
    transactions = extract_transactions_for_account(entries, config)
    if not transactions:
        logging.warning("No transactions for %s; skipping.", account)
        return None

    # Categorize the set of accounts encountered in the filtered transactions.
    seen_accounts = {posting.account
                     for entry in transactions
                     for posting in entry.postings}
    catmap = categorize_accounts_general(config, seen_accounts)

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
        ##entry.meta["description"] = KNOWN_SIGNATURES[signature]

        # Compute the cash flows associated with the transaction.
        flows = produce_cash_flows_general(entry, catmap)

        cash_flows.extend(flow._replace(balance=copy.deepcopy(balance))
                          for flow in flows)
        decorated_transactions.append(entry)

    cost_currencies = set(cf.amount.currency for cf in cash_flows)
    #assert len(cost_currencies) == 1, str(cost_currencies)
    cost_currency = cost_currencies.pop() if cost_currencies else None

    currency = config.currency
    commodity_map = getters.get_commodity_directives(entries)
    comm = commodity_map[currency] if currency else None

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
        date_start: Date,
        date_end: Date) -> List[CashFlow]:
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

        if account_data.cash_flows:
            final_cf = account_data.cash_flows[-1]
            units_balance = final_cf.balance.reduce(convert.get_units)
        else:
            units_balance = Inventory()
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
    .new-page { page-break-before: always; }
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
        returns = compute_returns(cash_flows, pricer, target_currency, date2)
        rows[0].append(returns.total)
        rows[1].append(returns.exdiv)
        rows[2].append(returns.div)
    return Table(header, rows)


def write_returns_pdf(pdf_filename: str, *args, **kwargs) -> subprocess.Popen:
    """Write out returns for a combined list of account account_data.."""
    logging.info("Writing returns file: %s", pdf_filename)

    with tempfile.TemporaryDirectory() as tmpdir:
        indexfile = write_returns_html(tmpdir, *args, **kwargs)
        command = ["google-chrome", "--headless", "--disable-gpu",
                   "--print-to-pdf={}".format(pdf_filename), indexfile]
        subprocess.check_call(command, stderr=open("/dev/null", "w"))
        assert path.exists(pdf_filename)
        logging.info("Done: file://%s", pdf_filename)


def write_returns_html(dirname: str,
                       pricer: Pricer,
                       account_data: List[AccountData],
                       title: str,
                       end_date: Date,
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

        # TOOD(blais): Prices should be plot separately, by currency.
        # fprint("<h2>Prices</h2>")
        # pairs = set((r.currency, r.cost_currency) for r in account_data)
        # plots = plot_prices(dirname, pricer.price_map, pairs)
        # for _, filename in sorted(plots.items()):
        #     fprint('<img src={} style="width: 100%"/>'.format(filename))

        fprint("<h2>Cash Flows</h2>")
        cash_flows_list = [ad.cash_flows for ad in account_data]
        cash_flows = truncate_and_merge_cash_flows(pricer, cash_flows_list,
                                                   None, end_date)

        transactions = data.sorted([txn for ad in account_data for txn in ad.transactions])

        returns = compute_returns(cash_flows, pricer, target_currency, end_date)
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

        fprint('<h2 class="new-page">Accounts</h2>')
        fprint("<p>Cost Currency: {}</p>".format(target_currency))
        for ad in account_data:
            fprint("<p>Account: {} ({})</p>".format(
                ad.account,
                ad.commodity.meta["name"] if ad.commodity else "N/A"))

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

    if date_min and date_max:
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

            set_axis(ax, dates[0] if dates else None, dates[-1] if dates else None)
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
        set_axis(ax, dates[0] if dates else None, dates[-1] if dates else None)
        ax.axhline(0, color='#000', linewidth=0.2)
        ax.vlines(dates_exdiv, 0, amounts_exdiv, linewidth=3, color='#000', alpha=0.7)
        ax.vlines(dates_div, 0, amounts_div, linewidth=3, color='#0A0', alpha=0.7)
    axs[1].set_yscale('symlog')

    axs[0].set_title("Cash Flows")
    axs[1].set_title("log(Cash Flows)")
    fig.autofmt_xdate()
    fig.tight_layout()
    filename = outplots["flows"] = path.join(output_dir, "flows.svg")
    plt.savefig(filename)
    plt.close(fig)

    # Render cumulative cash flows, with returns growth.
    lw = 0.8
    if dates:
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
        set_axis(ax, dates[0] if dates else None, dates[-1] if dates else None)
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

            description = KNOWN_SIGNATURES.get(sig, "?")
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


def is_glob(pattern):
    return re.compile(r"[*?]").search(pattern)


def _expand_globs(patterns: List[str], valid_set: List[str]) -> List[str]:
    out_values = []
    for pattern in patterns:
        if is_glob(pattern):
            out_values.extend(fnmatch.filter(valid_set, pattern))
        else:
            out_values.append(pattern)
    return out_values


def read_config(config_filename: str,
                filter_reports: List[str],
                accounts: List[Account]) -> Config:
    """Read the configuration and perform globbing expansions."""

    # Read the file.
    config = Config()
    with open(config_filename, "r") as infile:
        text_format.Merge(infile.read(), config)
    reports = list(config.reports.report)

    # Expand account names.
    for investment in config.investments.investment:
        assert not is_glob(investment.asset_account)
        investment.dividend_accounts[:] = _expand_globs(investment.dividend_accounts,
                                                        accounts)
        investment.match_accounts[:] = _expand_globs(investment.match_accounts, accounts)
        investment.cash_accounts[:] = _expand_globs(investment.cash_accounts, accounts)

    # Expand investment names.
    investment_names = [investment.asset_account
                        for investment in config.investments.investment]
    for report in config.reports.report:
        report.investment[:] = _expand_globs(report.investment, investment_names)

    # Filter down reports.
    if filter_reports:
        filter_set = set(filter_reports)
        reports = [report
                   for report in config.reports.report
                   if report.name in filter_set]
        del config.reports.report[:]
        config.reports.report.extend(reports)

    return config



def main():
    """Top-level function."""
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('ledger',
                        help="Beancount ledger file")
    parser.add_argument('config', action='store',
                        help='Configuration for accounts and reports.')
    parser.add_argument('output',
                        help="Output directory to write all output files to.")

    parser.add_argument('filter_reports', nargs='*',
                        help="Optional names of specific subset of reports to analyze.")

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Verbose mode')

    parser.add_argument('-d', '--days-price-threshold', action='store', type=int,
                        default=5,
                        help="The number of days to tolerate price latency.")

    parser.add_argument('-e', '--end-date', action='store',
                        type=datetime.date.fromisoformat,
                        help="The end date to compute returns up to.")

    parser.add_argument('--pdf', '--pdfs', action='store_true',
                        help="Render as PDFs. Default is HTML directories.")

    parser.add_argument('-j', '--parallel', action='store_true',
                        help="Run report generation concurrently.")

    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')
        logging.getLogger('matplotlib.font_manager').disabled = True

    output_accounts = path.join(args.output, "investments")
    output_reports = path.join(args.output, "reports")

    # Load the example file.
    logging.info("Reading ledger: %s", args.ledger)
    entries, _, options_map = loader.load_file(args.ledger)
    accounts = getters.get_accounts(entries)
    dcontext = options_map['dcontext']
    pricer = Pricer(prices.build_price_map(entries))

    # Load, filter and expand the configuration.
    config = read_config(args.config, args.filter_reports, accounts)
    with open(path.join(args.output, "config_expanded.pbtxt"), "w") as efile:
        print(config, file=efile)

    # Note: It might be useful to have an option for "the end of its history"
    # for Ledger that aren't updated up to today.
    end_date = args.end_date or TODAY

    # Remove all data after end date, if specified.
    if end_date < entries[-1].date:
        entries = [entry
                   for entry in entries
                   if entry.date < end_date]

    # Prune the list of entries for performance.
    pruned_entries = prune_entries(entries, config)

    # Filter just the list of instruments needed for the requested reports.
    used_accounts = set(inv
                        for report in config.reports.report
                        for inv in report.investment)
    investment_list = [invest
                       for invest in config.investments.investment
                       if invest.asset_account in used_accounts]

    # Process all the accounts.
    account_data = [process_account_entries(pruned_entries, options_map, aconfig)
                    for aconfig in investment_list]
    account_data = list(filter(None, account_data))
    account_data_map = {ad.account: ad for ad in account_data}

    # Write out a details file for each account for debugging.
    for ad in account_data:
        basename = path.join(output_accounts, ad.account.replace(":", "_"))
        write_account_file(dcontext, ad, basename + ".org")

    # Output transactions for each type (for debugging).
    output_signatures = path.join(args.output, "signature")
    write_transactions_by_type(output_signatures, account_data, dcontext)

    # Output required price directives (to be filled in the source ledger by
    # fetching prices).
    write_price_directives(path.join(args.output, "prices.beancount"),
                           pricer, args.days_price_threshold)

    # Write out a returns file for every account.
    multiprocessing.set_start_method('fork')
    os.makedirs(output_reports, exist_ok=True)
    calls = []
    for report in config.reports.report:
        adlist = [account_data_map[name] for name in report.investment]
        assert isinstance(adlist, list)
        assert all(isinstance(ad, AccountData) for ad in adlist)

        function = write_returns_pdf if args.pdf else write_returns_html
        filename = path.join(output_reports, report.name)
        if args.pdf: filename = filename + ".pdf"
        calls.append(partial(
            function, filename, pricer, adlist, report.name,
            end_date,
            report.currency))

    if args.parallel:
        with multiprocessing.Pool(5) as pool:
            asyns = []
            for func in calls:
                asyns.append(pool.apply_async(func))
            for asyn in asyns:
                asyn.wait()
                assert asyn.successful()
    else:
        for func in calls:
            func()


if __name__ == '__main__':
    main()
