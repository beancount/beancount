#!/usr/bin/env python3
"""Calculate my true returns, including dividends and real costs.

Notes:

- The calculation without dividends only accounts for cash dividends, not
  reinvested dividends.

TODO:

- Compare to a benchmark portfolio with the same cash flows.

- Compute the trailing returns (will need to pull historical prices).

- Produce various aggregations (e.g. in 401k account, don't care about source).

"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"


# pylint: disable=wrong-import-order,wrong-import-position

import datetime
from dateutil.relativedelta import relativedelta
from os import path
from pprint import pprint
from typing import Any, Dict, List, Set, Tuple, Optional
import argparse
import collections
import copy
import csv
import datetime
import enum
import functools
import logging
import os
import re
import typing

import numpy
#import numpy_financial as npf
from scipy.optimize import fsolve
ndarray = numpy.ndarray  # pylint: disable=invalid-name

from beancount import loader
from beancount.core import account as accountlib
from beancount.core import account_types as acctypes
from beancount.core import convert
from beancount.core import data
from beancount.core import getters
from beancount.core import prices
from beancount.core.amount import Amount
from beancount.core.inventory import Inventory
from beancount.core.inventory import Position
from beancount.parser import options
from beancount.parser import printer


# Basic type aliases.
Account = str
Currency = str
Date = datetime.date


# Al list of dated cash flows. This is the unit that this program operates in,
# the sanitized time-series that allows us to compute returns.
CashFlow = typing.NamedTuple("CashFlow", [
    ("date", Date),
    ("amount", Amount),
    ("is_dividend", bool),
])


# The date at which we evaluate this.
TODAY = Date.today()


DatedBalance = typing.NamedTuple("DatedBalance", [
    ("date", Date), # Requested date.
    ("actual_date", Date),
    ("value", Any),
])


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
        elif re.search(r":Dividend$", acc):
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
            print("unknown:", acc)
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


def compute_cash_flows(entry: data.Directive) -> List[CashFlow]:
    """Compute cash flows with and without dividends."""
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
            flows.append(CashFlow(entry.date, posting.units, False))
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
            flows.append(CashFlow(entry.date, posting.units, True))
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
            flows.append(CashFlow(entry.date, -convert.get_weight(posting), False))
        elif category == Cat.OTHERASSET:
            pass
        else:
            raise ValueError("Unsupported category: {}".format(category))
    return flows


def net_present_value(irr: float, cash_flows: ndarray, years: ndarray):
    """Net present value; objective function for optimizer."""
    return numpy.sum(cash_flows / (1. + irr) ** years)


def compute_irr(dated_flows: List[CashFlow],
                price_map: prices.PriceMap,
                target_currency: Currency) -> float:
    """Compute the irregularly spaced IRR."""

    # Array of cash flows, converted to USD.
    usd_flows = []
    for flow in dated_flows:
        usd_amount = convert.convert_amount(
            flow.amount, target_currency, price_map, date=flow.date)
        usd_flows.append(float(usd_amount.number))
        #print(date, amount, "->", usd_amount)
    cash_flows = numpy.array(usd_flows)

    # Array of time in years.
    years = []
    for flow in dated_flows:
        years.append((flow.date - TODAY).days / 365)
    years = numpy.array(years)

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
])


def compute_returns(flows: List[CashFlow],
                    price_map: prices.PriceMap,
                    target_currency: Currency) -> Returns:
    """Compute the returns from a list of cash flows."""
    if not flows:
        return Returns("?", TODAY, TODAY, 0, 0, 0, 0)
    flows = sorted(flows)
    irr = compute_irr(flows, price_map, target_currency)

    flows_exdiv = [flow for flow in flows if not flow.is_dividend]
    irr_exdiv = compute_irr(flows_exdiv, price_map, target_currency)

    first_date = flows[0].date
    last_date = flows[-1].date
    years = (last_date - first_date).days / 365
    return Returns("?", first_date, last_date, years, irr, irr_exdiv, (irr - irr_exdiv))


def copy_and_normalize(entry: data.Transaction) -> data.Transaction:
    """Copy entries and make sure all postings have valid metadata."""
    entry = copy.deepcopy(entry)
    postings = []
    for posting in entry.postings:
        if posting.meta is None:
            posting = posting._replace(meta={})
        postings.append(posting)
    return entry._replace(postings=postings)


# pylint: disable=too-many-locals
def process_account_entries(
        entries: data.Entries,
        options_map: data.Options,
        account: Account,
        interval_dates: List[Date],
) -> Tuple[data.Entries, Dict[Account, Cat], List[DatedBalance]]:
    """Process a single account."""

    # Main matcher that will pull in related transactions.
    accounts_regexp = re.sub("[A-Za-z]+:", "(.*):", account, 1) + "(:Dividend)?$"

    # Figure out the total set of accounts seed in those transactions.
    transactions = [entry
                    for entry in data.filter_txns(entries)
                    if any(re.match(accounts_regexp, posting.account)
                           for posting in entry.postings)]
    if not transactions:
        logging.warning("No transactions for %s; skipping.", account)
        return transactions, None, None

    # Categorize the set of accounts encountered in the filtered transactions.
    seen_accounts = {posting.account
                     for entry in transactions
                     for posting in entry.postings}
    atypes = options.get_account_types(options_map)
    catmap = categorize_accounts(account, seen_accounts, atypes)

    # Insert the requested dates in the stream to be processed in order.
    transactions.extend(DatedBalance(date, None, None)
                        for date in interval_dates)
    transactions.sort(key=lambda x: (x.date, 1 if isinstance(x, DatedBalance) else 0))

    # Process each of the transactions, adding derived values as metadata.
    decorated_entries = []
    balance = Inventory()
    balance_date = None
    dated_balances = []
    for entry in transactions:
        # Store dated balances.
        if isinstance(entry, DatedBalance):
            dated_balances.append(entry._replace(actual_date=balance_date,
                                                 value=copy.deepcopy(balance)))
            continue

        # Update the total position in the asset we're interested in.
        for posting in entry.postings:
            category = catmap[posting.account]
            if category is Cat.ASSET:
                balance.add_position(posting)

        # Compute the signature of the transaction.
        entry = copy_and_normalize(entry)
        signature = compute_transaction_signature(catmap, entry)
        entry.meta["signature"] = signature
        entry.meta["description"] = KNOWN_SIGNATURES[signature]

        # Compute the cash flows associated with the transaction.
        flows = compute_cash_flows(entry)
        entry.meta['cash_flows'] = flows

        decorated_entries.append(entry)

    return data.sorted(decorated_entries), catmap, dated_balances


def process_account_cash_flows(
        decorated_entries: data.Entries,
        price_map: prices.PriceMap,
        balance_start: Optional[DatedBalance] = None,
        balance_end: Optional[DatedBalance] = None
) -> Tuple[List[CashFlow], List[CashFlow]]:
    """Produce the full sequence of cash flows and compute the returns."""

    # Gather the list of computed flows.
    flows = []
    for entry in decorated_entries:
        flows.extend(entry.meta['cash_flows'])

    # Filter from the front, if specified.
    if balance_start is not None:
        cost_balance = balance_start.value.reduce(convert.get_value, price_map,
                                                  balance_start.date)
        position = cost_balance.get_only_position()
        new_flows = [flow for flow in flows if flow.date >= balance_start.date]
        if position is not None:
            new_flows.insert(0, CashFlow(balance_start.date, -position.units, False))
        flows = new_flows

    # Filter at the back, if specified.
    if balance_end is not None:
        cost_balance = balance_end.value.reduce(convert.get_value, price_map,
                                                balance_end.date)
        position = cost_balance.get_only_position()
        new_flows = [flow for flow in flows if flow.date < balance_end.date]
        if position is not None:
            new_flows.append(CashFlow(balance_end.date, position.units, False))
        flows = new_flows

    return flows, position


Result = typing.NamedTuple("Result", [
    ('currency', Currency),
    ('account', Account),
    ('irr', Returns),
    ('final_position', Position),
    ('quote_currency', Currency),
    ('flows', List[CashFlow]),
])


def write_details_file(dcontext,
                       result: Result,
                       catmap: Dict[Account, Cat],
                       decorated_entries: data.Entries,
                       filename: str):
    """Write out a file with details, for inspection and debugging."""

    epr = printer.EntryPrinter(dcontext=dcontext, stringify_invalid_types=True)
    with open(filename, "w") as outfile:
        fprint = functools.partial(print, file=outfile)
        fprint(";; -*- mode: beancount; coding: utf-8; fill-column: 400 -*-")

        # Print front summary section.
        fprint("* Summary\n")
        fprint("Account: {}".format(result.account))
        pos = result.final_position
        fprint("Position: {}".format(pos or "N/A"))
        fprint("IRR:                {:8.2%}".format(result.irr.total))
        fprint("IRR (dividends):    {:8.2%}".format(result.irr.div))
        fprint("\n\n")

        # Print out those details.
        fprint("** Category map\n")
        fprint()
        pprint(catmap, stream=outfile)
        fprint("\n\n")

        fprint("** Transactions\n")
        for entry in decorated_entries:
            fprint(epr(entry))
        fprint("\n\n")

        fprint("** Cash flows\n")
        for flow in result.flows:
            fprint(flow)
        fprint("\n\n")


def write_summary_byaccount(filename: str, results: List[Result]):
    """Write out the summary statistics to a file."""
    csv_writer = csv.writer(open(filename, "w"))
    num_fields = 6
    csv_writer.writerow(Returns._fields)
    for result in results:
        csv_writer.writerow(result.irr)


def write_summary_bycommodity(filename: str, price_map: prices.PriceMap,
                              target_currency: Currency, results: List[Result]):
    """Write out the summary statistics to a file."""
    # Group by currency.
    currency_flows = collections.defaultdict(list)
    for result in results:
        if result.final_position:
            currency_flows[result.currency].extend(result.flows)

    csv_writer = csv.writer(open(filename, "w"))
    csv_writer.writerow(Returns._fields)
    for currency in sorted(currency_flows):
        flows = currency_flows[currency]
        irr = compute_returns(flows, price_map, target_currency)
        csv_writer.writerow(irr._replace(groupname=currency))


def write_summary_overall(filename: str, price_map: prices.PriceMap,
                          target_currency: Currency, results: List[Result]) -> Returns:
    """Write out the summary statistics to a file."""
    flows = []
    for result in results:
        flows.extend(result.flows)
    irr = compute_returns(flows, price_map, target_currency)

    csv_writer = csv.writer(open(filename, "w"))
    csv_writer.writerow(Returns._fields)
    csv_writer.writerow(irr._replace(groupname="(all)"))

    return irr


def get_time_intervals(date: Date) -> Tuple[Tuple[str, Date, Date], List[Date]]:
    """Return a list of interesting time intervals we will need market values for."""

    intervals = [
        # ("1_month_ago", date - relativedelta(months=1), date),
        # ("2_months_ago", date - relativedelta(months=2), date),
        # ("3_months_ago", date - relativedelta(months=3), date),
        # ("6_months_ago", date - relativedelta(months=6), date),
        # ("1_year_ago", date - relativedelta(years=1), date),
        # ("2_years_ago", date - relativedelta(years=2), date),
        # ("3_years_ago", date - relativedelta(years=3), date),
        # ("4_years_ago", date - relativedelta(years=4), date),
        # ("5_years_ago", date - relativedelta(years=5), date),
        # ("year_to_date", Date(2020, 1, 1), date),
        ("10_years_ago", date - relativedelta(years=15), date),
        ]

    # Compute the set of unique dates to gather the inventory for.
    interval_dates = set()
    for _, date1, date2 in intervals:
        interval_dates.add(date1)
        interval_dates.add(date2)

    return intervals, interval_dates


def main():
    """Top-level function."""
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('ledger', help="Beancount ledger file")
    parser.add_argument('output', help="Output directory to write all output files to.")
    parser.add_argument('accounts', nargs='*',
                        help=("Name of specific accounts to analyze. "
                              "Default is all accounts."))

    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose mode')
    parser.add_argument('-c', '--target-currency', action='store', default='USD',
                        help="The target currency to convert flows to.")
    parser.add_argument('-s', '--start-date', action='store',
                        type=datetime.date.fromisoformat,
                        default=Date(TODAY.year - 10, TODAY.month, TODAY.day),
                        help=("Accounts already closed before this date will not be "
                              "included in reporting."))

    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')
    os.makedirs(args.output, exist_ok=True)

    # Load the example file.
    logging.info("Reading ledger: %s", args.ledger)
    entries, _, options_map = loader.load_file(args.ledger)
    price_map = prices.build_price_map(entries)

    # A list of time intervals of interest whose value we will need to accumulate.
    intervals, interval_dates = get_time_intervals(TODAY)

    # Figure out accounts to process.
    account_list = args.accounts or find_accounts(entries, options_map, args.start_date)
    signature_map = {sig: [] for sig in KNOWN_SIGNATURES}
    results = []
    for account in account_list:
        logging.info("Processing account: %s", account)

        # Categorize each entry and compute local flows for each entry.
        decorated_entries, catmap, dated_balances = process_account_entries(
            entries, options_map, account, interval_dates)
        if not decorated_entries:
            continue
        balances_map = {b.date: b for b in dated_balances}

        # Update the global signature map, so we can later output by category.
        for entry in decorated_entries:
            if entry.meta["signature"] not in signature_map:
                print(entry.meta)
            signature_map[entry.meta["signature"]].append(entry)

        for _, date1, date2 in intervals:
            # Compute final flows.
            flows, final_position = process_account_cash_flows(
                decorated_entries, price_map,
                balance_start=balances_map[date1],
                balance_end=balances_map[date2])

            # Compute IRR.
            irr = compute_returns(flows, price_map, args.target_currency)
            irr = irr._replace(groupname=account)

            # Build a results row.
            currency = accountlib.leaf(account)
            result = Result(currency, account,
                            irr,
                            final_position.units.number if final_position else "",
                            final_position.units.currency if final_position else "",
                            flows)
            results.append(result)

        # Produce a details output file.
        filename = path.join(args.output, account.replace(":", "_") + ".org")
        write_details_file(options_map['dcontext'],
                           result, catmap, decorated_entries, filename)

    # Output transactions for each type.
    for sig, sigentries in signature_map.items():
        filename = "signature.{}.beancount".format(sig)
        with open(path.join(args.output, filename), "w") as catfile:
            fprint = functools.partial(print, file=catfile)
            description = KNOWN_SIGNATURES[sig]
            fprint("description: {}".format(description))
            fprint("number_entries: {}".format(len(sigentries)))
            fprint()

            epr = printer.EntryPrinter(dcontext=options_map['dcontext'],
                                       stringify_invalid_types=True)
            for entry in sigentries:
                fprint(epr(entry))
                fprint()

    # Output summary files, by account, by currency, overall.
    write_summary_byaccount(path.join(args.output, "byaccount.csv"),
                            results)
    write_summary_bycommodity(path.join(args.output, "bycommodity.csv"),
                              price_map, args.target_currency, results)
    irr = write_summary_overall(path.join(args.output, "overall.csv"),
                                price_map, args.target_currency, results)

    # Compute my overall returns.
    print("Overall IRR:                {:8.2%}".format(irr.total))
    print("Overall IRR (ex-dividends): {:8.2%}".format(irr.exdiv))
    print("Overall IRR (dividends):    {:8.2%}".format(irr.div))


if __name__ == '__main__':
    main()
