"""Library code to extract time-series data for each investment.

This code produces a data object for each investment from a Beancount ledger,
containing the list of transactions for its asset account, a list of cash flows
to compute the returns, and a bit more. Investments are defined as "one
commodity in one account" and can be further combined as reports in code outside
this library.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
from pprint import pprint
from typing import Dict, List, Optional, Set, Tuple
from functools import partial
import collections
import copy
import datetime
import enum
import logging
import os
import re
import sys
import typing

import pandas

from beancount.core import account as accountlib
from beancount.core import display_context
from beancount.core import convert
from beancount.core import data
from beancount.core import getters
from beancount.core.amount import Amount
from beancount.core.inventory import Inventory
from beancount.parser import printer

from config_pb2 import Config
from config_pb2 import Investment
from config_pb2 import InvestmentConfig


# Basic type aliases.
Account = str
Currency = str
Date = datetime.date


class Cat(enum.Enum):
    """Posting categorization.

    This is used to produce unique templates to categorize each transaction. A
    template is a set of the categories below, which each of a transaction's
    postings are classified with.
    """

    # The account holding the commodity.
    ASSET = 1

    # Cash accounts, employer matches, contributions, i.e., anything external to
    # the investment.
    CASH = 2

    # Dividend income account.
    DIVIDEND = 3

    # Commissions, fees and other expenses.
    EXPENSES = 4

    # Non-dividend income, P/L, gains, or other.
    INCOME = 5

    # Other assets than the primary asset for this investment.
    OTHERASSET = 7

    # Any other account.
    OTHER = 6


# Al list of dated cash flows. This is the unit that this program operates in,
# the sanitized time-series that allows us to compute returns.
CashFlow = typing.NamedTuple("CashFlow", [
    ("date", Date),
    ("amount", Amount),     # The amount of the cashflow.
    ("is_dividend", bool),  # True if the flow is a dividend.
    ("source", str),        # Source of this cash flow.
    ("account", Account),   # Asset account for which this was generated.
])


# All flow information associated with an account.
AccountData = typing.NamedTuple("AccountData", [
    ('account', Account),
    ('currency', Currency),
    ('cost_currency', Currency),
    ('commodity', data.Commodity),
    ('open', data.Open),
    ('close', data.Close),
    ("cash_flows", List[CashFlow]),
    ('transactions', data.Entries),
    ('balance', Inventory),
    ('catmap', Dict[Account, Cat]),
])


def categorize_accounts(config: InvestmentConfig,
                        investment: Investment,
                        accounts: Set[Account]) -> Dict[Account, Cat]:
    """Categorize the type of accounts encountered for a particular investment's
    transactions. Our purpose is to make the types of postings generic, so they
    can be categorized and handled generically later on.
    """
    catmap = {}
    for account in accounts:
        if account == investment.asset_account:
            cat = Cat.ASSET
        elif account in investment.dividend_accounts:
            cat = Cat.DIVIDEND
        elif account in investment.cash_accounts:
            cat = Cat.CASH
        elif re.match(config.income_regexp or "Income:", account):
            cat = Cat.INCOME
        elif re.match(config.expenses_regexp or "Expenses:", account):
            cat = Cat.EXPENSES
        else:
            # Note: When applied, if the corresponding postings has a cost,
            # OTHERASSET will be assigned; otherwise OTHER will be assigned.
            cat = None
        catmap[account] = cat
    return catmap


def categorize_entry(catmap: Dict[Account, Cat],
                     entry: data.Directive) -> Tuple[Cat]:
    """Assigns metadata to each posting."""
    postings = []
    for posting in entry.postings:
        category = catmap[posting.account]
        if category is None:
            category = Cat.OTHER if posting.cost is None else Cat.OTHERASSET
        meta = posting.meta.copy() if posting.meta else {}
        meta["category"] = category
        postings.append(posting._replace(meta=meta))
    return entry._replace(postings=postings)


def compute_transaction_signature(entry: data.Directive) -> Tuple[Cat]:
    """Compute a unique signature for each transaction."""
    categories = set(posting.meta["category"] for posting in entry.postings)
    sigtuple = tuple(sorted(categories, key=lambda item: item.value))
    return "_".join(s.name for s in sigtuple)


_signature_registry = {}
def register(categories, description):
    """Registers a handler for a particular template/signature transaction."""
    def decorator(func):
        key = "_".join(c.name for c in sorted(categories, key=lambda c: c.value))
        _signature_registry[key] = (func, description)
        return func
    return decorator


def get_description(signature):
    _, description = _signature_registry.get(signature, (None, None))
    return description


def produce_cash_flows_general(entry: data.Directive,
                               account: Account) -> List[CashFlow]:
    """Produce cash flows using a generalized rule."""
    has_dividend = any(posting.meta["category"] == Cat.DIVIDEND
                       for posting in entry.postings)
    flows = []
    for posting in entry.postings:
        category = posting.meta["category"]
        if category == Cat.CASH:
            assert not posting.cost
            cf = CashFlow(entry.date, convert.get_weight(posting), has_dividend,
                          "cash", account)
            posting.meta["flow"] = cf
            flows.append(cf)

        elif category == Cat.OTHERASSET:
            # If the account deposits other assets, count this as an outflow.
            cf = CashFlow(entry.date, convert.get_weight(posting), False,
                          "other", account)
            posting.meta["flow"] = cf
            flows.append(cf)

    return flows


def produce_cash_flows_explicit(entry: data.Directive,
                                account: Account) -> List[CashFlow]:
    """Produce cash flows using explicit handlers from signatures."""
    sig = entry.meta["signature"]
    try:
        handler, _ = _signature_registry[sig]
    except KeyError:
        epr = printer.EntryPrinter(stringify_invalid_types=True)
        print(epr(entry), file=sys.stderr)
        raise
    return handler(entry, account)


@register([Cat.ASSET],
          "Stock splits, and conversions at same cost basis")
def handle_no_flows(*args) -> List[CashFlow]:
    "Exchanges of the same asset produce no cash flows."
    return []


@register([Cat.ASSET, Cat.DIVIDEND],
          "Asset dividend reinvested")
@register([Cat.ASSET, Cat.DIVIDEND, Cat.EXPENSES],
          "Asset dividend reinvested")
def handle_dividend_reinvestments(*args) -> List[CashFlow]:
    """Reinvested stock dividends remains internal, the money is just moved to more
    of the asset. Note that because of this, it would make it difficult to
    remove the dividend from the performance of this asset."""
    return []


@register([Cat.ASSET, Cat.EXPENSES],
          "Fee paid from liquidation")
def handle_fee_from_liquidation(*args) -> List[CashFlow]:
    """Fees paid purely from sales of assets. No in or out flows, the stock value is
    simply reduced."""
    return []


@register([Cat.ASSET, Cat.INCOME],
          "Cost basis adjustment")
@register([Cat.ASSET, Cat.INCOME, Cat.OTHER],
          "Cost basis adjustment")
@register([Cat.ASSET, Cat.INCOME, Cat.EXPENSES],
          "Fee from liquidation (with P/L)")
def handle_cost_basis_adjustments(*args) -> List[CashFlow]:
    """No cash is disbursed for these adjustments, just a change in basis. This
    affects tax only. There are no associated cash flows."""
    return []

@register([Cat.EXPENSES, Cat.OTHER],
          "Internal expense")
@register([Cat.OTHER],
          "Movement between internal accounts")
def handle_cost_basis_adjustments(*args) -> List[CashFlow]:
    """It's internal changes, no flows."""
    return []


@register([Cat.ASSET, Cat.CASH],
          "Regular purchase or sale")
@register([Cat.ASSET, Cat.CASH, Cat.OTHER],
          "Regular purchase or sale")
@register([Cat.ASSET, Cat.CASH, Cat.INCOME],
          "Regular purchase or sale, with P/L")
@register([Cat.ASSET, Cat.CASH, Cat.INCOME, Cat.OTHER],
          "Regular purchase or sale, with P/L")
@register([Cat.ASSET, Cat.CASH, Cat.EXPENSES, Cat.INCOME],
          "Regular purchase or sale, with expense and P/L")
@register([Cat.ASSET, Cat.CASH, Cat.EXPENSES, Cat.OTHER],
          "Regular purchase or sale")
@register([Cat.ASSET, Cat.CASH, Cat.EXPENSES, Cat.INCOME, Cat.OTHER],
          "Regular purchase or sale")
@register([Cat.ASSET, Cat.CASH, Cat.EXPENSES], "Regular purchase or sale, with expense")
def handle_buy_sell(entry: data.Directive, account: Account) -> List[CashFlow]:
    "In a regular purchase or sale, use the cash component for sales and purchases."
    return _handle_cash(entry, account, False)


@register([Cat.CASH, Cat.DIVIDEND],
          "Cash dividend")
@register([Cat.CASH, Cat.DIVIDEND, Cat.INCOME],
          "Cash dividend")
@register([Cat.CASH, Cat.EXPENSES, Cat.DIVIDEND],
          "Cash dividend, with expenses")
def handle_dividends(entry: data.Directive, account: Account) -> List[CashFlow]:
    "Dividends received in cash."
    return _handle_cash(entry, account, True)


@register([Cat.CASH, Cat.EXPENSES],
          "Cash for expense")
@register([Cat.CASH, Cat.OTHER],
          "Cash for other internal account")
@register([Cat.CASH, Cat.EXPENSES, Cat.OTHER],
          "Cash for expense or something else")
@register([Cat.CASH, Cat.EXPENSES, Cat.INCOME],
          "Recoveries from P2P lending")
def handle_cash_simple(entry: data.Directive, account: Account) -> List[CashFlow]:
    "Cash for income."
    return _handle_cash(entry, account, False)


def _handle_cash(entry: data.Directive, account: Account,
                 is_dividend: bool) -> List[CashFlow]:
    "In a regular purchase or sale, use the cash component for sales and purchases."
    flows = []
    for posting in entry.postings:
        if posting.meta["category"] == Cat.CASH:
            assert not posting.cost
            cf = CashFlow(entry.date, posting.units, is_dividend, "cash", account)
            posting.meta["flow"] = cf
            flows.append(cf)
    return flows


@register([Cat.ASSET, Cat.OTHERASSET],
          "Exchange of assets")
def handle_stock_exchange(entry: data.Directive, account: Account) -> List[CashFlow]:
    """This is for a stock exchange, similar to the issuance of GOOG from GOOGL."""
    flows = []
    for posting in entry.postings:
        if posting.meta["category"] == Cat.OTHERASSET:
            cf = CashFlow(entry.date, convert.get_weight(posting), False,
                          "other", account)
            posting.meta["flow"] = cf
            flows.append(cf)
    return flows


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


def process_account_entries(entries: data.Entries,
                            config: InvestmentConfig,
                            investment: Investment,
                            check_explicit_flows: bool) -> AccountData:
    """Process a single account."""
    account = investment.asset_account
    logging.info("Processing account: %s", account)

    # Extract the relevant transactions.
    transactions = extract_transactions_for_account(entries, investment)
    if not transactions:
        logging.warning("No transactions for %s; skipping.", account)
        return None

    # Categorize the set of accounts encountered in the filtered transactions.
    seen_accounts = {posting.account
                     for entry in transactions
                     for posting in entry.postings}
    catmap = categorize_accounts(config, investment, seen_accounts)

    # Process each of the transactions, adding derived values as metadata.
    cash_flows = []
    balance = Inventory()
    decorated_transactions = []
    for entry in transactions:

        # Compute the signature of the transaction.
        entry = categorize_entry(catmap, entry)
        signature = compute_transaction_signature(entry)
        entry.meta["signature"] = signature

        # TODO(blais): Cache balance in every transaction to speed up
        # computation? Do this later.
        if False:
            # Update the total position in the asset we're interested in.
            for posting in entry.postings:
                if posting.meta["category"] is Cat.ASSET:
                    balance.add_position(posting)

        # Compute the cash flows associated with the transaction.
        flows_general = produce_cash_flows_general(entry, account)
        if check_explicit_flows:
            # Attempt the explicit method.
            flows_explicit = produce_cash_flows_explicit(entry, account)
            if flows_explicit != flows_general:
                print("Differences found between general and explicit methods:")
                print("Explicit handlers:")
                for flow in flows_explicit:
                    print("  ", flow)
                print("General handler:")
                for flow in flows_general:
                    print("  ", flow)
                raise ValueError("Differences found between general and explicit methods:")

        cash_flows.extend(flows_general)
        decorated_transactions.append(entry)

    cost_currencies = set(cf.amount.currency for cf in cash_flows)
    #assert len(cost_currencies) == 1, str(cost_currencies)
    cost_currency = cost_currencies.pop() if cost_currencies else None

    currency = investment.currency
    commodity_map = getters.get_commodity_directives(entries)
    comm = commodity_map[currency] if currency else None

    open_close_map = getters.get_account_open_close(entries)
    opn, cls = open_close_map[account]

    # Compute the final balance.
    balance = compute_balance_at(decorated_transactions)

    return AccountData(account, currency, cost_currency, comm, opn, cls,
                       cash_flows, decorated_transactions, balance, catmap)


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


def compute_balance_at(transactions: data.Entries,
                       date: Optional[Date] = None) -> Inventory:
    """Compute the balance at a specific date."""
    balance = Inventory()
    for entry in transactions:
        if date is not None and entry.date >= date:
            break
        for posting in entry.postings:
            if posting.meta["category"] is Cat.ASSET:
                balance.add_position(posting)
    return balance


def write_account_file(dcontext: display_context.DisplayContext,
                       account_data: AccountData,
                       filename: str):
    """Write out a file with details, for inspection and debugging."""

    logging.info("Writing details file: %s", filename)
    epr = printer.EntryPrinter(dcontext=dcontext, stringify_invalid_types=True)
    os.makedirs(path.dirname(filename), exist_ok=True)
    with open(filename, "w") as outfile:
        fprint = partial(print, file=outfile)
        fprint(";; -*- mode: beancount; coding: utf-8; fill-column: 400 -*-")

        # Print front summary section.
        fprint("* Summary\n")
        fprint("Account: {}".format(account_data.account))

        # Print the final balance of the account.
        units_balance = account_data.balance.reduce(convert.get_units)
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

        # Flatten cash flows to a table.
        fprint("** Cash flows\n")
        df = cash_flows_to_table(account_data.cash_flows)
        fprint(df.to_string())
        fprint("\n\n")


def cash_flows_to_table(cash_flows: List[CashFlow]) -> pandas.DataFrame:
    """Flatten a list of cash flows to an HTML table string."""
    import reports
    header = ["date", "amount", "currency", "is_dividend", "source", "investment"]
    rows = []
    for flow in cash_flows:
        rows.append((flow.date,
                     float(flow.amount.number),
                     flow.amount.currency,
                     flow.is_dividend,
                     flow.source,
                     flow.account))
    return pandas.DataFrame(columns=header, data=rows)


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
    os.makedirs(output_signatures, exist_ok=True)
    for sig, sigentries in signature_map.items():
        sigentries = data.sorted(sigentries)

        filename = "{}.org".format(sig)
        with open(path.join(output_signatures, filename), "w") as catfile:
            fprint = partial(print, file=catfile)
            fprint(";; -*- mode: beancount; coding: utf-8; fill-column: 400 -*-")

            description = get_description(sig) or "?"
            fprint("description: {}".format(description))
            fprint("number_entries: {}".format(len(sigentries)))
            fprint()

            epr = printer.EntryPrinter(dcontext=dcontext,
                                       stringify_invalid_types=True)
            for entry in sigentries:
                fprint(epr(entry))
                fprint()


def extract(entries: data.Entries,
            dcontext: display_context.DisplayContext,
            config: Config,
            end_date: Date,
            check_explicit_flows: bool,
            output_dir: str) -> Dict[Account, AccountData]:
    """Extract data from the list of entries."""
    # Note: It might be useful to have an option for "the end of its history"
    # for Ledger that aren't updated up to today.

    # Remove all data after end date, if specified.
    if end_date < entries[-1].date:
        entries = [entry
                   for entry in entries
                   if entry.date < end_date]

    # Prune the list of entries for performance.
    pruned_entries = prune_entries(entries, config)

    # Process all the accounts.
    account_data = [process_account_entries(pruned_entries, config.investments, aconfig,
                                            check_explicit_flows)
                    for aconfig in config.investments.investment]
    account_data = list(filter(None, account_data))
    account_data_map = {ad.account: ad for ad in account_data}

    if output_dir:
        # Write out a details file for each account for debugging.
        for ad in account_data_map.values():
            basename = path.join(output_dir, ad.account.replace(":", "_"))
            write_account_file(dcontext, ad, basename + ".org")

        # Output transactions for each type (for debugging).
        write_transactions_by_type(path.join(output_dir, "signature"),
                                   account_data, dcontext)

    return account_data_map
