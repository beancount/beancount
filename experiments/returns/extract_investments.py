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
from typing import Dict, List, Set, Tuple
from functools import partial
import collections
import copy
import datetime
import enum
import logging
import re
import typing

from beancount.core import account as accountlib
from beancount.core import display_context
from beancount.core import convert
from beancount.core import data
from beancount.core import getters
from beancount.core.amount import Amount
from beancount.core.inventory import Inventory
from beancount.parser import printer

from returns_config_pb2 import Config
from returns_config_pb2 import Investment


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


# Al list of dated cash flows. This is the unit that this program operates in,
# the sanitized time-series that allows us to compute returns.
CashFlow = typing.NamedTuple("CashFlow", [
    ("date", Date),
    ("amount", Amount),     # The amount of the cashflow.
    ("is_dividend", bool),  # True if the flow is a dividend.
    ("balance", Inventory), # Balance after this cash flow.
])


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


IGNORE_CATEGORIES = {Cat.ROUNDING, Cat.TRACKING}


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


def copy_and_normalize(entry: data.Transaction) -> data.Transaction:
    """Copy entries and make sure all postings have valid metadata."""
    entry = copy.deepcopy(entry)
    postings = []
    for posting in entry.postings:
        if posting.meta is None:
            posting = posting._replace(meta={})
        postings.append(posting)
    return entry._replace(postings=postings)


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


def write_account_file(dcontext: display_context.DisplayContext,
                       account_data: AccountData,
                       filename: str):
    """Write out a file with details, for inspection and debugging."""

    logging.info("Writing details file: %s", filename)
    epr = printer.EntryPrinter(dcontext=dcontext, stringify_invalid_types=True)
    with open(filename, "w") as outfile:
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
        with open(path.join(output_signatures, filename), "w") as catfile:
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


def extract(entries: data.Entries,
            dcontext: display_context.DisplayContext,
            config: Config,
            end_date: Date,
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
    account_data = [process_account_entries(pruned_entries, aconfig)
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
