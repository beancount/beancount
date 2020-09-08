#!/usr/bin/env python3
"""Calculate my true returns, including dividends and real costs.

Notes:

- The calculation without dividends only accounts for cash dividends, not
  reinvested dividends.

TODO(blais): Compare to a benchmark portfolio with the same cash flows.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

from typing import List, Optional
import argparse
import collections
import datetime
import sys
import logging
import re

from beancount import loader
from beancount.core import account as accountlib
from beancount.core import account_types as acctypes
from beancount.core import data
from beancount.core import getters
from beancount.parser import options

from returns_config_pb2 import Config
from returns_config_pb2 import InvestmentConfig
from returns_config_pb2 import ReportConfig


# Basic type aliases.
Account = str
Currency = str
Date = datetime.date


def find_accounts(entries: data.Entries,
                  options_map: data.Options,
                  start_date: Optional[Date]) -> List[Account]:
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
            (_close is None or (start_date and _close.date > start_date))))


def infer_configuration(entries: data.Entries,
                        options_map: data.Options,
                        start_date: Optional[Date]) -> Config:
    """Infer an input configuration from a ledger's contents."""

    # Find out the list of accounts to be included.
    account_list = find_accounts(entries, options_map, start_date)

    # Figure out the available investments.
    config = Config()
    infer_investments_configuration(entries, account_list, config.investments)

    # Create reasonable reporting groups.
    infer_report_groups(entries, config.investments, config.reports)
    return config


def infer_investments_configuration(entries: data.Entries,
                                    account_list: List[Account],
                                    out_config: InvestmentConfig):
    """Infer a reasonable configuration for input."""

    all_accounts = set(getters.get_account_open_close(entries))

    for account in account_list:
        aconfig = out_config.investment.add()
        aconfig.currency = accountlib.leaf(account)
        aconfig.asset_account = account

        regexp = re.compile(re.sub(r"^[A-Z][^:]+:", "[A-Z][A-Za-z0-9]+:", account) +
                            ":Dividends?")
        for maccount in filter(regexp.match, all_accounts):
            aconfig.dividend_accounts.append(maccount)

        match_accounts = set()
        match_accounts.add(aconfig.asset_account)
        match_accounts.update(aconfig.dividend_accounts)
        match_accounts.update(aconfig.match_accounts)

        # Figure out the total set of accounts seed in those transactions.
        cash_accounts = set()
        for entry in data.filter_txns(entries):
            if any(posting.account in match_accounts for posting in entry.postings):
                for posting in entry.postings:
                    if (posting.account == aconfig.asset_account or
                        posting.account in aconfig.dividend_accounts or
                        posting.account in aconfig.match_accounts):
                        continue
                    if (re.search(r":(Cash|Checking|Receivable|GSURefund)$",
                                  posting.account) or
                        re.search(r"Receivable|Payable", posting.account) or
                        re.match(r"Income:.*:(Match401k)$", posting.account)):
                        cash_accounts.add(posting.account)
        aconfig.cash_accounts.extend(cash_accounts)


def infer_report_groups(entries: data.Entries,
                        investments: InvestmentConfig,
                        out_config: ReportConfig):
    """Logically group accounts for reporting."""
    groups = collections.defaultdict(list)
    open_close_map = getters.get_account_open_close(entries)
    for investment in investments.investment:
        opn, unused_cls = open_close_map[investment.asset_account]
        assert opn, "Missing open directive for '{}'".format(investment.account)
        groups[investment.currency].append(investment.asset_account)
    for currency, group_accounts in sorted(groups.items()):
        report = out_config.report.add()
        report.name = "currency.{}".format(currency)
        report.investment.extend(group_accounts)


def main():
    """Top-level function."""
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('ledger',
                        help="Beancount ledger file.")
    parser.add_argument('config', nargs='?', action='store',
                        help='Output configuration for accounts and reports.')

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Verbose mode.')

    parser.add_argument('-s', '--start-date', action='store',
                        type=datetime.date.fromisoformat,
                        default=None,
                        help=("Accounts already closed before this date will not be "
                              "included in reporting."))

    args = parser.parse_args()
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format='%(levelname)-8s: %(message)s')
        logging.getLogger('matplotlib.font_manager').disabled = True

    # Load the example file.
    logging.info("Reading ledger: %s", args.ledger)
    entries, _, options_map = loader.load_file(args.ledger)

    # Infer configuration proto.
    logging.info("Inferring configuration.")
    config = infer_configuration(entries, options_map, args.start_date)

    logging.info("Done.")
    outfile = open(args.config) if args.config else sys.stdout
    print(config, file=outfile)


if __name__ == '__main__':
    main()
