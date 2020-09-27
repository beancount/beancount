#!/usr/bin/env python3
"""Library coded to process the input configuration.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

from typing import List
import fnmatch
import re

from google.protobuf import text_format

from config_pb2 import Config


# Basic type aliases.
Account = str


def is_glob(pattern):
    return re.compile(r"[*?]").search(pattern)


def expand_globs(patterns: List[str], valid_set: List[str]) -> List[str]:
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
    """Read the configuration, perform globbing expansions, and whittle down the
    list of reports and investments to the requested minimal."""

    # Read the file.
    config = Config()
    with open(config_filename, "r") as infile:
        text_format.Merge(infile.read(), config)
    reports = list(config.groups.group)

    # Expand account names.
    for investment in config.investments.investment:
        assert not is_glob(investment.asset_account)
        investment.dividend_accounts[:] = expand_globs(investment.dividend_accounts,
                                                        accounts)
        investment.match_accounts[:] = expand_globs(investment.match_accounts, accounts)
        investment.cash_accounts[:] = expand_globs(investment.cash_accounts, accounts)

    # Expand investment names.
    investment_names = [investment.asset_account
                        for investment in config.investments.investment]
    for report in config.groups.group:
        report.investment[:] = expand_globs(report.investment, investment_names)

    # Filter down reports.
    if filter_reports:
        reports = [report
                   for report in config.groups.group
                   if any(fnmatch.fnmatch(report.name, pattern)
                          for pattern in filter_reports)]
        del config.groups.group[:]
        config.groups.group.extend(reports)

    # Filter just the list of investments needed for the reports defined.
    used_investments = set(inv
                           for report in config.groups.group
                           for inv in report.investment)
    investments = [invest
                   for invest in config.investments.investment
                   if invest.asset_account in used_investments]
    del config.investments.investment[:]
    config.investments.investment.extend(investments)

    return config
