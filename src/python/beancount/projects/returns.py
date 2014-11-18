#!/usr/bin/env python3
"""Compute the returns of a portfolio.

A document exists to describe the problem in more detail.
http://furius.ca/beancount/doc/portfolio-returns

Calculating the returns is carried out by identifying the entries whose accounts
match a regular expression that defines accounts to consider for valuation, to
compute the returns over. This set of "related accounts" must also cover the
internal flows that occur within that account, that is, the income and expense
accounts that result in the activity of the portfolio itself.

We consider three sets of accounts:

  "Assets accounts": Accounts whose balances are counted towards calculating the
    total value of the portfolio. These are asset accounts that match the
    regular expression pattern.

  "Internal flow accounts": Accounts which are not valued, but which are used to
    post internal activity of the account. These are income received as a result
    of the portfolio activity, such as dividends or realized capital gains, and
    expenses incurred as a result of carrying out activity related to the
    investment activity, such as commissions and fees. These are income and
    expenses accounts.

  "External flow accounts": Accounts that are considered external to the group
    of related accounts. These are accounts from which funds will be deposited
    or withdrawn. These deposits or withdrawals must be excluded from the
    portfolio returns. Their presence is the reason computing portfolio returns
    isn't just a trivial exercise!

"""
import argparse
import copy
import re
import logging

from beancount.core import amount
from beancount import loader
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import account_types
from beancount.core import inventory
from beancount.ops import prices


def find_matching(entries, acc_types, related_regexp):
    """Match entries and identify account groups.

    Args:
      entries: A list of directives.
      acc_types: An instance of account_types.AccountTypes
      related_regexp: A regular expression string that defines the set of
        related accounts.
    Returns:
      A list of all entries with an account matching the given pattern, and a
      triplet of account lists:
        accounts_assets: A set of the asset accounts in the related group.
        accounts_intflows: A set of the internal flow accounts in the related group.
        accounts_extflows: A set of the external flow accounts.
    """
    accounts_assets = set()
    accounts_intflows = set()
    accounts_extflows = set()
    match = re.compile(related_regexp).match

    matching_entries = []
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue
        if any(match(posting.account) for posting in entry.postings):
            matching_entries.append(entry)

            for posting in entry.postings:
                if match(posting.account):
                    if account_types.is_income_statement_account(posting.account, acc_types):
                        accounts_intflows.add(posting.account)
                    else:
                        accounts_assets.add(posting.account)
                else:
                    accounts_extflows.add(posting.account)

    return (matching_entries, (accounts_assets,
                               accounts_intflows,
                               accounts_extflows))


def compute_returns(entries, options_map, related_regexp):
    """Compute the returns of a portfolio of accounts defined by a regular expression.

    Args:
      entries: A list of directives.
      options_map: An options dict as produced by the loader.
    Returns:
      FIXME: INCOMPLETE.
    """

    acc_types = options.get_account_types(options_map)
    price_map = prices.build_price_map(entries)

    # Fetch the matching entries and figure out account name groups.
    matching_entries, (accounts_assets,
                       accounts_intflows,
                       accounts_extflows) = find_matching(entries, acc_types,
                                                          related_regexp)
    accounts_related = accounts_assets | accounts_intflows

    logging.info('Asset accounts:')
    for account in sorted(accounts_assets):
        logging.info('  %s', account)
    logging.info('Internal flows:')
    for account in sorted(accounts_intflows):
        logging.info('  %s', account)
    logging.info('External flows:')
    for account in sorted(accounts_extflows):
        logging.info('  %s', account)

    # Predicates based on account groups determined above.
    is_external_flow_entry = lambda entry: (isinstance(entry, data.Transaction) and
                                            any(posting.account in accounts_extflows
                                                for posting in entry.postings))

    # Verify that external flow entries only affect balance sheet accounts and
    # not income or expenses accounts (internal flows). We do this because we
    # want to ensure that all income and expenses are incurred against assets
    # that live within the assets group. An example of something we'd like to
    # avoid is an external flow paying for fees incurred within the account that
    # should diminish the returns of the related accounts.
    for entry in entries:
        if is_external_flow_entry(entry):
            if any(posting.account in accounts_intflows for posting in entry.postings):
                logging.error(
                    "External flow may not affect non-asset accounts: {}".format(entry))


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument('filename', help='Ledger filename')

    parser.add_argument('-r', '--related', action='store',
                        default='.*:ScotiaBank:.*',
                        help="A regular expression for related accounts")

    # parser.add_argument('-v', '--verbose', action='store_true',
    #                     help="Output detailed processing information. Useful for debugging")

    opts = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    # Load the input file and build the price database.
    entries, errors, options_map = loader.load(opts.filename)

    compute_returns(entries, options_map, opts.related)


if __name__ == '__main__':
    main()
