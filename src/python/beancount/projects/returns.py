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
import io
import re
import logging

from beancount.core.amount import ZERO
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


def compute_returns_with_regexp(entries, options_map, related_regexp):
    """Compute the returns of a portfolio of accounts defined by a regular expression.

    Args:
      entries: A list of directives.
      options_map: An options dict as produced by the loader.
    Returns:
      See compute_returns().
    """
    acc_types = options.get_account_types(options_map)
    price_map = prices.build_price_map(entries)

    # Fetch the matching entries and figure out account name groups.
    matching_entries, (accounts_assets,
                       accounts_intflows,
                       accounts_extflows) = find_matching(entries, acc_types,
                                                          related_regexp)

    logging.info('Asset accounts:')
    for account in sorted(accounts_assets):
        logging.info('  %s', account)

    logging.info('Internal flows:')
    for account in sorted(accounts_intflows):
        logging.info('  %s', account)

    logging.info('External flows:')
    for account in sorted(accounts_extflows):
        logging.info('  %s', account)
    logging.info('')

    return compute_returns(matching_entries, options_map, price_map,
                           accounts_assets, accounts_intflows)


def compute_returns(entries, options_map, price_map,
                    accounts_assets, accounts_intflows):
    """Compute the returns of a portfolio of accounts defined by a regular expression.

    Args:
      entries: A list of directives that may affect the account.
      options_map: An options dict as produced by the loader.
      price_map: An instance of PriceMap as computed by prices.build_price_map().
      accounts_assets: A set of account name strings, the names of the asset accounts
        included in valuing the portfolio.
      accounts_intflow: A set of account name strings, the names of internal flow
        accounts (normally income and expenses) that aren't external flows.
    Returns:
      A dict of currency -> float total returns and a pair of (begin, end) datetime.date
      instances from which it was computed.
    """
    accounts_related = accounts_assets | accounts_intflows

    # Predicates based on account groups determined above.
    is_external_flow_entry = lambda entry: (isinstance(entry, data.Transaction) and
                                            any(posting.account not in accounts_related
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
                oss = io.StringIO()
                printer.print_entry(entry, file=oss)
                logging.error("External flow may not affect non-asset accounts: {}".format(
                    oss.getvalue()))

    # Process all the entries in chronological order.
    balance = inventory.Inventory()
    iter_entries = (entry
                    for entry in entries
                    if isinstance(entry, data.Transaction))
    entry = next(iter_entries)
    done = False
    piecewise_returns = []
    date_first = entry.date
    while not done:
        date_begin = entry.date
        balance_begin = copy.copy(balance)

        # Consume all internal flow entries, simply accumulating the total balance.
        try:
            logging.debug('Internal entries:')
            while not is_external_flow_entry(entry):
                logging.debug("  Balance: %s", balance)
                log_entry(entry, logging.debug)
                for posting in entry.postings:
                    if posting.account in accounts_assets:
                        balance.add_position(posting.position)
                logging.debug("  Balance: %s", balance)
                logging.debug("")
                entry = next(iter_entries)
        except StopIteration:
            pass

        date_end = entry.date
        balance_end = copy.copy(balance)
        returns = compute_period_returns(date_begin, date_end,
                                         balance_begin, balance_end, price_map)
        piecewise_returns.append(returns)

        # Consume all external flow entries, absorbing the balance change before
        # beginning the next period.
        try:
            logging.debug('External entries:')
            while is_external_flow_entry(entry):
                logging.debug("  Balance: %s", balance)
                log_entry(entry, logging.debug)
                for posting in entry.postings:
                    if posting.account in accounts_assets:
                        balance.add_position(posting.position)
                logging.debug("  Balance: %s", balance)
                logging.debug("")
                entry = next(iter_entries)
        except StopIteration:
            done = True
    date_last = entry.date

    # Compute the piecewise returns. Note that we have to be careful to handle
    # all available currencies.
    currencies = set(currency
                     for returns in piecewise_returns
                     for currency in returns.keys())
    total_returns = {}
    for currency in currencies:
        total_return = 1.
        for returns in piecewise_returns:
            total_return *= returns.get(currency, 1.)
        total_returns[currency] = total_return

    return total_returns, (date_first, date_end)


def compute_period_returns(date_begin, date_end,
                           balance_begin, balance_end, price_map):
    """Compute the returns of the given begin/end balances.

    Args:
      date_begin: A datetime.date instance, the beginning date of the period.
      date_end: A datetime.date instance, the end date of the period.
      balance_begin: An instance of the Inventory at the beginning of the period.
      balance_end: An instance of the Inventory at the end of the period.
      price_map: An instance of PriceMap as computed by prices.build_price_map().
    Returns:
      A dict of currency -> floating-point return for the period. The union of
      all currencies at market-value is returned. This is done to be able to evaluate
      and report on returns in multiple currencies.
    """
    # Evaluate the boundary balances at market value.
    mktvalue_begin = prices.get_inventory_market_value(balance_begin, date_begin, price_map)
    mktvalue_end = prices.get_inventory_market_value(balance_end, date_end, price_map)

    # Compute the union of all currencies. At this point, ignore currencies
    # held-at-cost and issue a warning if some are found (if the price database
    # covers all the currencies held at cost, this shuold not occur).
    currencies = set()
    single_begin = {}
    single_end = {}
    for mktvalue, single in [(mktvalue_begin, single_begin),
                             (mktvalue_end, single_end)]:
        for pos in mktvalue.get_positions():
            if pos.lot.cost:
                logging.error('Could not reduce position "%s" to its value', pos)
            else:
                currencies.add(pos.lot.currency)
                assert pos.lot.currency not in single
                single[pos.lot.currency] = pos.number

    # Now for each of the currencies, compute the returns. Handle cases where
    # the currency is not present as a zero value for that currency.
    #
    # Note: In the future, we should instead require more inforamtion about the
    # desired currency for valuation and convert all contents to a single
    # currency above, so this is not needed except to handle really odd cases.
    returns = {}
    for currency in currencies:
        begin = single_begin.get(currency, ZERO)
        end = single_end.get(currency, ZERO)
        if begin == ZERO:
            returns[currency] = 1.0
        else:
            returns[currency] = float(end / begin)

    annual_returns = {}
    if date_end != date_begin:
        annual_returns = annualize_returns(returns, date_begin, date_end)

    logging.info("PERIOD: %s -> %s", date_begin, date_end)
    logging.info("  BEGIN %s => %s", balance_begin, mktvalue_begin)
    logging.info("  END   %s => %s", balance_end, mktvalue_end)
    logging.info("  RETURNS: %s", returns)
    logging.info("  ANNUALIZED: %s", annual_returns)
    logging.info("")

    return returns


def log_entry(entry, log_func):
    """Log an entry either to the logging module.
    """
    oss = io.StringIO()
    printer.print_entry(entry, file=oss)
    for line in oss.getvalue().rstrip().splitlines():
        log_func('  {}'.format(line))



def log_with_prefix(entry, is_external_flow):
    """Log an entry either to the info or debug log.

    Args:
      entry: A Transaction directive.
      is_external_flow: A boolean, true if this is an external flow entry.
    """
    oss = io.StringIO()
    printer.print_entry(entry, file=oss)
    log_func = logging.info if is_external_flow else logging.debug
    for line in oss.getvalue().splitlines():
        log_func(line)


def annualize_returns(returns, date_first, date_last):
    """Annualize the return rates computed from the given date range.

    Args:
      returns: A dict of floats, the calculated returns.
      date_first: A datetime.date instance, the beginning of the period.
      date_last: A datetime.date instance, the beginning of the period.
    Returns:
      A dict of float, the returns adjusted to equivalent annual rates.
    """
    num_days = (date_last - date_first).days
    exponent = 365. / num_days
    return {currency: return_ ** exponent
            for currency, return_ in returns.items()}


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument('filename', help='Ledger filename')

    parser.add_argument('related_regexp', action='store',
                        help="A regular expression for related accounts")

    parser.add_argument('-v', '--verbose', action='store_true',
                        help="Output detailed processing information. Useful for debugging")

    args = parser.parse_args()
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO,
                        format='%(levelname)-8s: %(message)s')

    # Load the input file and build the price database.
    entries, errors, options_map = loader.load(args.filename)

    # Compute the returns.
    returns, (date_first, date_last) = compute_returns_with_regexp(entries,
                                                                   options_map,
                                                                   args.related_regexp)

    # Annualize the returns.
    annual_returns = annualize_returns(returns, date_first, date_last)

    print('Averaged annual returns from {} to {}:'.format(date_first, date_last))
    for currency, return_ in sorted(annual_returns.items()):
        print('  {}: {:.3%}'.format(currency, return_ - 1))


if __name__ == '__main__':
    main()
