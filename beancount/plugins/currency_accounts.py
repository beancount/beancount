"""An implementation of currency accounts.

This is an automatic implementation of the method described here:
https://www.mathstat.dal.ca/~selinger/accounting/tutorial.html

You enable it just like this:

    plugin "beancount.plugins.currency_accounts" "Equity:CurrencyAccounts"

Accounts will be automatically created under the given base account, with the
currency name appended to it, e.g.,

    Equity:CurrencyAccounts:CAD
    Equity:CurrencyAccounts:USD

etc., where used. You can have a look at the account balances with a query like
this:

    bean-query $L "select account, sum(position), convert(sum(position), 'USD')
                   where date >= 2018-01-01 and  account ~ 'CurrencyAccounts' "

The sum total of the converted amounts should be a number not too large:

    bean-query $L "select convert(sum(position), 'USD')
                   where date >= 2018-01-01 and  account ~ 'CurrencyAccounts'"

WARNING: This is a prototype. Note the FIXMEs in the code below, which indicate
some potential problems.

"""
__copyright__ = "Copyright (C) 2019  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core.data import Posting
from beancount.core.data import Transaction

from beancount.core import account
from beancount.core import convert
from beancount.core import data
from beancount.core import inventory


__plugins__ = ('insert_currency_trading_postings',)


META_PROCESSED = 'currency_accounts_processed'
DEFAULT_BASE_ACCOUNT = 'Equity:CurrencyAccounts'


def insert_currency_trading_postings(entries, options_map, config):
    """Insert currency trading postings.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
      config: The base account name for currency trading accounts.
    Returns:
      A list of new errors, if any were found.
    """
    base_account = config.strip()
    if not account.is_valid(base_account):
        base_account = DEFAULT_BASE_ACCOUNT

    errors = []
    new_entries = []
    new_accounts = set()
    for entry in entries:
        if isinstance(entry, Transaction):
            curmap, has_price = group_postings_by_weight_currency(entry)
            if has_price and len(curmap) > 1:
                new_postings = get_neutralizing_postings(
                    curmap, base_account, new_accounts)
                entry = entry._replace(postings=new_postings)
                if META_PROCESSED:
                    entry.meta[META_PROCESSED] = True
        new_entries.append(entry)

    earliest_date = entries[0].date
    open_entries = [
        data.Open(data.new_metadata('<currency_accounts>', index),
                  earliest_date, acc, None, None)
        for index, acc in enumerate(sorted(new_accounts))]

    return open_entries + new_entries, errors


def group_postings_by_weight_currency(entry: Transaction):
    """Return where this entry might require adjustment."""
    curmap = collections.defaultdict(list)
    has_price = False
    for posting in entry.postings:
        currency = posting.units.currency
        if posting.cost:
            currency = posting.cost.currency
            if posting.price:
                assert posting.price.currency == currency
            elif posting.price:
                has_price = True
                currency = posting.price.currency
        if posting.price:
            has_price = True
        curmap[currency].append(posting)
    return curmap, has_price


def get_neutralizing_postings(curmap, base_account, new_accounts):
    """Process an entry.

    Args:
      curmap: A dict of currency to a list of Postings of this transaction.
      base_account: A string, the root account name to insert.
      new_accounts: A set, a mutable accumulator of new account names.
    Returns:
      A modified entry, with new postings inserted to rebalance currency trading
      accounts.
    """
    new_postings = []
    for currency, postings in curmap.items():
        # Compute the per-currency balance.
        inv = inventory.Inventory()
        for posting in postings:
            inv.add_amount(convert.get_cost(posting))
        if inv.is_empty():
            new_postings.extend(postings)
            continue

        # Re-insert original postings and remove price conversions.
        #
        # Note: This may cause problems if the implicit_prices plugin is
        # configured to run after this one, or if you need the price annotations
        # for some scripting or serious work.
        #
        # FIXME: We need to handle these important cases (they're not frivolous,
        # this is a prototype), probably by inserting some exceptions with
        # collaborating code in the booking (e.g. insert some metadata that
        # disables price conversions on those postings).
        #
        # FIXME(2): Ouch! Some of the residual seeps through here, where there
        # are more than a single currency block. This needs fixing too. You can
        # easily mitigate some of this to some extent, by excluding transactions
        # which don't have any price conversion in them.
        for pos in postings:
            if pos.price is not None:
                pos = pos._replace(price=None)
            new_postings.append(pos)

        # Insert the currency trading accounts postings.
        amount = inv.get_only_position().units
        acc = account.join(base_account, currency)
        new_accounts.add(acc)
        new_postings.append(
            Posting(acc, -amount, None, None, None, None))

    return new_postings
