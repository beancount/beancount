"""Remove commissions from P/L of closing/sales transactions.

This plugin inserts a pair of postings to remove the commissions portion of the
P/L. For example, this transaction:

  2014-06-18 * "Sold some stock"
    Assets:US:Invest:HOOL                   -9 HOOL {356.32 USD} @ 321.28 USD
    Assets:US:Invest:HOOL                   -2 HOOL {356.32 USD} @ 321.28 USD
    Expenses:Financial:Commissions        5.15 USD
    Assets:US:Invest:Cash              3435.08 USD
    Income:US:Invest:HOOL:PnL           479.29 USD

Becomes this:

  2014-06-18 * "Sold some stock"
    Assets:US:Invest:HOOL                   -9 HOOL {356.32 USD} @ 321.28 USD
    Assets:US:Invest:HOOL                   -2 HOOL {356.32 USD} @ 321.28 USD
    Expenses:Financial:Commissions        5.15 USD
    Assets:US:Invest:Cash              3435.08 USD
    Income:US:Invest:HOOL:PnL           479.29 USD
    Income:US:Invest:HOOL:PnL             5.15 USD
    Income:US:Invest:HOOL:Commissions    -5.15 USD

Note that in the transaction above, this INCREASES the loss: remember that the
Income amounts are greater when they're negative.

You need to configure the plugin with three account regexps: the commissions
account to be debited, the P/L account to be debited from, and the P/L account
where to move the commissions income to. For example:

  plugin "office.commissions" "
    Expenses:Financial:Commissions
    Income:US:Invest:.*:PnL
    Income:US:Invest:Commissions"


Secondly, this plugin also distributes the commission onto each of the postings
held at cost in proportion to their number of units and attaches that amount as
a 'commission' metadata. This can be used for reporting on individual lots
without the commissions. See the list-wash-sales script in experiments, which
uses this.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import re

from beancount.core import data
from beancount.core import amount
from beancount.core import inventory


__plugins__ = ('remove_commissions',)


Error = collections.namedtuple('Error', 'source message entry')


def parse_config(config):
    """Parse the configuration accounts & regular expressions.

    Args:
      config: A configuration string, as per the plugin option.
    Returns:
      A list of commission_regexp, income_regexp, outgoing_account strings.
    Raises:
      ValueError: If the input string is invalid.
    """
    try:
        commission_str, income_str, outgoing_account = config.split()
        income_regexp = re.compile(income_str)
        commission_regexp = re.compile(commission_str)
    except (ValueError, re.error) as exc:
        raise ValueError(exc)
    return commission_regexp, income_regexp, outgoing_account


def remove_commissions(entries, unused_options_map, config):
    """Remove the commissions from the P/L of closing/sales transactions."""

    try:
        commission_regexp, income_regexp, outgoing_account = parse_config(config)
    except ValueError:
        return [], [
            Error(None, "Invalid configuration for {} plugin".format(__file__), None)]

    new_entries = []
    for entry in entries:
        # Match the transaction.
        if (isinstance(entry, data.Transaction) and
            any(income_regexp.match(posting.account)
                for posting in entry.postings) and
            any(commission_regexp.match(posting.account)
                for posting in entry.postings)):

            # Find the commissions amounts.
            commissions = inventory.Inventory()
            for posting in entry.postings:
                if commission_regexp.match(posting.account):
                    commissions.add_amount(posting.units)

            # Find the first income account.
            for posting in entry.postings:
                if income_regexp.match(posting.account):
                    income_account = posting.account
                    break
            assert income_account, "Income account not found."

            # Insert the new legs.
            new_postings = []
            for cposition in commissions:
                new_postings.extend([
                    data.Posting(income_account, cposition.units, None, None, None, None),
                    data.Posting(outgoing_account, -cposition.units, None, None, None, None),
                    ])

                # Distribute the commission.
                distribute_commission_on_metadata(cposition.units, entry.postings)

            entry = entry._replace(postings=entry.postings + new_postings)

        new_entries.append(entry)

    return new_entries, []


def distribute_commission_on_metadata(commission, postings):
    """Distributed the commission to the postings which have a cost basis.

    This function returns nothing; it inserts metadata on the postings.

    Args:
      commission: An Amount instance, the total amount of commission for this trade.
      postings: A list of postings.
    """
    trade_postings = []
    for posting in postings:
        if posting.cost is not None:
            cost = (posting.price.number
                    if posting.price else
                    posting.cost.number)
            trade_postings.append((posting, posting.units.number * cost))
    total = sum(cost for _, cost in trade_postings)
    for posting, cost in trade_postings:
        if 'commission' not in posting.meta:
            posting.meta['commission'] = inventory.Inventory()
        posting_commission = amount.mul(commission, (cost / total))
        posting.meta['commission'].add_amount(posting_commission)
