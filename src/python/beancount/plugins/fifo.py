"""A plugin that automatically converts postings at price to postings held at
cost, applying an automatic booking algorithm in assigning the cost bases and
matching lots.

This plugin restricts itself to applying these transformations within a
particular account, which you provide. For each of those accounts, it also
requires a corresponding Income account to book the profit/loss of reducing
lots (i.e., sales):

  plugin "beancount.plugins.fifo" "Assets:Bitcoin,Income:Bitcoin"

Then, simply input the transactions with price conversion. We use "Bitcoins" in
this example, converting Bitcoin purchases that were carried out as currency
into maintaining these with cost basis, for tax reporting purposes:

  2015-09-04 * "Buy some bitcoins"
    Assets:Bank          -1000.00 USD
    Assets:Bitcoin       4.333507 BTC @ 230.76 USD

  2015-09-05 * "Buy some more bitcoins at a different price"
    Assets:Bank          -1000.00 USD
    Assets:Bitcoin       4.345747 BTC @ 230.11 USD

  2015-09-20 * "Use (sell) some bitcoins"
    Assets:Bitcoin       -6.000000 BTC @ 230.50 USD
    Expenses:Something

The result is that cost bases are inserted on augmenting lots:

  2015-09-04 * "Buy some bitcoins"
    Assets:Bitcoin  4.333507 BTC {230.76 USD} @ 230.76 USD
    Assets:Bank     -1000.00 USD

  2015-09-05 * "Buy some more bitcoins at a different price"
    Assets:Bitcoin  4.345747 BTC {230.11 USD} @ 230.11 USD
    Assets:Bank     -1000.00 USD

While on reducing lots, matching FIFO lots are automatically found and the
corresponding cost basis added:

  2015-09-20 * "Use (sell) some bitcoins"
    Assets:Bitcoin          -4.333507 BTC {230.76 USD} @ 230.50 USD
    Assets:Bitcoin          -1.666493 BTC {230.11 USD} @ 230.50 USD
    Income:Bitcoin         0.47677955 USD
    Expenses:Something  1383.00000000 USD

Note that multiple lots were required to fulfill the sale quantity here. As in
this example, this may result in multiple lots being created for a single one.

Finally, Beancount will eventually support booking methods built-in, but this is
a quick method that shows how to hack your own booking method via
transformations of the postings that run in in a plugin.
"""

__author__ = 'Martin Blais <blais@furius.ca>'
__plugins__ = ('book_price_conversions_as_fifo',)

import copy
import collections
import logging
import re

from beancount.core.number import ZERO
from beancount.core.number import D
from beancount.core.amount import Amount
from beancount.core.position import Position
from beancount.core import data
from beancount.parser import printer



def is_matching(posting, account):
    """"A special posting is one in the FIFO currency that doesn't have a cost and that
    has a price.

    Args:
      posting: An instance of a Posting.
      account: The account name configured.
    Returns:
      A boolean, true if this posting is one that we should be concerned with.
    """
    return (posting.account == account and
            posting.position.lot.cost is None and
            posting.price is not None)


# An error in the configuration for this plugin.
ConfigError = collections.namedtuple('ConfigError', 'source message entry')

# A failure to find a matching lot.
FIFOError = collections.namedtuple('FIFOError', 'source message entry')


def book_price_conversions_as_fifo(entries, options_map, config):
    # The expected configuration is two account names, separated by whitespace.
    errors = []
    try:
        assets_account, income_account = re.split(r'[ \t,;]', config)
    except Exception as exc:
        errors.append(
            ConfigError(None,
                        ('Invalid configuration: "{}"; '
                         'should be two accounts, skipping booking').format(config),
                        None))
        return entries, errors

    # Pairs of (Position, Transaction) instances used to match augmenting
    # entries with reducing ones.
    fifo_lots = []

    new_entries = []
    for entry in entries:

        # Figure out if this transaction has postings in Bitcoins without a cost.
        # The purpose of this plugin is to fixup those.
        if isinstance(entry, data.Transaction) and any(is_matching(posting, assets_account)
                                                       for posting in entry.postings):
            # Segregate the reducing lots, augmenting lots and other lots.
            augmenting, reducing, other = [], [], []
            for index, posting in enumerate(entry.postings):
                if is_matching(posting, assets_account):
                    out = augmenting if posting.position.number >= ZERO else reducing
                else:
                    out = other
                out.append(posting)

            # We will create a replacement list of postings with costs filled
            # in, possibly more than the original list, to account for the
            # different lots.
            new_postings = []

            # Convert all the augmenting postings to cost basis.
            for posting in augmenting:
                lot = posting.position.lot._replace(cost=copy.copy(posting.price))
                pos = Position(lot, posting.position.number)
                new_postings.append(posting._replace(position=pos))
                fifo_lots.append( (copy.copy(pos), entry) )

            # Then process all the reducing postings, booking them to FIFO basis.
            pnl = ZERO
            for posting in reducing:
                match_number = -posting.position.number
                match_currency = posting.position.lot.currency
                cost_currency = posting.price.currency
                while match_number != ZERO:
                    # Find the first lot with matching currency.
                    for fpos, fentry in fifo_lots:
                        if (fpos.lot.currency == match_currency and
                            fpos.lot.cost and fpos.lot.cost.currency == cost_currency):
                            break
                    else:
                        errors.append(
                            FIFOError(posting.meta,
                                      "Could not match position {}".format(posting),
                                      entry))
                        break

                    # Reduce the FIFO lots.
                    number = min(match_number, fpos.number)
                    cost = fpos.lot.cost
                    match_number -= number
                    fpos.number -= number
                    if fpos.number == ZERO:
                        fifo_lots.pop(0)

                    # Add a corresponding posting.
                    pos = Position(lot._replace(cost=copy.copy(cost)), -number)
                    new_postings.append(posting._replace(position=pos))

                    # Update the P/L.
                    pnl += number * (posting.price.number - pos.lot.cost.number)

            if reducing:
                # If some reducing lots were seen in this transcation, insert an
                # Income leg to absorb the P/L.
                new_postings.append(
                    data.Posting(income_account,
                                 data.Position(data.Lot(cost_currency, None, None), -pnl),
                                 None, None, None))

            # Third, add back all the other unrelated legs in.
            for posting in other:
                new_postings.append(posting)

            entry = entry._replace(postings=new_postings)

        new_entries.append(entry)

    return new_entries, errors


# FIXME: TODO
# - Add links too
# - Show how to report trades using links, running this file as a script.
# - Move code in functions, add tests.
