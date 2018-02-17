"""A plugin that automatically inserts a balance check on a tagged closing posting.

Some postings are known to the user to be "closing trades", which means that the
resulting position of the instrument just after the trade should be zero. For
instance, this is the case for most ordinary options trading, only one lot of a
particular instrument is held, and eventually expires or gets sold off in its
entirely. One would like to confirm that, and the way to do this in Beancount is
to insert a balance check.

This plugin allows you to do that more simply, by inserting metadata. For
example, this transaction:

  2018-02-16 * "SOLD -14 QQQ 100 16 FEB 18 160 CALL @5.31"
    Assets:US:Brokerage:Main:Options     -1400 QQQ180216C160 {2.70 USD} @ 5.31 USD
      closing: TRUE
    Expenses:Financial:Commissions       17.45 USD
    Expenses:Financial:Fees               0.42 USD
    Assets:US:Brokerage:Main:Cash      7416.13 USD
    Income:US:Brokerage:Main:PnL

Would expand into the following two directives:

  2018-02-16 * "SOLD -14 QQQ 100 16 FEB 18 160 CALL @5.31"
    Assets:US:Brokerage:Main:Options     -1400 QQQ180216C160 {2.70 USD} @ 5.31 USD
    Expenses:Financial:Commissions       17.45 USD
    Expenses:Financial:Fees               0.42 USD
    Assets:US:Brokerage:Main:Cash      7416.13 USD
    Income:US:Brokerage:Main:PnL

  2018-02-17 balance Assets:US:Brokerage:Main:Options  0 QQQ180216C160

Insert the closing line when you know you're closing the position.
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import datetime

from beancount.core.number import ZERO
from beancount.core import data
from beancount.core import amount

__plugins__ = ('check_closing',)


def check_closing(entries, options_map):
    """Expand 'closing' metadata to a zero balance check.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    new_entries = []
    for entry in entries:
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                if posting.meta and posting.meta.get('closing', False):
                    # Remove the metadata.
                    meta = posting.meta.copy()
                    del meta['closing']
                    entry = entry._replace(meta=meta)

                    # Insert a balance.
                    date = entry.date + datetime.timedelta(days=1)
                    balance = data.Balance(data.new_metadata("<check_closing>", 0),
                                           date, posting.account,
                                           amount.Amount(ZERO, posting.units.currency),
                                           None, None)
                    new_entries.append(balance)
        new_entries.append(entry)
    return new_entries, []
