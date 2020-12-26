"""Transfer lots to another account maintaining cost basis and specs.

This is a common occurrence with crypto scenarios. Transactions are selected by
providing a tag. Transactions are expected to have a single posting without cost
basis.
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import collections
from typing import List

from beancount.core import account
from beancount.core import convert
from beancount.core import data
from beancount.core import inventory
from beancount.parser import printer

__plugins__ = ('transfer_lots',)


TransferLotsError = collections.namedtuple('TransferLots', 'source message entry')


def transfer_lots(entries: List[data.Directive],
                  unused_options_map: dict,
                  match_tag: str):
    """Transfer lots to another account maintaining cost basis and specs.

    Args:
      entries: A list of directives.
      unused_options_map: A parser options dict.
      match_tag: The name of a tag to select a transact to process.
    Returns:
      A list of entries.
    """
    new_entries = []
    errors = []
    for entry in entries:
        if isinstance(entry, data.Transaction) and match_tag in entry.tags:
            # Find the posting with the transfer acccount. There should be only
            # one posting without a cost basis.
            cost_postings = []
            transfer_postings = []
            for posting in entry.postings:
                plist = transfer_postings if posting.cost is None else cost_postings
                plist.append(posting)
            if len(transfer_postings) != 1:
                errors.append(TransferLotsError(
                    data.new_metadata('<fill_account>', 0),
                    "Could not identify transfer account", None))
            else:
                transfer_account = transfer_postings[0].account
                new_postings = list(cost_postings) + [
                    posting._replace(account=transfer_account,
                                     units=-posting.units)
                    for posting in cost_postings]
                entry = entry._replace(postings=new_postings)
        new_entries.append(entry)

    return new_entries, []
