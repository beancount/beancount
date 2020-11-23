"""Insert an posting with a default account when there is only a single posting.

This is convenient to use in files which have mostly expenses, such as during a trip.
Set the name of the default account to fill in as an option.
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import account
from beancount.core import convert
from beancount.core import data
from beancount.core import inventory

__plugins__ = ('fill_account',)


FillAccountError = collections.namedtuple('FillAccountError', 'source message entry')


def fill_account(entries, unused_options_map, insert_account):
    """Insert an posting with a default account when there is only a single posting.

    Args:
      entries: A list of directives.
      unused_options_map: A parser options dict.
      insert_account: A string, the name of the account.
    Returns:
      A list of entries, possibly with more Price entries than before, and a
      list of errors.
    """
    if not account.is_valid(insert_account):
        return entries, [
            FillAccountError(data.new_metadata('<fill_account>', 0),
                             "Invalid account name: '{}'".format(insert_account),
                             None)]

    new_entries = []
    for entry in entries:
        if isinstance(entry, data.Transaction) and len(entry.postings) == 1:
            inv = inventory.Inventory()
            for posting in entry.postings:
                if posting.cost is None:
                    inv.add_amount(posting.units)
                else:
                    inv.add_amount(convert.get_cost(posting))
            inv.reduce(convert.get_units)
            new_postings = list(entry.postings)
            for pos in inv:
                new_postings.append(data.Posting(insert_account, -pos.units,
                                                 None, None, None, None))
            entry = entry._replace(postings=new_postings)
        new_entries.append(entry)

    return new_entries, []
