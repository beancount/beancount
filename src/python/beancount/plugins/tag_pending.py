#!/usr/bin/env python3
"""An example of tracking unpaid payables or receivables.

A user with lots of invoices to track may want to produce a report of pending or
incomplete payables or receivables. Beancount does not by default offer such a
dedicated feature, but it is easy to build one by using existing link attributes
on transactions. This is an example on how to implement that with a plugin.

For example, assuming the user enters linked transactions like this:

  2013-03-28 * "Bill for datacenter electricity"  ^invoice-27a30ab61191
    Expenses:Electricity			  450.82 USD
    Liabilities:AccountsPayable

  2013-04-15 * "Paying electricity company" ^invoice-27a30ab61191
    Assets:Checking			  	-450.82 USD
    Liabilities:AccountsPayable

Transactions are grouped by link ("invoice-27a30ab61191") and then the
intersection of their common accounts is automatically calculated
("Liabilities:AccountsPayable"). We then add up the balance of all the postings
for this account in this link group and check if the sum is zero. If there is a
residual amount in this balance, we mark the associated entries as incomplete by
inserting a #PENDING tag on them. The user can then use that tag to navigate to
the corresponding view in the web interface, or just find the entries and
produce a listing of them.
"""

__author__ = 'Martin Blais <blais@furius.ca>'

import argparse

from beancount.core import data
from beancount.core import inventory
from beancount.ops import basicops
from beancount import loader
from beancount.parser import printer

__plugins__ = ('tag_pending_plugin',)


def tag_pending_transactions(entries, tag_name='PENDING'):
    """Filter out incomplete linked transactions to a transfer account.

    Given a list of entries, group the entries by their link and compute the
    balance of the intersection of their common accounts. If the balance does
    not sum to zero, insert a 'tag_name' tag in the entries.

    Args:
      entries: A list of directives/transactions to process.
      tag_name: A string, the name of the tag to be inserted if a linked group
        of entries is found not to match
    Returns:
      A modified set of entries, possibly tagged as pending.

    """
    link_groups = basicops.group_entries_by_link(entries)

    pending_entry_ids = set()
    for link, link_entries in link_groups.items():
        assert link_entries
        if len(link_entries) == 1:
            # If a single entry is present, it is assumed incomplete.
            pending_entry_ids.add(id(link_entries[0]))
        else:
            # Compute the sum total balance of the common accounts.
            common_accounts = basicops.get_common_accounts(link_entries)
            balance = inventory.Inventory()
            for entry in link_entries:
                for posting in entry.postings:
                    if posting.account in common_accounts:
                        balance.add_position(posting.position)

            # Mark entries as pending if a residual balance is found.
            if not balance.is_empty():
                for entry in link_entries:
                    pending_entry_ids.add(id(entry))

    # Insert tags if marked.
    return [(data.entry_replace(entry, tags=(entry.tags or set()) | set((tag_name,)))
             if id(entry) in pending_entry_ids
             else entry)
            for entry in entries]


def tag_pending_plugin(entries, options_map):
    """A plugin that finds and tags pending transactions.

    Args:
      entries: A list of entry instances.
      options_map: A dict of options parsed from the file.
    Returns:
      A tuple of entries and errors.
    """
    return (tag_pending_transactions(entries, 'PENDING'), [])


def main():
    """Print out a list of the unpaid transactions."""
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('filename', help='Beancount input filename.')
    opts = parser.parse_args()

    # Parse the entries with a filter plugin.
    loader.install_load_plugin(tag_pending_plugin)
    entries, errors, _ = loader.load(opts.filename, do_print_errors=True)

    # Print the entries that have been tagged.
    pending_entries = [entry for entry in basicops.filter_tag('PENDING', entries)]
    if pending_entries:
        print('Pending/incomplete transactions:')
        print()
        for entry in pending_entries:
            print(printer.format_entry(entry))


if __name__ == '__main__':
    main()
