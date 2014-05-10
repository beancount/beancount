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

from collections import defaultdict
import argparse
import pprint

from beancount.core import data
from beancount.core import inventory
from beancount import loader
from beancount.parser import printer


def group_entries_by_link(entries):
    """Group the list of entries by link.

    Args:
      entries: A list of directives/transactions to process.
    Returns:
      A dict of link-name to list of entries.
    """
    link_groups = defaultdict(list)
    for entry in entries:
        if not (isinstance(entry, data.Transaction) and entry.links):
            continue
        for link in entry.links:
            link_groups[link].append(entry)
    return link_groups


def get_common_accounts(entries):
    """Compute the intersection of the accounts on the given entries.

    Args:
      entries: A list of directives/transactions to process.
    Returns:
      A set of strings, the names of the common accounts from these
      entries.
    """
    if not entries:
        return set()
    entries_iter = iter(entries)
    intersection = set(posting.account for posting in next(entries_iter).postings)
    for entry in entries_iter:
        accounts = set(posting.account for posting in entry.postings)
        intersection &= accounts
        if not intersection:
            break
    return intersection


def tag_pending_transactions(entries, tag_name, matching_link_regexp=None):
    """Filter out incomplete linked transactions to a transfer account.

    Given a list of entries, group the entries by their link (only for entries
    matching the given regexp if specified) and compute the balance of the
    intersection of their common accounts. If the balance does not sum to zero,
    insert a 'tag_name' tag in the entries.

    Args:
      entries: A list of directives/transactions to process.
      tag_name: A string, the name of the tag to be inserted if a linked group
        of entries is found not to match
      matching_link_regexp: A string, a regular expression used to restrict the
        link names which are subjected to this matching pending checks.
    Returns:
      A modified set of entries, possibly tagged as pending.
    """
    link_groups = group_entries_by_link(entries)

    pending_entry_ids = set()
    for link, link_entries in link_groups.items():
        # Skip links which do not match the restricting regexp.
        if matching_link_regexp and not re.match(matching_link_regexp, link):
            continue

        if len(link_entries) < 2:
            pending = True
        else:
            common_accounts = get_common_accounts(link_entries)
            balance = inventory.Inventory()
            for entry in link_entries:
                for posting in entry.postings:
                    if posting.account in common_accounts:
                        balance.add_position(posting.position)
            pending = not balance.is_empty()

        for entry in link_entries:
            pending_entry_ids.add(id(entry))

    return [entry._replace('tag', entry.tags | tag_name)
            if id(entry) in pending_entry_ids
            else entry
            for entry in entries]


def main():
    """Print out a list of the unpaid transactions."""
    parser = argparse.ArgumentParser(__doc__)

    parser.add_argument('filename',
                        help='Beancount input filename.')

    parser.add_argument('--matching-links-format', action='store',
                        help="A regular expression used to filter links to be "
                        "subjected to matching pending check.")

    opts = parser.parse_args()

    entries, errors, options = loader.load(opts.filename, do_print_errors=True)
    pending_entries = get_pending_transactions(entries, opts.account)

    if pending_entries:
        print('Pending/incomplete transactions:')
        print()
        for entry in pending_entries:
            print(printer.format_entry(entry))


if __name__ == '__main__':
    main()
