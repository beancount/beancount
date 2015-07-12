#!/usr/bin/env python3
"""Split expenses of a Beancount ledger between multiple people.

This plugin is given a list of names. It assumes that any Expenses account whose
components do not include any of the given names are to be split between the
members. It goes through all the transactions and converts all such postings
into multiple postings, one for each member.

For example, given the names 'Martin' and 'Caroline', the following transaction:

  2015-02-01 * "Aqua Viva Tulum - two nights"
     Income:Caroline:CreditCard      -269.00 USD
     Expenses:Accommodation

Will be converted to this:

  2015-02-01 * "Aqua Viva Tulum - two nights"
    Income:Caroline:CreditCard       -269.00 USD
    Expenses:Accommodation:Martin     134.50 USD
    Expenses:Accommodation:Caroline   134.50 USD

After these transformations, all account names should include the name of a
member. You can generate reports for a particular person by filtering postings
to accounts with a component by their name.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import copy
import re

from beancount.core import data
from beancount.core import account
from beancount.core import getters
from beancount.core import account_types
from beancount.parser import options


__plugins__ = ('split_expenses',)


def split_expenses(entries, options_map, config):
    # Validate and sanitize configuration.
    if isinstance(config, str):
        members = config.split()
    elif isinstance(config, (tuple, list)):
        members = config
    else:
        raise RuntimeError("Invalid plugin configuration: configuration for split_expenses "
                           "should be a string or a sequence.")

    acctypes = options.get_account_types(options_map)
    def is_expense_account(account):
        return account_types.get_account_type(account) == acctypes.expenses

    # A predicate to quickly identify if an account contains the name of a
    # member.
    is_individual_account = re.compile('|'.join(map(re.escape, members))).search

    # Existing and previously unseen accounts.
    new_accounts = set()

    # Filter the entries and transform transactions.
    new_entries = []
    for entry in entries:
        if isinstance(entry, data.Transaction):
            new_postings = []
            for posting in entry.postings:
                if (is_expense_account(posting.account) and
                    not is_individual_account(posting.account)):

                    # Split this posting into multiple postings.
                    split_position = copy.copy(posting.position)
                    split_position.number /= len(members)

                    for member in members:
                        # Mark the account as new if never seen before.
                        subaccount = account.join(posting.account, member)
                        new_accounts.add(subaccount)

                        # Add a new posting for each member, to a new account
                        # with the name of this member.
                        new_postings.append(
                            posting._replace(account=subaccount,
                                             position=split_position))
                else:
                    new_postings.append(posting)

            # Modify the entry in-place, replace its postings.
            entry = entry._replace(postings=new_postings)

        new_entries.append(entry)

    # Create Open directives for new subaccounts if necessary.
    oc_map = getters.get_account_open_close(entries)
    open_date = entries[0].date
    meta = data.new_metadata('<split_expenses>', 0)
    open_entries = []
    for new_account in new_accounts:
        if new_account not in oc_map:
            entry = data.Open(meta, open_date, new_account, None, None)
            open_entries.append(entry)

    return open_entries + new_entries, []
