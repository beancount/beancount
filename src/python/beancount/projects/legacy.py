#!/usr/bin/env python3
"""A script to produce a document that describes all assets.

This script produces a document to be attached to a legal will, a document that
describes the full list of all assets and account names with institutions names
and addresses, to make the work of gathering and liquidating assets easy. It
uses various metadata fields to group Beancount accounts together and produce an
output that shoul be readable by a lay-person. The script supports loading and
merging encrypted Beancount files.

The metadata fields used are as follows:

institution: A short string, the name of a group of accounts. This should be the
  name of the institution.

The following fields can be attached to any of the accounts grouped in an
institution:

  summary: A long string that describes why these accounts were opened and what
    they are expected to contain.
  address: The address of an institution.

For each account, the following fields are rendered if present:

  description: What this account is expected to contain.
  account: The account number.
  ignore: True if we should ignore the account.

"""
__author__ = 'Martin Blais <blais@furius.ca>'

import collections
import logging
import re
import subprocess
import sys
import textwrap
from os import path

from beancount import loader
from beancount.core import realization
from beancount.core import getters
from beancount.core import account_types
from beancount.core import data
from beancount.core import account
from beancount.parser import options
from beancount.parser import printer


def group_accounts_by_metadata(entries, field_name):
    """Group the accounts hierarchy by the value of a metadata field on the corresponding
    Open entry of in one of its parent accounts.

    Accounts without a corresponding declaration will end up in the special
    empty ('') group key.

    Args:
      entries: A list of directives.
      field_name: A string, the name of a metadata key to extract to figure out the
        group name.
    Returns:
      A dict of group names (the values of the metadata field) to a list of account
      name strings.
    """
    open_close_map = getters.get_account_open_close(entries)
    groups = collections.defaultdict(list)
    for account_ in open_close_map:
        # Find the group of this account; the group is defined as the first
        # parent account that has a particular metadata field. If an account is
        # not covered by a parent with the metadata, it defines its own group.
        for parent_account in account.parents(account_):
            open_entry, close_entry = open_close_map.get(parent_account, (None, None))
            if (open_entry is not None and
                open_entry.meta and
                field_name in open_entry.meta):
                group = open_entry.meta[field_name]
                break
        else:
            group = ''
        groups[group].append(account_)
    return dict(groups)



def get_first_meta(entries, field_name):
    """Get the first metadata value for a field in a list of entries.

    Args:
      entries: A list of directives, with metadata.
      field_name: A string, the key of a metadata field to fetch.
    Returns:
      The first seen value for field 'field_name'.
    """
    for entry in entries:
        if field_name in entry.meta:
            return entry.meta[field_name]


Institution = collections.namedtuple('Institution', 'name summary address accounts')
Account = collections.namedtuple('Account', 'name date number description')


def print_latex_report(institutions):
    """Print the readable output to stdout.

    Args:
      institutions: A list of Institutions instances.
    """
    print(PREAMBLE.lstrip())

    for institution in institutions:
        print('\\section{%s}' % institution.name)

        if institution.summary:
            print('{%s}\n\n' % institution.summary)
        if institution.address:
            print('Address: {%s}\n\n' % institution.address)

        print('\\begin{itemize}')
        for account_ in institution.accounts:
            print('\\item %s' % account_.name)
        print('\\end{itemize}')

        print()

    print(POSTSCRIPT)


PREAMBLE = r"""
\documentclass[letterpaper,10pt]{article}

\usepackage{fixltx2e}
\usepackage{cmap}
\usepackage{ifthen}
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}

\usepackage{times}
\usepackage{fullpage}
\thispagestyle{empty}

\begin{document}
"""

POSTSCRIPT = """
\end{document}
"""


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount ledger filename')
    args = parser.parse_args()

    logging.info('Read the data')
    entries, _, options_map = loader.load_file(args.filename,
                                               log_errors=logging.error)
    acc_types = options.get_account_types(options_map)

    logging.info('Group the accounts using groups defined implicitly by metadata')
    groups = group_accounts_by_metadata(entries, 'institution')

    logging.info("Select only balance sheet accounts that are open")
    open_close_map = getters.get_account_open_close(entries)
    open_map = {key: value[0] for key, value in open_close_map.items()}
    close_map = {key: value[1] for key, value in open_close_map.items()}
    new_groups = {}
    for group, account_list in groups.items():
        filtered_accounts = [
            account_
            for account_ in account_list
            if (not (open_map[account_].meta or {}).get('ignore', False) and
                close_map[account_] is None and
                account_types.is_balance_sheet_account(account_, acc_types))]
        if filtered_accounts:
            new_groups[group] = filtered_accounts
    groups = new_groups

    logging.info("Gather and organize the data")
    institutions = []
    for group, child_accounts in sorted(groups.items()):
        if not group:
            for child_account in child_accounts:
                logging.error('Ungrouped Account: {}'.format(child_account))
            continue
        values = {field: get_first_meta(map(open_map.__getitem__,
                                            sorted(child_accounts)), field)
                  for field in ('summary', 'address')}
        institution = Institution(group, values['summary'], values['address'], [])
        institutions.append(institution)

        for child_account in sorted(child_accounts):
            open_entry = open_map[child_account]
            meta = open_map[child_account].meta or {}
            institution.accounts.append(
                Account(child_account,
                        open_entry.date,
                        meta.get('number', None),
                        meta.get('description', None)))

    logging.info("Produce a report")
    print_latex_report(institutions)


if __name__ == '__main__':
    main()
