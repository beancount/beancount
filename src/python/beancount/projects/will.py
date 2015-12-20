#!/usr/bin/env python3
"""A script to produce a document that describes assets and account information.

This script produces a document to be attached to a legal will, a document that
describes the full list of all assets and account names with institutions names
and addresses, to make the work of gathering and liquidating assets easy. It
uses various metadata fields to group Beancount accounts together and produce an
output that should be readable by a layperson.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import collections
import logging
import io
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
from beancount.core import realization
from beancount.parser import options
from beancount.parser import printer


def group_accounts_by_metadata(accounts_map, meta_name):
    """Group accounts by the value of a metadata field on its corresponding Open
    entry or in one of its parent accounts.

    Args:
      accounts_map: A mapping of account name to its Open entry.
      meta_name: A string, the name of a metadata key to extract to figure out the
        group name.
    Returns:
      A dict of group names (the values of the metadata field) to a list of account
      name strings.
    """
    groups = collections.defaultdict(list)
    for account_ in accounts_map:
        # Find the group of this account; the group is defined as the first
        # parent account that has a particular metadata field. If an account is
        # not covered by a parent with the metadata, it defines its own group.
        for parent_account in account.parents(account_):
            open_entry = accounts_map.get(parent_account, None)
            if (open_entry and open_entry.meta and meta_name in open_entry.meta):
                group = open_entry.meta[meta_name]
                groups[group].append(account_)
                break
    for group in groups.values():
        group.sort()
    return dict(groups)


def find_institutions(entries, options_map):
    """Gather all the institutions and valid accounts from the list of entries.
    """
    acc_types = options.get_account_types(options_map)

    # Filter out accounts that are closed or that are income accounts.
    open_close_map = getters.get_account_open_close(entries)
    accounts_map = {acc: open_entry
                    for acc, (open_entry, close_entry) in open_close_map.items()
                    if (account_types.is_balance_sheet_account(acc, acc_types) and
                        close_entry is None)}

    # Group the accounts using groups defined implicitly by metadata.
    return group_accounts_by_metadata(accounts_map, 'institution')


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


# Report data types.
Report = collections.namedtuple('Report', 'title institutions')

InstitutionReport = collections.namedtuple('Institution', 'name fields accounts')
INSTITUTION_FIELDS = ['address', 'phone', 'website', 'representative']

AccountReport = collections.namedtuple('Account', 'name open_date balance fields')
ACCOUNT_FIELDS = ['number', 'description']

def create_report(entries, options_map):
    real_root = realization.realize(entries)

    # Find the institutions from the data.
    groups = find_institutions(entries, options_map)

    # Gather missing fields and create a report object.
    oc_map = getters.get_account_open_close(entries)
    institutions = []
    for name, accounts in sorted(groups.items()):
        # Get the institution fields.
        fields = {field: get_first_meta((oc_map[acc][0] for acc in accounts), field)
                  for field in INSTITUTION_FIELDS}

        # Create infos for each account in this institution.
        account_reports = []
        for accname in accounts:
            fields = {field: get_first_meta((oc_map[acc][0] for acc in accounts), field)
                      for field in ACCOUNT_FIELDS}
            open_date = oc_map[accname][0].date
            real_node = realization.get(real_root, accname)
            account_reports.append(AccountReport(
                accname, open_date, real_node.balance.to_string(), fields))

        # Create the institution report.
        institution = InstitutionReport(name, fields, account_reports)
        institutions.append(institution)

    return Report(options_map['title'], institutions)


def format_xhtml_report(report):
    oss = io.StringIO()

    iss = io.StringIO()
    for inst in report.institutions:
        iss.write('''
          <div class="institution">
            <h2>{i.name}</h2>
          </div>
        '''.format(i=inst))

    oss.write('''
      <div class="report">
        <h1 class="title">{r.title}</h1>
        {institutions}
      </div>
    '''.format(r=report, institutions=iss.getvalue()))

    return oss.getvalue()


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
    parser.add_argument('filename', help='Beancount input filename')
    args = parser.parse_args()

    entries, _, options_map = loader.load_file(args.filename,
                                               log_errors=logging.error)

    report = create_report(entries, options_map)

    text = format_xhtml_report(report)
    sys.stdout.write(text)


if __name__ == '__main__':
    main()
