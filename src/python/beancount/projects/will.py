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
import pprint
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

AccountReport = collections.namedtuple('Account', 'name open_date balance fields')

def create_report(entries, options_map):
    real_root = realization.realize(entries)

    # Find the institutions from the data.
    groups = find_institutions(entries, options_map)

    # Gather missing fields and create a report object.
    oc_map = getters.get_account_open_close(entries)
    open_map = {acc: open_entry for acc, (open_entry, _) in oc_map.items()}
    institutions = []
    for name, accounts in sorted(groups.items()):

        # Get the institution fields, which is the union of the fields for all
        # the accounts with the institution fields.
        institution_accounts = [acc for acc in accounts
                                if 'institution' in open_map[acc].meta]

        institution_fields = {}
        for acc in institution_accounts:
            for key, value in open_map[acc].meta.items():
                institution_fields.setdefault(key, value)
        institution_fields.pop('filename', None)
        institution_fields.pop('lineno', None)

        # Create infos for each account in this institution.
        account_reports = []
        for acc in accounts:
            open_entry = open_map[acc]
            account_fields = open_entry.meta.copy()
            account_fields.pop('filename', None)
            account_fields.pop('lineno', None)
            for field in institution_fields:
                account_fields.pop(field, None)

            real_node = realization.get(real_root, acc)
            account_reports.append(AccountReport(
                acc, open_entry.date, real_node.balance, account_fields))

        # Create the institution report.
        institution = InstitutionReport(name, institution_fields, account_reports)
        institutions.append(institution)

    return Report(options_map['title'], institutions)


XHTML_TEMPLATE_PRE = '''
<html>
  <head>
    <style type="text/css">

table.accounts {
  border-collapse: collapse;
}

table.accounts td {
  border: thin solid black;
  padding: 0.1em 0.3em;
}

    </style>
  </head>
  <body>
'''
XHTML_TEMPLATE_POST = '''
  </body>
</html>
'''


def format_xhtml_report(report):
    oss = io.StringIO()
    oss.write(XHTML_TEMPLATE_PRE)

    oss.write('''
      <div class="report">
        <h1 class="title">{r.title}</h1>
    '''.format(r=report))

    for inst in report.institutions:
        oss.write('''
          <div class="institution">
            <h2>{i.name}</h2>
            {fields}
        '''.format(i=inst, fields=format_xhtml_table(sorted(inst.fields.items()))))

        # Compute the set of fields to render.
        unique_fields = {key
                         for acc in inst.accounts
                         for key in acc.fields}
        fieldnames = ['_name', '_open_date'] + sorted(unique_fields) + ['_balance']
        if inst.accounts:
            oss.write('''
              <table class="accounts">
              <thead>
              <tr>
            ''')
            for fieldname in fieldnames:
                oss.write('<th>{}</th>'.format(fieldname.strip('_').capitalize()))
            oss.write('</tr></thead>\n')

            for acc in inst.accounts:
                fields = acc.fields.copy()
                fields['_name'] = acc.name
                fields['_open_date'] = acc.open_date
                fields['_balance'] = acc.balance.cost().to_string()
                oss.write('<tr>\n')
                for field in fieldnames:
                    oss.write('<td>{}</td>\n'.format(fields.get(field, '')))
                oss.write('</tr>\n')
            oss.write('</table>\n')

        oss.write('</div>\n')
    oss.write('</div>\n')

    oss.write(XHTML_TEMPLATE_POST)
    return oss.getvalue()


def format_xhtml_table(items):
    """Render a mapping of values to an HTML table.

    Args:
      items: A dict of key/value pairs.
    Returns:
      A string of rendered HTML.
    """
    oss = io.StringIO()
    oss.write('<table class="fields">\n')
    for key, value in items:
        oss.write('<tr><td>{}:</td><td>{}</td></tr>'.format(key.capitalize(), value))
    oss.write('</table>\n')
    return oss.getvalue()


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
