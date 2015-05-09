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


## FIXME: Move these utility functions to the main Beancount codebase, add tests
## for each.

def read_encrypted_file(filename):
    """Load and decrypt an encrypted file.

    Args:
      filename: A string, the path to the encrypted file.
    Returns:
      A string, the contents of the file.
    """
    pipe = subprocess.Popen(
        ['gpg', '--batch', '--decrypt', filename],
        shell=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    contents, errors = pipe.communicate()
    return contents.decode('utf-8')


def load_encrypted_file(filename, **kwds):
    """Load a single file; if encrypted, decrypt and load from string.
    No copy is of the decrypted file is ever made on disk.

    Args:
      filenames: A string, the names of file to parse and load.
      **kwds: Extra arguments for loader.load_file() and loader.load_string(),
        which you can use to specify error reporting arguments.
    Returns:
      A tuple as per loader.load_file() and loader.load_string().
    """
    if re.match('.*\.(asc|gpg)$', filename):
        results = loader.load_string(read_encrypted_file(filename), **kwds)
    else:
        results = loader.load_file(filename, **kwds)
    return results


def load_merge(filenames, **kwds):
    """Load up files and merge them and their metadata where possible.

    This routine will load all the given files and merge their metadata where
    certain types of directives intersect. This routine also supports loading
    files encrypted with GPG.

    Args:
      fileanems: A list of strings, the names of files to parse and load.
      **kwds: Extra arguments for loader.load_file() and loader.load_string(),
        which you can use to specify error reporting arguments.
    Returns:
      A tuple as per loader.load_file() and loader.load_string().
    """
    logging.info('Loading: {}'.format(filenames[0]))
    entries, errors, options_map = load_encrypted_file(filenames[0], **kwds)

    for filename in filenames[1:]:
        logging.info('Loading: {}'.format(filename))
        new_entries, new_errors, new_options_map = load_encrypted_file(filename, **kwds)

        # Merge the metadata and contents of indexed entries.
        entries = update_metadata(entries, new_entries)
        errors.extend(new_errors)

    return entries, errors, options_map


def update_metadata_single(target_entries, source_entries,
                           key_fun, filter_fun):
    """Pull metadata from source entries to target for selected entry types.

    Args:
      target_entries: A list of entries whose metadata we want to augment.
      source_entries: A list of entries from which to pull additional metadata.
      key_fun: A callable that extracts a unique key from an entry.
      filter_fun: A callable that selects an entry for metadata update.
    Returns:
      An augmented version of target_entries.
    """
    # Compute unique mappings between source and target.
    source_map = {key_fun(entry): entry
                  for entry in source_entries
                  if filter_fun(entry)}

    # Filter the target transaction set.
    new_entries = []
    for entry in target_entries:
        if filter_fun(entry):
            source_entry = source_map.pop(key_fun(entry), None)
            if source_entry is not None:
                # Copy the metadata from source to target.
                meta = entry.meta.copy()
                meta.update(source_entry.meta)
                entry = entry._replace(meta=meta)
        new_entries.append(entry)

    # Add in the missing entries.
    for key, entry in source_map.items():
        new_entries.append(entry)

    return new_entries


def update_metadata(target_entries, source_entries):
    """Pull metadata from source entries to target for similar entry types.

    This function copies the metadata of certain particular types of entries,
    like Open and Close directives, onto the corresponding directives in a
    list of target entries. Colliding metadata from source is overwritten
    onto target.

    Args:
      target_entries: A list of entries whose metadata we want to augment.
      source_entries: A list of entries from which to pull additional metadata.
    Returns:
      An augmented version of target_entries.
    """
    entries = target_entries
    get_account = lambda entry: entry.account
    for key_fun, filter_fun in [
            (get_account, lambda entry: isinstance(entry, data.Open)),
            (get_account, lambda entry: isinstance(entry, data.Close)),
    ]:
        entries = update_metadata_single(entries, source_entries, key_fun, filter_fun)
    return data.sorted(entries)


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
    parser.add_argument('ledger_filename', help='Beancount ledger filename')
    parser.add_argument('accounts_filename', help='Encrypted accounts filename')
    args = parser.parse_args()

    logging.info('Read the data')
    entries, _, options_map = load_merge([args.ledger_filename, args.accounts_filename],
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
