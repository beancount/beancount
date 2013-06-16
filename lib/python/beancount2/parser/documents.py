"""
Everything that relates to creating the Document directives.

This is more related to the parser than the core.
"""
import os
import re
import datetime
import heapq
import logging
from os import path
from collections import namedtuple

from beancount2 import utils
from beancount2.parser import parser
from beancount2.core.data import FileLocation, Document
from beancount2.core import data
from beancount2.core import realization
from beancount2.core import validation


# An error from trying to find the documents.
DocumentError = namedtuple('DocumentError', 'fileloc message entry')


def process_documents(entries, filename, documents_dirs):
    """Do all the processing related to document directives."""

    # Check that the entries from the input file as okay.
    errors = verify_document_entries(entries)

    # Detect filenames that should convert into entries.
    if documents_dirs:
        document_entries, autodoc_errors = (
            process_auto_documents(filename, documents_dirs, entries))

        errors.extend(autodoc_errors)

        # FIXME: I'd like the comparison function to be by 'date' here...
        # We need a mergesort that can take a custom key.
        # entries = heapq.merge(entries, document_entries)
        entries.extend(document_entries)
        entries.sort(key=data.entry_sortkey)

    return entries, errors


def verify_document_entries(entries):
    """Verify that the document entries point to existing files.
    Return a list of DocumentError errors (or an empty list)."""
    document_errors = []

    for entry in utils.filter_type(entries, Document):
        if not path.exists(entry.filename):
            error = DocumentError(entry.fileloc, "File does not exist.", entry)
            document_errors.append(error)

    return document_errors


def process_auto_documents(input_filename, document_dirs, entries):
    """Gather all the documents from the specified document roots in the
    options."""

    new_entries = []
    errors = []

    root = path.dirname(input_filename)
    for document_dir in document_dirs:
        # Compute the documents directory name relative to the beancount input
        # file itself.
        if not path.isabs(document_dir):
            document_dir = path.normpath(path.join(root, document_dir))

        # Ensure the path exists.
        if not path.exists(document_dir):
            fileloc = FileLocation(input_filename, 0)
            error = DocumentError(fileloc,
                                  "Document root '{}' does not exist.".format(document_dir),
                                  None)
            errors.append(error)
        else:
            # Find the documents under this root.
            document_entries = find_documents(document_dir, input_filename, entries)
            new_entries.extend(document_entries)

    new_entries.sort(key=data.entry_sortkey)
    return new_entries, errors


def walk_accounts(root_directory):
    """A version of os.walk() which provides the directory as account name."""
    for root, dirs, files in os.walk(root_directory):
        relroot = root[len(root_directory)+1:]
        account_name = relroot.replace(os.sep, ':')
        yield (root, account_name, dirs, files)



def find_documents(root_directory, input_filename, entries):
    """Find dated document files under the given directory 'root_directory', located
    only in directories that correspond to one of the accounts extracted from
    'entries'.
    """
    new_entries = []

    accounts = data.gather_accounts(entries)
    for root, account_name, dirs, files in walk_accounts(root_directory):

        # Look for files that have a dated filename.
        for filename in files:
            mo = re.match('(\d\d\d\d)-(\d\d)-(\d\d).(.*)', filename)
            if not mo:
                continue

            # FIXME: Decide how we'll add documents that belong in parent
            # accounts with no declarations. This generates errors if we enable
            # it due to our tight error checking.
            if 1:
                # Only look for files in subdirectories that correspond to an account
                # name.
                if account_name not in accounts:
                    # logging.warn("Skipping document '{}' because no corresponding account.".format(
                    #     path.join(root, filename)))
                    continue
                account = accounts[account_name]
            else:
                # Try to find a corresponding account. If this is in a parent
                # account, just create the account.
                try:
                    account = accounts[account_name]
                except KeyError:
                    account = data.account_from_name(account_name)

            # Found one! Create a new directive.
            fileloc = FileLocation(input_filename, -1)
            date = datetime.date(*map(int, mo.group(1,2,3)))
            entry = Document(fileloc, date, account, path.join(root, filename))
            new_entries.append(entry)

    return new_entries
