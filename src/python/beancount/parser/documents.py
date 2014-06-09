"""
Everything that relates to creating the Document directives.

This is more related to the parser than the core.
"""
import os
import re
import datetime
from os import path
from collections import namedtuple

from beancount.core import account
from beancount.core import account_types
from beancount.core.data import FileLocation, Document
from beancount.core import data
from beancount.core import getters


# An error from trying to find the documents.
DocumentError = namedtuple('DocumentError', 'fileloc message entry')


def process_documents(entries, filename, documents_dirs):
    """Check files for document directives and find documents automatically.

    Args:
      entries: a list of all entry objects parsed from the file.
      filename: the name of the ledger input file
      documents_dirs: a list of directory names to be used as roots
                      of the hierarchies that will be searched.
    Returns:
      A pair of list of all entries (including new ones), and errors
      generated during the process of creating document directives.
    """

    # Check that the entries from the input file are okay.
    document_entries = [entry
                        for entry in entries
                        if isinstance(entry, Document)]
    errors = verify_document_entries(document_entries)

    # Detect filenames that should convert into entries.
    if documents_dirs:
        accounts = getters.get_accounts(entries)
        document_entries, autodoc_errors = (
            process_auto_documents(filename, documents_dirs, accounts))

        errors.extend(autodoc_errors)

        # FIXME: I'd like the comparison function to be by 'date' here...
        # We need a mergesort that can take a custom key.
        # entries = heapq.merge(entries, document_entries)
        entries.extend(document_entries)
        entries.sort(key=data.entry_sortkey)

    return entries, errors


def verify_document_entries(document_entries):
    """Verify that the document entries point to existing files.
    Return a list of DocumentError errors (or an empty list).

    Args:
      document_entries: a list of Document objects to be validated.
    Returns:
      A list of errors encountered (hopefully empty).
    """
    document_errors = []

    for entry in document_entries:
        assert isinstance(entry, Document)
        if not path.exists(entry.filename):
            error = DocumentError(entry.fileloc, "File does not exist.", entry)
            document_errors.append(error)

    return document_errors


def process_auto_documents(input_filename, document_dirs, accounts):
    """Gather all the documents from the specified document roots in the options.

    Args:
      input_filename: the name of the ledger input file. This is used to resolve
                      relative path names.
      document_dirs: a list of string, the names of the roots of directory
                     hierarchies to search.
      accounts: a set of accounts to consider as valid ones.
    Returns:
      A pair of (list of new Document objects created from files, a list of errors
      encountered during the processing.)
    """
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
            document_entries = find_documents(document_dir, input_filename, accounts)
            new_entries.extend(document_entries)

    new_entries.sort(key=data.entry_sortkey)
    return new_entries, errors


# FIXME: I think you can remove 'accounts' as a mapping here, should just be a set.
def find_documents(root_directory, location_filename, accounts):
    """Find dated document files under the given directory 'root_directory', located
    only in directories that correspond to one of the given accounts.

    Args:
      root_directory: the name of the root of the directory hierarchy to be searched.
      location_filename: the name of the file to be used for the Document directives.
      accounts: a set of valid accounts strings to search for.
    Returns:
      A list of new Document objects that were created from the files found.
    """
    document_entries = []

    root_directory = path.abspath(root_directory)
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
                    # logging.warn(("Skipping document '{}' because no corresponding "
                    #               "account.").format(path.join(root, filename)))
                    continue
                account = account_name
            else:
                # Try to find a corresponding account. If this is in a parent
                # account, just create the account.
                account = account_name

            # Found one! Create a new directive.
            fileloc = FileLocation(location_filename, -1)
            date = datetime.date(*map(int, mo.group(1, 2, 3)))
            entry = Document(fileloc, date, account, path.join(root, filename))
            document_entries.append(entry)

    return document_entries


def walk_accounts(root_directory):
    """A version of os.walk() which yields directories that are valid account names.

    This only yields directories that are accounts... it skips the other ones.
    For convenience, it also yields you the account's name.

    Args:
      root_directory: the name of the root of the hierarchy to be walked.
    Returns:
      A generator that walks over (root, account-name, dirs, files).
    """
    for root, dirs, files in os.walk(root_directory):
        relroot = root[len(root_directory)+1:]
        account_name = relroot.replace(os.sep, account.sep)
        if account_types.is_valid_account_name(account_name):
            yield (root, account_name, dirs, files)
