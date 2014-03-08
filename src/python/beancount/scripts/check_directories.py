"""Check that the auto-doc directories matches a list of accounts.
"""
from os import path
import argparse

from beancount import load
from beancount.parser import documents
from beancount.core import data


def main():
    parser = argparse.ArgumentParser(__doc__.strip())
    parser.add_argument('filename', help='Beancount input filename.')
    opts = parser.parse_args()

    # Load up the file, print errors.
    entries, errors, options = load(opts.filename, do_print_errors=True)

    root = path.dirname(opts.filename)
    document_dirs = [dn if path.isabs(dn) else path.join(root, dn)
                     for dn in options['documents']]

    accounts = data.get_accounts(entries)
    for document_dir in document_dirs:
        validate_directories(accounts, document_dir)


def validate_directories(accounts, document_dir):

# FIXME: You need to realize here and use the tree to validate the hierarchy.
# This is unfinished but is beancount material.

    for root, account_name, dirs, files in documents.walk_accounts(document_dir):
        print(account_name)






if __name__ == '__main__':
    main()
