"""Debugging tool for those finding bugs in Beancount.

This tool is able to dump lexer/parser state, and will provide other services in
the name of debugging.
"""
import sys
import argparse

from beancount.parser.parser import dump_lexer
from beancount.parser.parser import options
from beancount.core.account_types import get_account_sort_function
from beancount.core import getters
from beancount.core import realization
from beancount import loader


def do_dump_lexer(filename):
    """Dump the lexer output for a Beancount syntax file.

    Args:
      filename: A string, the Beancount input filename.
    """
    dump_lexer(filename, sys.stdout)


def do_list_accounts(filename):
    """Dump the lexer output for a Beancount syntax file.

    Args:
      filename: A string, the Beancount input filename.
    """
    # Load the input file and gather the open/close directives.
    entries, errors, options_map = loader.load(filename, quiet=True)
    open_close = getters.get_account_open_close(entries)

    if not entries:
        return

    # Render to stdout.
    maxlen = max(len(account) for account in open_close)
    sortkey_fun = get_account_sort_function(options.get_account_types(options_map))
    for account, (open, close) in sorted(open_close.items(),
                                         key=lambda entry: sortkey_fun(entry[0])):
        open_date = open.date if open else ''
        close_date = close.date if close else ''
        print('{:{len}}  {}  {}'.format(account, open_date, close_date, len=maxlen))


def do_print_trial(filename):
    """Render and print the trial balance for a ledger.

    Args:
      filename: A string, the Beancount input filename.
    """
    # Load the file, realize it, and dump the accounts tree.
    entries, errors, options_map = loader.load(filename, do_print_errors=True)
    real_accounts = realization.realize(entries)
    realization.dump_tree_balances(real_accounts)


def first_doc_sentence(object_):
    """Return the first sentence of a docstring.

    Args:
      object_: An object, that has a docstring attached to it.
    Returns:
      A string with just the first sentence on a single line.
    """
    lines = []
    for line in object_.__doc__.strip().splitlines():
        if not line:
            break
        lines.append(line.rstrip())
    return ' '.join(lines)


def main():
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('filename', help='Beancount input filename.')

    parser.add_argument('--dump-lexer', action='store_true',
                        help=first_doc_sentence(do_dump_lexer))

    parser.add_argument('--list-accounts', action='store_true',
                        help=first_doc_sentence(do_list_accounts))

    parser.add_argument('--print-trial', action='store_true',
                        help=first_doc_sentence(do_print_trial))

    opts = parser.parse_args()

    # Dispatch the debugging commands.
    if opts.dump_lexer:
        do_dump_lexer(opts.filename)
    if opts.list_accounts:
        do_list_accounts(opts.filename)
    if opts.print_trial:
        do_print_trial(opts.filename)


if __name__ == '__main__':
    main()
