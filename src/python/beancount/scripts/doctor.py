"""Debugging tool for those finding bugs in Beancount.

This tool is able to dump lexer/parser state, and will provide other services in
the name of debugging.
"""
import csv
import sys
import argparse

from beancount.parser.parser import dump_lexer
from beancount.parser.parser import options
from beancount.core.account_types import get_account_sort_function
from beancount.core import getters
from beancount.core import realization
from beancount.ops import prices
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
    dump_str = realization.dump_balances(real_accounts)
    print(dump_str)


def do_prices(filename):
    """Render and print the trial balance for a ledger.

    Args:
      filename: A string, the Beancount input filename.
    """
    # Load the file, realize it, and dump the accounts tree.
    entries, errors, options_map = loader.load(filename, do_print_errors=True)
    price_map = prices.build_price_map(entries)
    for base_quote in price_map.forward_pairs:
        price_list = price_map[base_quote]
        base, quote = base_quote
        writer = csv.writer(sys.stdout)
        for date, price in price_list:
            writer.writerow((base, quote, date, price))
        writer.writerow(())


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
    # FIXME: Add help for each command
                        # help=first_doc_sentence(do_print_trial))

    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument('command', action='store',
                        help="The command to run.")
    parser.add_argument('filename', help='Beancount input filename.')
    opts = parser.parse_args()

    # Run the command.
    try:
        command_name = "do_{}".format(opts.command.replace('-', '_'))
        function = globals()[command_name]
        function(opts.filename)
    except KeyError:
        parser.error("Invalid command name: '{}'".format(opts.command))


if __name__ == '__main__':
    main()
