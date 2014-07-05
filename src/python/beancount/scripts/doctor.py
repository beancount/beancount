"""Debugging tool for those finding bugs in Beancount.

This tool is able to dump lexer/parser state, and will provide other services in
the name of debugging.
"""
import csv
import sys
import argparse
import logging
from os import path

from beancount.parser import parser
from beancount.parser import lexer
from beancount.parser import options
from beancount.parser import printer
from beancount.core import account_types
from beancount.core import getters
from beancount.core import realization
from beancount.ops import prices
from beancount.core import compare
from beancount import loader


def do_dump_lexer(filename):
    """Dump the lexer output for a Beancount syntax file.

    Args:
      filename: A string, the Beancount input filename.
    """
    for token, lineno, text, obj in lexer.lex_iter(filename):
        sys.stdout.write('{:12} {:6d} {}\n'.format(token, lineno, repr(text)))


def do_list_accounts(filename):
    """Dump the lexer output for a Beancount syntax file.

    Args:
      filename: A string, the Beancount input filename.
    """
    # Load the input file and gather the open/close directives.
    entries, errors, options_map = loader.load(filename)
    open_close = getters.get_account_open_close(entries)

    if not entries:
        return

    # Render to stdout.
    maxlen = max(len(account) for account in open_close)
    sortkey_fun = account_types.get_account_sort_function(
        options.get_account_types(options_map))
    for account, (open, close) in sorted(open_close.items(),
                                         key=lambda entry: sortkey_fun(entry[0])):
        open_date = open.date if open else ''
        close_date = close.date if close else ''
        print('{:{len}}  {}  {}'.format(account, open_date, close_date, len=maxlen))


# FIXME: Move this to a report.
def do_print_trial(filename):
    """Render and print the trial balance for a ledger.

    Args:
      filename: A string, the Beancount input filename.
    """
    # Load the file, realize it, and dump the accounts tree.
    entries, errors, options_map = loader.load(filename)
    printer.print_errors(errors, file=sys.stderr)

    real_accounts = realization.realize(entries)
    dump_str = realization.dump_balances(real_accounts)
    print(dump_str)


# FIXME: Move this to a report.
def do_prices(filename):
    """Render and print a list of the prices in a ledger.

    Args:
      filename: A string, the Beancount input filename.
    """
    # Load the file, realize it, and dump the accounts tree.
    entries, errors, options_map = loader.load(filename)
    printer.print_errors(errors, file=sys.stderr)

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


def do_roundtrip(filename):
    """Round-trip test on arbitrary Ledger.

    Read a Ledger's transactions, print them out, re-read them again and compare
    them. Both sets of parsed entries should be equal. Both printed files are
    output to disk, so you can also run diff on them yourself afterwards.

    Args:
      filename: A string, the Beancount input filename.
    """
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    logging.info("Read the entries")
    entries, errors, options = loader.load(filename)
    printer.print_errors(errors, file=sys.stderr)

    logging.info("Print them out to a file")
    basename, extension = path.splitext(filename)
    round1_filename = ''.join([basename, '.roundtrip1', extension])
    with open(round1_filename, 'w') as outfile:
        printer.print_entries(entries, outfile)

    logging.info("Read the entries from that file")
    # Note that we don't want to run any of the auto-generation here...
    # parse-only, not load.
    entries_roundtrip, errors, options = parser.parse(round1_filename)

    # Print out the list of errors from parsing the results.
    if errors:
        print(',----------------------------------------------------------------------')
        printer.print_errors(errors, file=sys.stdout)
        print(error_text)
        print('`----------------------------------------------------------------------')

    logging.info("Print what you read to yet another file")
    round2_filename = ''.join([basename, '.roundtrip2', extension])
    with open(round2_filename, 'w') as outfile:
        printer.print_entries(entries_roundtrip, outfile)

    logging.info("Compare the original entries with the re-read ones")
    same, missing1, missing2 = compare.compare_entries(entries, entries_roundtrip)
    if same:
        logging.info('Entries are the same. Congratulations.')
    else:
        logging.error('Entries differ!')
        print()
        print('\n\nMissing from original:')
        #printer.print_entries(missing1)
        for entry in entries:
            print(entry)
            print(compare.hash_entry(entry))
            print(printer.format_entry(entry))
            print()

        print('\n\nMissing from round-trip:')
        #printer.print_entries(missing2)
        for entry in missing2:
            print(entry)
            print(compare.hash_entry(entry))
            print(printer.format_entry(entry))
            print()


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
