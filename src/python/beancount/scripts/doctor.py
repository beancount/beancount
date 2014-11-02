"""Debugging tool for those finding bugs in Beancount.

This tool is able to dump lexer/parser state, and will provide other services in
the name of debugging.
"""
import re
import sys
import argparse
import logging
from os import path

from beancount.parser import parser
from beancount.parser import lexer
from beancount.parser import options
from beancount.parser import printer
from beancount.core import compare
from beancount.core import data
from beancount.core import complete
from beancount import loader
from beancount.utils import misc_utils
from beancount.scripts import directories
from beancount.scripts import checkdeps


def do_dump_lexer(filename, unused_args):
    """Dump the lexer output for a Beancount syntax file.

    Args:
      filename: A string, the Beancount input filename.
    """
    for token, lineno, text, obj in lexer.lex_iter(filename):
        sys.stdout.write('{:12} {:6d} {}\n'.format(token, lineno, repr(text)))


def do_roundtrip(filename, unused_args):
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
    entries_roundtrip, errors, options = parser.parse_file(round1_filename)

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


def do_directories(filename, args):
    """Validate a directory hierarchy against a ledger's account names.

    Read a ledger's list of account names and check that all the capitalized
    subdirectory names under the given roots match the account names.

    Args:
      filename: A string, the Beancount input filename.
      args: The rest of the arguments provided on the command-line, which in this
        case will be interpreted as the names of root directories to validate against
        the accounts in the given ledger.
    """
    entries, _, __ = loader.load(filename)

    directories.validate_directories(entries, args)


def do_list_options(*unused_args):
    """Print out a list of the available options.

    Args:
      unused_args: Ignored.
    """
    print(options.list_options())


def get_commands():
    """Return a list of available commands in this file.

    Returns:
      A list of pairs of (command-name string, docstring).
    """
    commands = []
    for attr_name, attr_value in globals().items():
        match = re.match('do_(.*)', attr_name)
        if match:
            commands.append((match.group(1),
                             misc_utils.first_paragraph(attr_value.__doc__)))
    return commands


def do_checkdeps(*unused_args):
    """Report on the runtime dependencies.

    Args:
      unused_args: Ignored.
    """
    print("Dependencies:")
    for package, version, sufficient in checkdeps.check_dependencies():
        print("  {:16}: {} {}".format(
            package,
            version or 'NOT INSTALLED',
            "(INSUFFICIENT)" if version and not sufficient else ""))


def do_context(filename, args):
    """Describe the context that a particular transaction is applied to.

    Args:
      filename: A string, which consists in the filename.
      args: A tuple of the rest of arguments. We're expecting the first argument
        to be an integer as a string.
    """
    # Parse the arguments, get the line number.
    if len(args) != 1:
        raise SystemExit("Missing line number argument.")
    lineno = int(args[0])

    # Load the input file.
    entries, errors, options = loader.load(filename)

    # Find the closest entry.
    closest_entry = data.find_closest(entries, filename, lineno)
    if closest_entry is None:
        raise SystemExit("No entry could be found before {}:{}".format(filename, lineno))
    source = closest_entry.source
    print("{}:{}:".format(source.filename, source.lineno))

    # Get the entry's accounts and accumulate the balances of these accounts up
    # to the entry.
    balance_before, balance_after = complete.compute_entry_context(entries, closest_entry)

    # Get the list of account sorted by the order in which they appear in the
    # closest entry.
    accounts = sorted(balance_before.keys())
    if isinstance(closest_entry, data.Transaction):
        ordering = {posting.account: index
                    for (index, posting) in enumerate(closest_entry.postings)}
        accounts = sorted(accounts, key=ordering.get)

    # Create a format line for printing the contents of account balances.
    max_account_width = max(map(len, accounts)) if accounts else 1
    position_line = '; {{:1}} {{:{width}}}  {{:>49}}'.format(width=max_account_width)

    # Print the context before.
    print()
    before_hashes = set()
    for account in accounts:
        for position in balance_before[account].get_positions():
            before_hashes.add((account, hash(position)))
            print(position_line.format('', account, str(position)))
        print()

    # Print the entry itself.
    print()
    printer.print_entry(closest_entry)

    # Print the context after.
    print()
    for account in accounts:
        for position in balance_after[account].get_positions():
            changed = (account, hash(position)) not in before_hashes
            print(position_line.format('!' if changed else '', account, str(position)))
        print()


def main():
    commands_doc = ('Available Commands:\n' +
                    '\n'.join('  {:24}: {}'.format(*x) for x in get_commands()))
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawTextHelpFormatter,
                                     epilog=commands_doc)
    parser.add_argument('command', action='store',
                        help="The command to run.")
    parser.add_argument('filename', nargs='?', help='Beancount input filename.')
    parser.add_argument('rest', nargs='*', help='All remaining arguments.')
    opts = parser.parse_args()

    # Run the command.
    try:
        command_name = "do_{}".format(opts.command.replace('-', '_'))
        function = globals()[command_name]
    except KeyError:
        parser.error("Invalid command name: '{}'".format(opts.command))
    else:
        function(opts.filename, opts.rest)


if __name__ == '__main__':
    main()
