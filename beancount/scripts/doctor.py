"""Debugging tool for those finding bugs in Beancount.

This tool is able to dump lexer/parser state, and will provide other services in
the name of debugging.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import os
import re
import sys
import argparse
import logging
from os import path

# Note: Because of the presence of beancount.scripts.deps, we have to operate
# under the assumption that not all third-party dependencies are installed.
# Import what you need as late as possible.
from beancount.utils import misc_utils
from beancount.parser import version
from beancount.core import display_context


# pylint: disable=import-outside-toplevel


def do_lex(filename, unused_args):
    """Dump the lexer output for a Beancount syntax file.

    Args:
      filename: A string, the Beancount input filename.
    """
    from beancount.parser import lexer
    for token, lineno, text, obj in lexer.lex_iter(filename):
        sys.stdout.write('{:12} {:6d} {}\n'.format(
            '(None)' if token is None else token, lineno, repr(text)))

do_dump_lexer = do_lex


def do_parse(filename, unused_args):
    """Run the parser in debug mode.

    Args:
      filename: A string, the Beancount input filename.
    """
    from beancount.parser import parser
    entries, errors, _ = parser.parse_file(filename, yydebug=1)


def do_roundtrip(filename, unused_args):
    """Round-trip test on arbitrary Ledger.

    Read a Ledger's transactions, print them out, re-read them again and compare
    them. Both sets of parsed entries should be equal. Both printed files are
    output to disk, so you can also run diff on them yourself afterwards.

    Args:
      filename: A string, the Beancount input filename.
    """
    from beancount.parser import printer
    from beancount.core import compare
    from beancount import loader

    round1_filename = round2_filename = None
    try:
        logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
        logging.info("Read the entries")
        entries, errors, options_map = loader.load_file(filename)
        printer.print_errors(errors, file=sys.stderr)

        logging.info("Print them out to a file")
        basename, extension = path.splitext(filename)
        round1_filename = ''.join([basename, '.roundtrip1', extension])
        with open(round1_filename, 'w') as outfile:
            printer.print_entries(entries, file=outfile)

        logging.info("Read the entries from that file")

        # Note that we don't want to run any of the auto-generation here, but
        # parsing now returns incomplete objects and we assume idempotence on a
        # file that was output from the printer after having been processed, so
        # it shouldn't add anything new. That is, a processed file printed and
        # resolve when parsed again should contain the same entries, i.e.
        # nothing new should be generated.
        entries_roundtrip, errors, options_map = loader.load_file(round1_filename)

        # Print out the list of errors from parsing the results.
        if errors:
            print(',----------------------------------------------------------------------')
            printer.print_errors(errors, file=sys.stdout)
            print('`----------------------------------------------------------------------')

        logging.info("Print what you read to yet another file")
        round2_filename = ''.join([basename, '.roundtrip2', extension])
        with open(round2_filename, 'w') as outfile:
            printer.print_entries(entries_roundtrip, file=outfile)

        logging.info("Compare the original entries with the re-read ones")
        same, missing1, missing2 = compare.compare_entries(entries, entries_roundtrip)
        if same:
            logging.info('Entries are the same. Congratulations.')
        else:
            logging.error('Entries differ!')
            print()
            print('\n\nMissing from original:')
            for entry in entries:
                print(entry)
                print(compare.hash_entry(entry))
                print(printer.format_entry(entry))
                print()

            print('\n\nMissing from round-trip:')
            for entry in missing2:
                print(entry)
                print(compare.hash_entry(entry))
                print(printer.format_entry(entry))
                print()
    finally:
        for rfilename in (round1_filename, round2_filename):
            if path.exists(rfilename):
                os.remove(rfilename)


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
    from beancount import loader
    from beancount.scripts import directories
    entries, _, __ = loader.load_file(filename)
    directories.validate_directories(entries, args)


def do_list_options(*unused_args):
    """Print out a list of the available options.

    Args:
      unused_args: Ignored.
    """
    from beancount.parser import options
    print(options.list_options())


def do_print_options(filename, *args):
    """Print out the actual options parsed from a file.

    Args:
      unused_args: Ignored.
    """
    from beancount import loader
    _, __, options_map = loader.load_file(filename)
    for key, value in sorted(options_map.items()):
        print('{}: {}'.format(key, value))


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


def do_deps(*unused_args):
    """Report on the runtime dependencies.

    Args:
      unused_args: Ignored.
    """
    from beancount.scripts import deps
    deps.list_dependencies(sys.stdout)
    print('')
    print('Use "pip3 install <package>" to install new packages.')

# Alias old name.
do_checkdeps = do_deps


def do_context(filename, args):
    """Describe the context that a particular transaction is applied to.

    Args:
      filename: A string, which consists in the filename.
      args: A tuple of the rest of arguments. We're expecting the first argument
        to be a string which contains either a lineno integer or a filename:lineno
        combination (which can be used if the location is not in the top-level file).
    """
    from beancount.parser import context
    from beancount import loader

    # Check we have the required number of arguments.
    if len(args) != 1:
        raise SystemExit("Missing line number argument.")

    # Load the input files.
    entries, errors, options_map = loader.load_file(filename)

    # Parse the arguments, get the line number.
    match = re.match(r"(.+):(\d+)$", args[0])
    if match:
        search_filename = path.abspath(match.group(1))
        lineno = int(match.group(2))
    elif re.match(r"(\d+)$", args[0]):
        # Note: Make sure to use the absolute filename used by the parser to
        # resolve the file.
        search_filename = options_map['filename']
        lineno = int(args[0])
    else:
        raise SystemExit("Invalid format for location.")

    str_context = context.render_file_context(entries, options_map,
                                              search_filename, lineno)
    sys.stdout.write(str_context)


RenderError = collections.namedtuple('RenderError', 'source message entry')


def do_linked(filename, args):
    """Print out a list of transactions linked to the one at the given line.

    Args:
      filename: A string, which consists in the filename.
      args: A tuple of the rest of arguments. We're expecting the first argument
        to be a string which contains either a lineno integer or a
        (filename:)?lineno(:lineno)? combination (which can be used if the
        location is not in the top-level file).
    """
    from beancount.core import data
    from beancount.core import prices
    from beancount import loader

    # Parse the arguments, get the line number.
    if len(args) != 1:
        raise SystemExit("Missing line number or link argument.")
    location_spec = args[0]

    # Load the input file.
    entries, errors, options_map = loader.load_file(filename)

    # Accept an explicit link name as the location. Must include the '^'
    # character.
    if re.match(r"\^(.*)$", location_spec):
        search_filename = options_map['filename']
        links = {location_spec[1:]}
        linked_entries = find_linked_entries(entries, links, False)

    elif re.match(r"#(.*)$", location_spec):
        search_filename = options_map['filename']
        tag = location_spec[1:]
        linked_entries = find_tagged_entries(entries, tag)

    else:
        # Parse the argument as a line number or a
        # "<filename>:<lineno>:<lineno>" spec to pull context from, with
        # optional filename and optional last line number.
        #
        # If a filename is not provided, the ledger's top-level filename is used
        # (this is the common case). An explicit filename is used to get context
        # in included files.
        #
        # If a single line number is provided the closest transaction is
        # selected. If an internal of line numbers is provided, the list of all
        # transactions whose first line is inside the interval are selected.
        match = re.match(r"(\d+)(?::(\d+))?$", location_spec)
        if match:
            included_filename = None
            first_line, last_line = match.groups()
        else:
            match = re.match(r"(.+?):(\d+)(?::(\d+))?$", location_spec)
            if match:
                included_filename, first_line, last_line = match.groups()
            else:
                raise SystemExit("Invalid line number or link format for location.")

        search_filename = (path.abspath(included_filename)
                           if included_filename else
                           options_map['filename'])
        lineno = int(first_line)
        if last_line is None:
            # Find the closest entry.
            closest_entry = data.find_closest(entries, search_filename, lineno)

            # Find its links.
            if closest_entry is None:
                raise SystemExit("No entry could be found before {}:{}".format(
                    search_filename, lineno))
            links = (closest_entry.links
                     if isinstance(closest_entry, data.Transaction)
                     else data.EMPTY_SET)
        else:
            # Find all the entries in the interval, following all links.
            last_lineno = int(last_line)
            links = set()
            for entry in data.filter_txns(entries):
                if (entry.meta['filename'] == search_filename and
                    lineno <= entry.meta['lineno'] <= last_lineno):
                    links.update(entry.links)

        # Get the linked entries, or just the closest one, if no links.
        linked_entries = (find_linked_entries(entries, links, True)
                          if links
                          else [closest_entry])

    render_mini_balances(linked_entries, options_map, None)


# TODO(blais): This should be folded as an option when we convert this to click.
def do_region_value(filename, args):
    """Print out a list of transactions in a region and balances at market value.
    """
    return do_region(filename, args, at_value=True)


def do_region(filename, args, at_value=False):
    """Print out a list of transactions in a region and balances.

    Args:
      filename: A string, which consists in the filename.
      args: A tuple of the rest of arguments. We're expecting the first argument
        to be a string which contains either a lineno integer or a
        (filename:)?lineno:lineno combination (which can be used if the location
        is not in the top-level file).
      at_value: A boolean, if true, convert balances output to market value.
    """
    from beancount.core import data
    from beancount.core import prices
    from beancount import loader

    # Parse the arguments, get the line number.
    if len(args) != 1:
        raise SystemExit("Missing line number or link argument.")
    location_spec = args[0]

    # Load the input file.
    entries, errors, options_map = loader.load_file(filename)

    # Parse the argument as a line number or a
    # "<filename>:<lineno>:<lineno>" spec to pull context from, with
    # optional filename and optional last line number.
    #
    # If a filename is not provided, the ledger's top-level filename is used
    # (this is the common case). An explicit filename is used to get context
    # in included files.
    #
    # If a single line number is provided the closest transaction is
    # selected. If an internal of line numbers is provided, the list of all
    # transactions whose first line is inside the interval are selected.
    match = re.match(r"(?:(.+?):)?(\d+):(\d+)$", location_spec)
    if not match:
        raise SystemExit("Invalid line number or link format for region.")

    included_filename, first_line, last_line = match.groups()
    search_filename = (path.abspath(included_filename)
                       if included_filename else
                       options_map['filename'])
    lineno = int(first_line)
    last_lineno = int(last_line)

    # Find all the entries in the region. (To be clear, this isn't like the
    # 'linked' command, none of the links are followed.)
    region_entries = [
        entry
        for entry in data.filter_txns(entries)
        if (entry.meta['filename'] == search_filename and
            lineno <= entry.meta['lineno'] <= last_lineno)]

    price_map = prices.build_price_map(entries) if at_value else None
    render_mini_balances(region_entries, options_map, price_map)


def render_mini_balances(entries, options_map, price_map=None):
    """Render a treeified list of the balances for the given transactions.

    Args:
      entries: A list of selected transactions to render.
      options_map: The parsed options.
      price_map: A price map from the original entries. If this isn't provided,
        the inventories are rendered directly. If it is, their contents are
        converted to market value.
    """
    from beancount.parser import options
    from beancount.parser import printer
    from beancount.core import account_types
    from beancount.core import convert
    from beancount.core import inventory
    from beancount.core import realization

    # Render linked entries (in date order) as errors (for Emacs).
    errors = [RenderError(entry.meta, '', entry)
              for entry in entries]
    printer.print_errors(errors)

    # Print out balances.
    real_root = realization.realize(entries)
    dformat = options_map['dcontext'].build(alignment=display_context.Align.DOT,
                                            reserved=2)

    if price_map is not None:
        # Warning: Mutate the inventories in-place, converting them to market
        # value.
        for real_account in realization.iter_children(real_root):
            real_account.balance = real_account.balance.reduce(
                convert.get_value, price_map)

    realization.dump_balances(real_root, dformat, file=sys.stdout)

    # Print out net income change.
    acctypes = options.get_account_types(options_map)
    net_income = inventory.Inventory()
    for real_node in realization.iter_children(real_root):
        if account_types.is_income_statement_account(real_node.account, acctypes):
            net_income.add_inventory(real_node.balance)

    print()
    print('Net Income: {}'.format(-net_income))


def find_linked_entries(entries, links, follow_links: bool):
    """Find all linked entries.

    Note that there is an option here: You can either just look at the links
    on the closest entry, or you can include the links of the linked
    transactions as well. Whichever one you want depends on how you use your
    links. Best would be to query the user (in Emacs) when there are many
    links present.
    """
    from beancount.core import data

    linked_entries = []
    if not follow_links:
        linked_entries = [entry
                          for entry in entries
                          if (isinstance(entry, data.Transaction) and
                              entry.links and
                              entry.links & links)]
    else:
        links = set(links)
        linked_entries = []
        while True:
            num_linked = len(linked_entries)
            linked_entries = [entry
                              for entry in entries
                              if (isinstance(entry, data.Transaction) and
                                  entry.links and
                                  entry.links & links)]
            if len(linked_entries) == num_linked:
                break
            for entry in linked_entries:
                if entry.links:
                    links.update(entry.links)
    return linked_entries


def find_tagged_entries(entries, tag):
    """Find all entries with the given tag."""
    from beancount.core import data
    return [entry
            for entry in entries
            if (isinstance(entry, data.Transaction) and
                entry.tags and
                tag in entry.tags)]


def do_missing_open(filename, args):
    """Print out Open directives that are missing for the given input file.

    This can be useful during demos in order to quickly generate all the
    required Open directives without having to type them manually.

    Args:
      filename: A string, which consists in the filename.
      args: A tuple of the rest of arguments. We're expecting the first argument
        to be an integer as a string.
    """
    from beancount.parser import printer
    from beancount.core import data
    from beancount.core import getters
    from beancount import loader

    entries, errors, options_map = loader.load_file(filename)

    # Get accounts usage and open directives.
    first_use_map, _ = getters.get_accounts_use_map(entries)
    open_close_map = getters.get_account_open_close(entries)

    new_entries = []
    for account, first_use_date in first_use_map.items():
        if account not in open_close_map:
            new_entries.append(
                data.Open(data.new_metadata(filename, 0), first_use_date, account,
                          None, None))

    dcontext = options_map['dcontext']
    printer.print_entries(data.sorted(new_entries), dcontext)


def do_display_context(filename, args):
    """Print out the precision inferred from the parsed numbers in the input file.

    Args:
      filename: A string, which consists in the filename.
      args: A tuple of the rest of arguments. We're expecting the first argument
        to be an integer as a string.
    """
    from beancount import loader
    entries, errors, options_map = loader.load_file(filename)
    dcontext = options_map['dcontext']
    sys.stdout.write(str(dcontext))


def do_validate_html(directory, args):
    """Validate all the HTML files under a directory hierarchy.

    Args:
      directory: A string, the root directory whose contents to validate.
      args: A tuple of the rest of arguments.
    """
    from beancount.utils import scrape
    files, missing, empty = scrape.validate_local_links_in_dir(directory)
    logging.info('%d files processed', len(files))
    for target in missing:
        logging.error('Missing %s', target)
    for target in empty:
        logging.error('Empty %s', target)


def main():
    commands_doc = ('Available Commands:\n' +
                    '\n'.join('  {:24}: {}'.format(*x) for x in get_commands()))
    argparser = version.ArgumentParser(description=__doc__,
                                       formatter_class=argparse.RawTextHelpFormatter,
                                       epilog=commands_doc)
    argparser.add_argument('command', action='store',
                           help="The command to run.")
    argparser.add_argument('filename', nargs='?', help='Beancount input filename.')
    argparser.add_argument('rest', nargs='*', help='All remaining arguments.')
    opts = argparser.parse_args()

    # Run the command.
    try:
        command_name = "do_{}".format(opts.command.replace('-', '_'))
        function = globals()[command_name]
    except KeyError:
        argparser.error("Invalid command name: '{}'".format(opts.command))
    else:
        function(opts.filename, opts.rest)


if __name__ == '__main__':
    main()
