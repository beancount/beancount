"""Debugging tool for those finding bugs in Beancount.

This tool is able to dump lexer/parser state, and will provide other services in
the name of debugging.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import click
import collections
import os
import re
import sys
import logging
from os import path

# Note: Because of the presence of beancount.scripts.deps, we have to operate
# under the assumption that not all third-party dependencies are installed.
# Import what you need as late as possible.
from beancount.utils import misc_utils
from beancount.utils import version
from beancount.core import display_context


class FileLocation(click.ParamType):
    name = "location"

    def convert(self, value, param, ctx):
        match = re.match(r"(.+):(\d+)$", value)
        if match:
            filename = path.abspath(match.group(1))
            lineno = int(match.group(2))
        elif re.match(r"(\d+)$", value):
            filename = None
            lineno = int(value)
        else:
            self.fail("{!r} is not a valid location".format(value), param, ctx)
        return filename, lineno


class Group(click.Group):
    def command(self, *args, alias=None, **kwargs):
        # allow to specify an alias for the command name
        def decorator(f):
            cmd = click.command(*args, **kwargs)(f)
            self.add_command(cmd)
            if alias:
                kwargs.update(name=alias, hidden=True)
                cmd = click.command(*args, **kwargs)(f)
                self.add_command(cmd)
            return cmd
        return decorator

    def get_command(self, ctx, name):
        # allow to use '_' or '-' in command names
        name = name.replace('_', '-')
        return self.commands.get(name)


@click.command(cls=Group)
def doctor():
    pass


@doctor.command(alias='dump-lexer')
@click.argument('filename', type=click.Path())
def lex(filename):
    """Dump the lexer output for a Beancount syntax file."""

    from beancount.parser import lexer
    for token, lineno, text, obj in lexer.lex_iter(filename):
        sys.stdout.write('{:12} {:6d} {}\n'.format(
            '(None)' if token is None else token, lineno, repr(text)))


@doctor.command()
@click.argument('filename', type=click.Path())
def parse(filename):
    """Parse the a ledger in debug mode.

    Run the parser on ledger FILENAME with debug mode active.

    """
    from beancount.parser import parser
    entries, errors, _ = parser.parse_file(filename, yydebug=1)


@doctor.command()
@click.argument('filename', type=click.Path())
def roundtrip(filename):
    """Round-trip test on arbitrary ledger.

    Read transactions from ledger FILENAME, print them out, re-read
    them again and compare them. Both sets of parsed entries should be
    equal.  Both printed files are output to disk, so you can also run
    diff on them yourself afterwards.

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


@doctor.command()
@click.argument('filename', type=click.Path())
@click.argument('dirs', type=click.Path(file_okay=False), nargs=-1)
def directories(filename, dirs):
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
    directories.validate_directories(entries, dirs)


@doctor.command()
def list_options():
    """Print a list of the available options."""

    from beancount.parser import options
    print(options.list_options())


@doctor.command()
@click.argument('filename', type=click.Path())
def print_options(filename):
    """Print the actual options parsed from a ledger."""

    from beancount import loader
    _, __, options_map = loader.load_file(filename)
    for key, value in sorted(options_map.items()):
        print('{}: {}'.format(key, value))


@doctor.command(alias='checkdeps')
def deps():
    """Report on the runtime dependencies."""

    from beancount.scripts import deps
    deps.list_dependencies(sys.stdout)
    print('')
    print('Use "pip3 install <package>" to install new packages.')


@doctor.command()
@click.argument('filename', type=click.Path())
@click.argument('location', type=FileLocation())
def context(filename, location):
    """Describe the context that a transaction is applied to.

    The transaction is looked up in ledger FILENAME at LOCATION. The
    LOCATION argument is either a line number or a filename:lineno
    combination in the case the transaction is not in the is not in
    the top-level file.

    """
    from beancount.reports import context
    from beancount import loader

    search_filename, lineno = location
    if search_filename is None:
        search_filename = filename

    # Load the input files.
    entries, errors, options_map = loader.load_file(filename)

    str_context = context.render_file_context(entries, options_map,
                                              search_filename, lineno)
    sys.stdout.write(str_context)


RenderError = collections.namedtuple('RenderError', 'source message entry')


@doctor.command()
@click.argument('filename', type=click.Path())
@click.argument('location', type=FileLocation())
def linked(filename, location):
    """Print a list of linked transactions.

    Searches for all transaction in ledger FILENAME linked to the one
    at the given LOCATION. The LOCATION argument is either a line
    number or a filename:lineno combination in the case the
    transaction is not in the is not in the top-level file.

    """
    from beancount.parser import options
    from beancount.parser import printer
    from beancount.core import account_types
    from beancount.core import inventory
    from beancount.core import data
    from beancount.core import realization
    from beancount import loader

    search_filename, lineno = location
    if search_filename is None:
        search_filename = filename

    # Load the input file.
    entries, errors, options_map = loader.load_file(filename)

    # Find the closest entry.
    closest_entry = data.find_closest(entries, search_filename, lineno)

    # Find its links.
    if closest_entry is None:
        raise SystemExit("No entry could be found before {}:{}".format(search_filename, lineno))
    links = (closest_entry.links
             if isinstance(closest_entry, data.Transaction)
             else data.EMPTY_SET)
    if not links:
        linked_entries = [closest_entry]
    else:
        # Find all linked entries.
        #
        # Note that there is an option here: You can either just look at the links
        # on the closest entry, or you can include the links of the linked
        # transactions as well. Whichever one you want depends on how you use your
        # links. Best would be to query the user (in Emacs) when there are many
        # links present.
        follow_links = True
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

    # Render linked entries (in date order) as errors (for Emacs).
    errors = [RenderError(entry.meta, '', entry)
              for entry in linked_entries]
    printer.print_errors(errors)

    # Print out balances.
    real_root = realization.realize(linked_entries)
    dformat = options_map['dcontext'].build(alignment=display_context.Align.DOT,
                                            reserved=2)
    realization.dump_balances(real_root, dformat, file=sys.stdout)

    # Print out net income change.
    acctypes = options.get_account_types(options_map)
    net_income = inventory.Inventory()
    for real_node in realization.iter_children(real_root):
        if account_types.is_income_statement_account(real_node.account, acctypes):
            net_income.add_inventory(real_node.balance)

    print()
    print('Net Income: {}'.format(-net_income))


@doctor.command()
@click.argument('filename', type=click.Path())
def missing_open(filename):
    """Print Open directives missing in FILENAME.

    This can be useful during demos in order to quickly generate all the
    required Open directives without having to type them manually.

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


@doctor.command()
@click.argument('filename', type=click.Path())
def display_context(filename):
    """Print the precision inferred from the parsed numbers in the input file."""

    from beancount import loader
    entries, errors, options_map = loader.load_file(filename)
    dcontext = options_map['dcontext']
    sys.stdout.write(str(dcontext))


@doctor.command()
@click.argument('directory', type=click.Path(file_okay=False))
def validate_html(directory, args):
    """Validate all the HTML files in DIRECTORY."""

    from beancount.web import scrape
    files, missing, empty = scrape.validate_local_links_in_dir(directory)
    logging.info('%d files processed', len(files))
    for target in missing:
        logging.error('Missing %s', target)
    for target in empty:
        logging.error('Empty %s', target)


main = doctor()

if __name__ == '__main__':
    main()
