"""Debugging tool for those finding bugs in Beancount.

This tool is able to dump lexer/parser state, and will provide other services in
the name of debugging.
"""

__copyright__ = "Copyright (C) 2014-2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import logging
import os
import re
import sys

import click

from beancount import loader
from beancount.core import account
from beancount.core import account_types
from beancount.core import compare
from beancount.core import convert
from beancount.core import data
from beancount.core import getters
from beancount.core import inventory
from beancount.core import prices
from beancount.core import realization
from beancount.core.display_context import Align
from beancount.parser import lexer
from beancount.parser import options
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser.context import render_file_context
from beancount.parser.version import VERSION
from beancount.scripts.directories import validate_directories

ledger_path = click.Path(resolve_path=True, exists=True)


class FileLocation(click.ParamType):
    name = "location"

    def convert(self, value, param, ctx):
        match = re.match(r"(?:(.+):)?(\d+)$", value)
        if not match:
            self.fail("{!r} is not a valid location".format(value), param, ctx)
        filename, lineno = match.groups()
        if filename:
            filename = os.path.abspath(filename)
        return filename, int(lineno)


class FileRegion(click.ParamType):
    name = "region"

    def convert(self, value, param, ctx):
        match = re.match(r"(?:(.+):)?(\d+):(\d+)$", value)
        if not match:
            self.fail("{!r} is not a valid region".format(value), param, ctx)
        filename, start_lineno, end_lineno = match.groups()
        if filename:
            filename = os.path.abspath(filename)
        return filename, int(start_lineno), int(end_lineno)


class Group(click.Group):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.aliases = {}

    def command(self, *args, alias=None, **kwargs):
        wrap = click.Group.command(self, *args, **kwargs)

        def decorator(f):
            cmd = wrap(f)
            if alias:
                self.aliases[alias] = cmd.name
            return cmd

        return decorator

    def get_command(self, ctx, cmd_name):
        # aliases
        name = self.aliases.get(cmd_name, cmd_name)
        # allow to use '_' or '-' in command names.
        name = name.replace("_", "-")
        return click.Group.get_command(self, ctx, name)


@click.command(cls=Group)
@click.version_option(message=VERSION)
def doctor():
    pass


@doctor.command(alias="dump-lexer")
@click.argument("filename", type=ledger_path)
def lex(filename):
    """Dump the lexer output for a Beancount syntax file."""

    for token, lineno, text, obj in lexer.lex_iter(filename):
        sys.stdout.write(
            "{:12} {:6d} {}\n".format(
                "(None)" if token is None else token, lineno, repr(text)
            )
        )


@doctor.command()
@click.argument("filename", type=ledger_path)
def parse(filename):
    """Parse the a ledger in debug mode.

    Run the parser on ledger FILENAME with debug mode active.

    """
    entries, errors, _ = parser.parse_file(filename, debug=True)


@doctor.command()
@click.argument("filename", type=ledger_path)
def roundtrip(filename):
    """Round-trip test on arbitrary ledger.

    Read transactions from ledger FILENAME, print them out, re-read
    them again and compare them. Both sets of parsed entries should be
    equal.  Both printed files are output to disk, so you can also run
    diff on them yourself afterwards.

    """
    round1_filename = round2_filename = None
    try:
        logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")
        logging.info("Read the entries")
        entries, errors, options_map = loader.load_file(filename)
        printer.print_errors(errors, file=sys.stderr)

        logging.info("Print them out to a file")
        basename, extension = os.path.splitext(filename)
        round1_filename = "".join([basename, ".roundtrip1", extension])
        with open(round1_filename, "w", encoding="utf-8") as outfile:
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
            print(",----------------------------------------------------------------------")
            printer.print_errors(errors, file=sys.stdout)
            print("`----------------------------------------------------------------------")

        logging.info("Print what you read to yet another file")
        round2_filename = "".join([basename, ".roundtrip2", extension])
        with open(round2_filename, "w", encoding="utf-8") as outfile:
            printer.print_entries(entries_roundtrip, file=outfile)

        logging.info("Compare the original entries with the re-read ones")
        same, missing1, missing2 = compare.compare_entries(entries, entries_roundtrip)
        if same:
            logging.info("Entries are the same. Congratulations.")
        else:
            logging.error("Entries differ!")
            print()
            print("\n\nMissing from original:")
            for entry in entries:
                print(entry)
                print(compare.hash_entry(entry))
                print(printer.format_entry(entry))
                print()

            print("\n\nMissing from round-trip:")
            for entry in missing2:
                print(entry)
                print(compare.hash_entry(entry))
                print(printer.format_entry(entry))
                print()
    finally:
        for rfilename in (round1_filename, round2_filename):
            if os.path.exists(rfilename):
                os.remove(rfilename)


@doctor.command()
@click.argument("filename", type=ledger_path)
@click.argument(
    "dirs", type=click.Path(resolve_path=True, exists=True, file_okay=False), nargs=-1
)
def directories(filename, dirs):
    """Validate a directory hierarchy against the ledger's account names.

    Read a ledger's list of account names and check that all the capitalized
    subdirectory names under the given roots match the account names.

    Args:
      filename: A string, the Beancount input filename.
      args: The rest of the arguments provided on the command-line, which in this
        case will be interpreted as the names of root directories to validate against
        the accounts in the given ledger.
    """
    entries, _, __ = loader.load_file(filename)
    validate_directories(entries, dirs)


@doctor.command()
def list_options():
    """List available options."""
    print(options.list_options())


@doctor.command()
@click.argument("filename", type=ledger_path)
def print_options(filename):
    """List options parsed from a ledger."""
    _, __, options_map = loader.load_file(filename)
    for key, value in sorted(options_map.items()):
        print("{}: {}".format(key, value))


@doctor.command()
@click.argument("filename", type=ledger_path)
@click.argument("location", type=FileLocation())
def context(filename, location):
    """Describe transaction context.

    The transaction is looked up in ledger FILENAME at LOCATION. The
    LOCATION argument is either a line number or a filename:lineno
    tuple to indicate a location in a ledger included from the main
    input file.

    """
    search_filename, lineno = location
    if search_filename is None:
        search_filename = filename

    # Load the input files.
    entries, errors, options_map = loader.load_file(filename)

    str_context = render_file_context(entries, options_map, search_filename, lineno)
    sys.stdout.write(str_context)


RenderError = collections.namedtuple("RenderError", "source message entry")


@doctor.command()
@click.argument("filename", type=ledger_path)
@click.argument("location_spec", metavar="[LINK|TAG|LOCATION|REGION]")
def linked(filename, location_spec):
    """List related transactions.

    List all transaction in ledger FILENAME linked to LINK or tagged
    with TAG, or linked to the one at LOCATION, or linked to any
    transaction in REGION.

    The LINK and TAG arguments must include the leading ^ or #
    charaters. The LOCATION argument is either a line number or a
    filename:lineno tuple to indicate a location in a ledger file
    included from the main input file. The REGION argument is either a
    stard:end line numbers tuple or a filename:start:end triplet to
    indicate a region in a ledger file included from the main input
    file.

    """
    # Load the input file.
    entries, errors, options_map = loader.load_file(filename)

    # Link name.
    if re.match(r"\^(.*)$", location_spec):
        search_filename = options_map["filename"]
        links = {location_spec[1:]}
        linked_entries = find_linked_entries(entries, links, False)

    # Tag name.
    elif re.match(r"#(.*)$", location_spec):
        search_filename = options_map["filename"]
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

        search_filename = (
            os.path.abspath(included_filename)
            if included_filename
            else options_map["filename"]
        )
        lineno = int(first_line)
        if last_line is None:
            # Find the closest entry.
            closest_entry = data.find_closest(entries, search_filename, lineno)
            selected_entries = [closest_entry]

            # Find its links.
            if closest_entry is None:
                raise SystemExit(
                    "No entry could be found before {}:{}".format(search_filename, lineno)
                )
            links = (
                closest_entry.links
                if isinstance(closest_entry, data.Transaction)
                else data.EMPTY_SET
            )
        else:
            # Find all the entries in the interval, following all links.
            last_lineno = int(last_line)
            links = set()
            selected_entries = []
            for entry in data.filter_txns(entries):
                if (
                    entry.meta["filename"] == search_filename
                    and lineno <= entry.meta["lineno"] <= last_lineno
                ):
                    links.update(entry.links)
                    selected_entries.append(entry)

        # Get the linked entries, or just the closest one, if no links.
        linked_entries = (
            find_linked_entries(entries, links, True) if links else selected_entries
        )

    render_mini_balances(linked_entries, options_map, None)


def resolve_region_to_entries(
    entries: list[data.Entries], filename: str, region: tuple[str, int, int]
) -> list[data.Entries]:
    """Resolve a filename and region to a list of entries."""

    search_filename, first_lineno, last_lineno = region
    if search_filename is None:
        search_filename = filename

    # Find all the entries in the region. (To be clear, this isn't like the
    # 'linked' command, none of the links are followed.)
    region_entries = [
        entry
        for entry in data.filter_txns(entries)
        if (
            entry.meta["filename"] == search_filename
            and first_lineno <= entry.meta["lineno"] <= last_lineno
        )
    ]

    return region_entries


@doctor.command()
@click.argument("filename", type=ledger_path)
@click.argument("region", type=FileRegion())
@click.option(
    "--conversion",
    type=click.Choice(["value", "cost"]),
    help="Convert balances output to market value or cost.",
)
def region(filename, region, conversion):
    """Print out a list of transactions within REGION and compute balances.

    The REGION argument is either a stard:end line numbers tuple or a
    filename:start:end triplet to indicate a region in a ledger file
    included from the main input file.

    """
    entries, errors, options_map = loader.load_file(filename)
    region_entries = resolve_region_to_entries(entries, filename, region)
    price_map = prices.build_price_map(entries) if conversion == "value" else None
    render_mini_balances(region_entries, options_map, conversion, price_map)


def render_mini_balances(entries, options_map, conversion=None, price_map=None):
    """Render a treeified list of the balances for the given transactions.

    Args:
      entries: A list of selected transactions to render.
      options_map: The parsed options.
      conversion: Conversion method string, None, 'value' or 'cost'.
      price_map: A price map from the original entries. If this isn't provided,
        the inventories are rendered directly. If it is, their contents are
        converted to market value.
    """
    # Render linked entries (in date order) as errors (for Emacs).
    errors = [RenderError(entry.meta, "", entry) for entry in entries]
    printer.print_errors(errors)

    # Print out balances.
    real_root = realization.realize(entries)
    dformat = options_map["dcontext"].build(alignment=Align.DOT, reserved=2)

    # TODO(blais): I always want to be able to convert at cost. We need
    # arguments capability.
    #
    # TODO(blais): Ideally this conversion inserts a new transactions to
    # 'Unrealized' to account for the difference between cost and market value.
    # Insert one and update the realization. Add an update() method to the
    # realization, given a transaction.
    acctypes = options.get_account_types(options_map)
    if conversion == "value":
        assert price_map is not None

        # Warning: Mutate the inventories in-place, converting them to market
        # value.
        balance_diff = inventory.Inventory()
        for real_account in realization.iter_children(real_root):
            balance_cost = real_account.balance.reduce(convert.get_cost)
            balance_value = real_account.balance.reduce(convert.get_value, price_map)
            real_account.balance = balance_value
            balance_diff.add_inventory(balance_cost)
            balance_diff.add_inventory(-balance_value)
        if not balance_diff.is_empty():
            account_unrealized = account.join(
                acctypes.income, options_map["account_unrealized_gains"]
            )
            unrealized = realization.get_or_create(real_root, account_unrealized)
            unrealized.balance.add_inventory(balance_diff)

    elif conversion == "cost":
        for real_account in realization.iter_children(real_root):
            real_account.balance = real_account.balance.reduce(convert.get_cost)

    realization.dump_balances(real_root, dformat, file=sys.stdout)

    # Print out net income change.
    net_income = inventory.Inventory()
    for real_node in realization.iter_children(real_root):
        if account_types.is_income_statement_account(real_node.account, acctypes):
            net_income.add_inventory(real_node.balance)

    print()
    print("Net Income: {}".format(-net_income))


def find_linked_entries(entries, links, follow_links: bool):
    """Find all linked entries.

    Note that there is an option here: You can either just look at the links
    on the closest entry, or you can include the links of the linked
    transactions as well. Whichever one you want depends on how you use your
    links. Best would be to query the user (in Emacs) when there are many
    links present.
    """
    linked_entries = []
    if not follow_links:
        linked_entries = [
            entry
            for entry in entries
            if (isinstance(entry, data.Transaction) and entry.links and entry.links & links)
        ]
    else:
        links = set(links)
        linked_entries = []
        while True:
            num_linked = len(linked_entries)
            linked_entries = [
                entry
                for entry in entries
                if (
                    isinstance(entry, data.Transaction)
                    and entry.links
                    and entry.links & links
                )
            ]
            if len(linked_entries) == num_linked:
                break
            for entry in linked_entries:
                if entry.links:
                    links.update(entry.links)
    return linked_entries


def find_tagged_entries(entries, tag):
    """Find all entries with the given tag."""
    return [
        entry
        for entry in entries
        if (isinstance(entry, data.Transaction) and entry.tags and tag in entry.tags)
    ]


@doctor.command()
@click.argument("filename", type=ledger_path)
def missing_open(filename):
    """Print Open directives missing in FILENAME.

    This can be useful during demos in order to quickly generate all the
    required Open directives without having to type them manually.

    """
    entries, errors, options_map = loader.load_file(filename)

    # Get accounts usage and open directives.
    first_use_map, _ = getters.get_accounts_use_map(entries)
    open_close_map = getters.get_account_open_close(entries)

    new_entries = []
    for account_, first_use_date in first_use_map.items():
        if account_ not in open_close_map:
            new_entries.append(
                data.Open(
                    data.new_metadata(filename, 0), first_use_date, account_, None, None
                )
            )

    dcontext = options_map["dcontext"]
    printer.print_entries(data.sorted(new_entries), dcontext)


@doctor.command()
@click.argument("filename", type=ledger_path)
def display_context(filename):
    """Print the precision inferred from the parsed numbers in the input file."""
    entries, errors, options_map = loader.load_file(filename)
    dcontext = options_map["dcontext"]
    sys.stdout.write(str(dcontext))


main = doctor

if __name__ == "__main__":
    main()
