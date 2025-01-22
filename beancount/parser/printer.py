"""Conversion from internal data structures to text."""

from __future__ import annotations

__copyright__ = "Copyright (C) 2014-2025  Martin Blais"
__license__ = "GNU GPLv2"

import codecs
import datetime
import enum
import io
import re
import sys
import textwrap
from decimal import Decimal
from typing import Optional

from beancount.core import account
from beancount.core import amount
from beancount.core import convert
from beancount.core import data
from beancount.core import display_context
from beancount.core import interpolate
from beancount.core import inventory
from beancount.core import position
from beancount.utils import misc_utils


def align_position_strings(strings):
    """A helper used to align rendered amounts positions to their first currency
    character (an uppercase letter). This class accepts a list of rendered
    positions and calculates the necessary width to render them stacked in a
    column so that the first currency word aligns. It does not go beyond that
    (further currencies, e.g. for the price or cost, are not aligned).

    This is perhaps best explained with an example. The following positions will
    be aligned around the column marked with '^':

              45 HOOL {504.30 USD}
               4 HOOL {504.30 USD, 2014-11-11}
            9.95 USD
       -22473.32 CAD @ 1.10 USD
                 ^

    Strings without a currency character will be rendered flush left.

    Args:
      strings: A list of rendered position or amount strings.
    Returns:
      A pair of a list of aligned strings and the width of the aligned strings.
    """
    # Maximum length before the alignment character.
    max_before = 0
    # Maximum length after the alignment character.
    max_after = 0
    # Maximum length of unknown strings.
    max_unknown = 0

    string_items = []
    search = re.compile("[A-Z]").search
    for string in strings:
        match = search(string)
        if match:
            index = match.start()
            if index != 0:
                max_before = max(index, max_before)
                max_after = max(len(string) - index, max_after)
                string_items.append((index, string))
                continue
        # else
        max_unknown = max(len(string), max_unknown)
        string_items.append((None, string))

    # Compute formatting string.
    max_total = max(max_before + max_after, max_unknown)
    max_after_prime = max_total - max_before
    fmt = "{{:>{0}}}{{:{1}}}".format(max_before, max_after_prime).format
    fmt_unknown = "{{:<{0}}}".format(max_total).format

    # Align the strings and return them.

    aligned_strings = []
    for index, string in string_items:
        if index is not None:
            string = fmt(string[:index], string[index:])
        else:
            string = fmt_unknown(string)
        aligned_strings.append(string)

    return aligned_strings, max_total


class EntryPrinter:
    """A multi-method interface for printing all directive types.

    Attributes:
      dcontext: An instance of DisplayContext with which to render all the numbers.
      render_weight: A boolean, true if we should render the weight of the postings
        as a comment, for debugging.
      min_width_account: An integer, the minimum width to leave for the account name.
      prefix: User-specific prefix for custom indentation (for Fava).
      stringify_invalid_types: If a metadata value is invalid, force a conversion to
        string for printout.
    """

    def __init__(
        self,
        dcontext=None,
        render_weight=False,
        min_width_account=None,
        prefix=None,
        stringify_invalid_types=False,
        write_source=False,
    ):
        self.dcontext = dcontext or display_context.DEFAULT_DISPLAY_CONTEXT
        self.dformat = self.dcontext.build(precision=display_context.Precision.MOST_COMMON)
        self.dformat_max = self.dcontext.build(precision=display_context.Precision.MAXIMUM)
        self.render_weight = render_weight
        self.min_width_account = min_width_account
        self.prefix = prefix or "  "
        self.stringify_invalid_types = stringify_invalid_types
        self.write_source = write_source

    def __call__(self, obj):
        """Render a directive.

        Args:
          obj: The directive to be rendered.
        Returns:
          A string, the rendered directive.
        """
        oss = io.StringIO()

        # We write optional entry source for every entry type, hence writing it here
        self.write_entry_source(obj.meta, oss, prefix="")

        method = getattr(self, obj.__class__.__name__)
        method(obj, oss)
        return oss.getvalue()

    META_IGNORE = {"filename", "lineno"}

    def write_metadata(self, meta, oss, prefix=None):
        """Write metadata to the file object, excluding filename and line number.

        Args:
          meta: A dict that contains the metadata for this directive.
          oss: A file object to write to.
        """
        if meta is None:
            return
        if prefix is None:
            prefix = self.prefix

        # Note: meta.items() is assumed stable from 3.7 onwards; we're not sorting
        # on purpose in order to keep the original insertion order in print.
        for key, value in meta.items():
            if key not in self.META_IGNORE and not key.startswith("__"):
                value_str = None
                if isinstance(value, str):
                    value_str = '"{}"'.format(misc_utils.escape_string(value))
                elif isinstance(value, (Decimal, datetime.date, amount.Amount, enum.Enum)):
                    value_str = str(value)
                elif isinstance(value, bool):
                    value_str = "TRUE" if value else "FALSE"
                elif isinstance(value, (dict, inventory.Inventory)):
                    pass  # Ignore dicts, don't print them out.
                elif value is None:
                    value_str = ""  # Render null metadata as empty, on purpose.
                else:
                    if self.stringify_invalid_types:
                        # This is only intended to be used during development,
                        # when debugging for custom values of data types
                        # attached directly and not coming from the parser.
                        value_str = str(value)
                    else:
                        raise ValueError("Unexpected value: '{!r}'".format(value))
                if value_str is not None:
                    oss.write("{}{}: {}\n".format(prefix, key, value_str))

    def write_entry_source(self, meta, oss, prefix=None):
        """Write source file and line number in a format interpretable as a message
        location for Emacs, VSCode or other editors. As this is for
        "debugging" purposes, this information will be commented out by a
        semicolon.

        Args:
          meta: A dict that contains the metadata for this directive.
          oss: A file object to write to.
          prefix: User-specific prefix for custom indentation
        """
        if not self.write_source:
            return

        if prefix is None:
            prefix = self.prefix

        oss.write("{}; source: {}\n".format(prefix, render_source(meta)))

    def Transaction(self, entry, oss):
        # Compute the string for the payee and narration line.
        strings = []
        if entry.payee:
            strings.append('"{}"'.format(misc_utils.escape_string(entry.payee)))
        if entry.narration:
            strings.append('"{}"'.format(misc_utils.escape_string(entry.narration)))
        elif entry.payee:
            # Ensure we append an empty string for narration if we have a payee.
            strings.append('""')

        if entry.tags:
            for tag in sorted(entry.tags):
                strings.append("#{}".format(tag))
        if entry.links:
            for link in sorted(entry.links):
                strings.append("^{}".format(link))

        oss.write(
            "{e.date} {flag} {}\n".format(
                " ".join(strings), e=entry, flag=render_flag(entry.flag)
            )
        )
        self.write_metadata(entry.meta, oss)

        rows = [self.render_posting_strings(posting) for posting in entry.postings]
        strs_account = [row[0] for row in rows]
        width_account = (
            max(len(flag_account) for flag_account in strs_account) if strs_account else 1
        )
        strs_position, width_position = align_position_strings(row[1] for row in rows)
        strs_weight, width_weight = align_position_strings(row[2] for row in rows)

        if self.min_width_account and self.min_width_account > width_account:
            width_account = self.min_width_account

        non_trivial_balance = (
            any(map(interpolate.has_nontrivial_balance, entry.postings))
            if self.render_weight and width_weight > 0
            else False
        )
        if non_trivial_balance:
            for posting, account, position, weight in zip(
                entry.postings, strs_account, strs_position, strs_weight
            ):
                oss.write(
                    f"{self.prefix}{account:{width_account}}  "
                    f"{position:{width_position}}  "
                    f"; {weight:{max(1, width_weight)}}".rstrip()
                    + "\n"
                )
                if posting.meta:
                    self.write_metadata(posting.meta, oss, "    ")
        else:
            for posting, account, position in zip(
                entry.postings, strs_account, strs_position
            ):
                oss.write(
                    f"{self.prefix}{account:{width_account}}  "
                    f"{position:{max(1, width_position)}}".rstrip()
                    + "\n"
                )
                if posting.meta:
                    self.write_metadata(posting.meta, oss, "    ")

    def render_posting_strings(self, posting):
        """This renders the three components of a posting: the account and its optional
        posting flag, the position, and finally, the weight of the position. The
        purpose is to align these in the caller.

        Args:
          posting: An instance of Posting, the posting to render.
        Returns:
          A tuple of
            flag_account: A string, the account name including the flag.
            position_str: A string, the rendered position string.
            weight_str: A string, the rendered weight of the posting.
        """
        # Render a string of the flag and the account.
        flag = "{} ".format(render_flag(posting.flag)) if posting.flag else ""
        flag_account = flag + posting.account

        # Render a string with the amount and cost and optional price, if
        # present. Also render a string with the weight.
        weight_str = ""
        if isinstance(posting.units, amount.Amount):
            position_str = position.to_string(posting, self.dformat)
            # Note: we render weights at maximum precision, for debugging.
            if posting.cost is None or (
                isinstance(posting.cost, position.Cost)
                and isinstance(posting.cost.number, Decimal)
            ):
                weight_str = str(convert.get_weight(posting))
        else:
            position_str = ""

        if posting.price is not None:
            position_str += " @ {}".format(posting.price.to_string(self.dformat_max))

        return flag_account, position_str, weight_str

    def Posting(self, posting, oss):
        # Note: This is to be used when rendering postings directly only. The
        # method rendering a transaction attempts to align the posting strings
        # together.
        flag_account, position_str, weight_str = self.render_posting_strings(posting)
        oss.write(
            "{}{:64} {} ; {}\n".format(
                self.prefix, flag_account, position_str, weight_str
            ).rstrip()
        )
        if posting.meta:
            self.write_metadata(posting.meta, oss, "    ")

    def Balance(self, entry, oss):
        comment = "   ; Diff: {}".format(entry.diff_amount) if entry.diff_amount else ""
        number_str = (
            self.dformat.format(entry.amount.number, entry.amount.currency)
            if isinstance(entry.amount.number, Decimal)
            else str(self.number)
        )

        # Render optional tolerance.
        tolerance = ""
        if entry.tolerance:
            tolerance_fmt = self.dformat.format(entry.tolerance, entry.amount.currency)
            tolerance = "~ {tolerance} ".format(tolerance=tolerance_fmt)

        oss.write(
            (
                "{e.date} balance {e.account:47} {amount} {tolerance}{currency}"
                "{comment}\n"
            ).format(
                e=entry,
                amount=number_str,
                tolerance=tolerance,
                currency=entry.amount.currency,
                comment=comment,
            )
        )
        self.write_metadata(entry.meta, oss)

    def Note(self, entry, oss):
        oss.write('{e.date} note {e.account} "{e.comment}"'.format(e=entry))
        if entry.tags or entry.links:
            oss.write(" ")
            for tag in sorted(entry.tags):
                oss.write("#{}".format(tag))
            for link in sorted(entry.links):
                oss.write("^{}".format(link))
        oss.write("\n")
        self.write_metadata(entry.meta, oss)

    def Document(self, entry, oss):
        oss.write('{e.date} document {e.account} "{e.filename}"'.format(e=entry))
        if entry.tags or entry.links:
            oss.write(" ")
            for tag in sorted(entry.tags):
                oss.write("#{}".format(tag))
            for link in sorted(entry.links):
                oss.write("^{}".format(link))
        oss.write("\n")
        self.write_metadata(entry.meta, oss)

    def Pad(self, entry, oss):
        oss.write("{e.date} pad {e.account} {e.source_account}\n".format(e=entry))
        self.write_metadata(entry.meta, oss)

    def Open(self, entry, oss):
        oss.write(
            "{e.date} open {e.account:47} {currencies} {booking}".format(
                e=entry,
                currencies=",".join(entry.currencies or []),
                booking=(
                    '"{}"'.format(entry.booking.name) if entry.booking is not None else ""
                ),
            ).rstrip()
        )
        oss.write("\n")
        self.write_metadata(entry.meta, oss)

    def Close(self, entry, oss):
        oss.write("{e.date} close {e.account}\n".format(e=entry))
        self.write_metadata(entry.meta, oss)

    def Commodity(self, entry, oss):
        oss.write("{e.date} commodity {e.currency}\n".format(e=entry))
        self.write_metadata(entry.meta, oss)

    def Price(self, entry, oss):
        oss.write(
            "{e.date} price {e.currency:<22} {amount:>22}\n".format(
                e=entry, amount=entry.amount.to_string(self.dformat_max)
            )
        )
        self.write_metadata(entry.meta, oss)

    def Event(self, entry, oss):
        oss.write('{e.date} event "{e.type}" "{e.description}"\n'.format(e=entry))
        self.write_metadata(entry.meta, oss)

    def Query(self, entry, oss):
        oss.write('{e.date} query "{e.name}" "{e.query_string}"\n'.format(e=entry))
        self.write_metadata(entry.meta, oss)

    def Custom(self, entry, oss):
        custom_values = []
        for value, dtype in entry.values:
            if dtype is account.TYPE:
                value = "{}".format(value)
            elif isinstance(value, str):
                value = '"{}"'.format(value)
            elif isinstance(value, Decimal):
                value = str(value)
            elif isinstance(value, datetime.date):
                value = value.isoformat()
            elif isinstance(value, bool):
                value = "TRUE" if value else "FALSE"
            elif isinstance(value, amount.Amount):
                value = value.to_string()
            custom_values.append(value)
        oss.write(
            '{e.date} custom "{e.type}" {}\n'.format(" ".join(custom_values), e=entry)
        )
        self.write_metadata(entry.meta, oss)


def render_flag(inflag: Optional[str]) -> str:
    """Render a flag, which can be None, a symbol of a character to a string."""
    if not inflag:
        return ""
    return inflag


def format_entry(
    entry: data.Directive,
    dcontext: display_context.DisplayContext | None = None,
    render_weights: bool = False,
    prefix: str | None = None,
    write_source: bool = False,
) -> str:
    """Format an entry into a string in the same input syntax the parser accepts.

    Args:
      entry: An entry instance.
      dcontext: An instance of DisplayContext used to format the numbers.
      render_weights: A boolean, true to render the weights for debugging.
      write_source: If true a source file and line number will be written for
        each entry in a format interpretable as a message location for Emacs,
        VSCode or other editors. As this is for
        "debugging" purposes, this information will be commented out by a
        semicolon.
    Returns:
      A string, the formatted entry.
    """
    return EntryPrinter(dcontext, render_weights, prefix=prefix, write_source=write_source)(
        entry
    )


def print_entry(entry, dcontext=None, render_weights=False, file=None, write_source=False):
    """A convenience function that prints a single entry to a file.

    Args:
      entry: A directive entry.
      dcontext: An instance of DisplayContext used to format the numbers.
      render_weights: A boolean, true to render the weights for debugging.
      file: An optional file object to write the entries to.
      write_source: If true a source file and line number will be written for
        each entry in a format interpretable as a message location for Emacs,
        VSCode or other editors. This is usefull for "debugging" purposes,
        especially in a multi-file setup
    """
    # TODO(blais): DO remove this now, it's a huge annoyance not to be able to
    # print in-between other statements.
    output = file or (
        codecs.getwriter("utf-8")(sys.stdout.buffer)
        if hasattr(sys.stdout, "buffer")
        else sys.stdout
    )
    output.write(format_entry(entry, dcontext, render_weights, write_source=write_source))
    output.write("\n")


# TODO(blais): Change this to a function which accepts the same optional
# arguments as the printer object. Isolate the spacer/segmentation algorithm to
# its own function.
def print_entries(
    entries, dcontext=None, render_weights=False, file=None, prefix=None, write_source=False
):
    """A convenience function that prints a list of entries to a file.

    Args:
      entries: A list of directives.
      dcontext: An instance of DisplayContext used to format the numbers.
      render_weights: A boolean, true to render the weights for debugging.
      file: An optional file object to write the entries to.
      prefix: User-specific prefix for custom indentation (for Fava).
      write_source: If true a source file and line number will be written for
        each entry in a format interpretable as a message location for Emacs,
        VSCode or other editors. This is usefull for "debugging" peurposes,
        especially in a multi-file setup
    """
    assert isinstance(entries, list), "Entries is not a list: {}".format(entries)
    output = file or (
        codecs.getwriter("utf-8")(sys.stdout.buffer)
        if hasattr(sys.stdout, "buffer")
        else sys.stdout
    )

    if prefix:
        output.write(prefix)
    previous_type = type(entries[0]) if entries else None
    eprinter = EntryPrinter(dcontext, render_weights, write_source=write_source)
    for entry in entries:
        # Insert a newline between transactions and between blocks of directives
        # of the same type.
        entry_type = type(entry)
        if (
            entry_type in (data.Transaction, data.Commodity)
            or entry_type is not previous_type
            or write_source
        ):
            output.write("\n")
            previous_type = entry_type

        string = eprinter(entry)
        output.write(string)


# TODO(blais): Rename to format_source() to be consistent, better:
# format_location().
def render_source(meta):
    """Render the source for errors in a way that it will be both detected by
    Emacs and align and rendered nicely.

    Args:
      meta: A dict with the metadata.
    Returns:
      A string, rendered to be interpretable as a message location for Emacs or
      other editors.
    """
    return "{}:{}".format(meta["filename"], "{}:".format(meta["lineno"]))


def format_error(error):
    """Given an error objects, return a formatted string for it.

    Args:
      error: a namedtuple objects representing an error. It has to have an
        'entry' attribute that may be either a single directive object or a
        list of directive objects.
    Returns:
      A string, the errors rendered.
    """
    oss = io.StringIO()
    oss.write("{} {}\n".format(render_source(error.source), error.message))
    if error.entry is not None:
        entries = error.entry if isinstance(error.entry, list) else [error.entry]
        error_string = "\n".join(format_entry(entry) for entry in entries)
        oss.write("\n")
        oss.write(textwrap.indent(error_string, "   "))
        oss.write("\n")
    return oss.getvalue()


def print_error(error, file=None):
    """A convenience function that prints a single error to a file.

    Args:
      error: An error object.
      file: An optional file object to write the errors to.
    """
    output = file or sys.stdout
    output.write(format_error(error))
    output.write("\n")


def print_errors(errors, file=None, prefix=None):
    """A convenience function that prints a list of errors to a file.

    Args:
      errors: A list of errors.
      file: An optional file object to write the errors to.
    """
    output = file or sys.stdout
    if prefix:
        output.write(prefix)
    for error in errors:
        output.write(format_error(error))
        output.write("\n")
