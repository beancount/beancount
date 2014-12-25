"""Conversion from internal data structures to text.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import io
import re
import sys
import textwrap

from beancount.core import interpolate
from beancount.core import data
from beancount.core import display_context


def align_position_strings(strings):
    """A helper used to align rendered amounts positions to their first currency
    character (an uppercase letter). This class accepts a list of rendered
    positions and calculates the necessary width to render them stacked in a
    column so that the first currency word aligns. It does not go beyond that
    (further currencies, e.g. for the price or cost, are not aligned).

    This is perhaps best explained with an example. The following positions will
    be aligned around the column marked with '^':

              45 GOOG {504.30 USD}
               4 GOOG {504.30 USD / 2014-11-11}
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
    # Maxmimum length of unknown strings.
    max_unknown = 0

    string_items = []
    search = re.compile('[A-Z]').search
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

    Atributes:
      dcontext: An instance of DisplayContext with which to render all the numbers.
      render_weight: A boolean, true if we should render the weight of the postings
        as a comment, for debugging.
      min_width_account: An integer, the minimum width to leave for the account name.
    """

    # pylint: disable=invalid-name

    def __init__(self, dcontext=None, render_weight=False, min_width_account=None):
        self.dcontext = dcontext or display_context.DEFAULT_DISPLAY_CONTEXT
        self.render_weight = render_weight
        self.min_width_account = min_width_account

    def __call__(self, obj):
        """Render a directive.

        Args:
          obj: The directive to be rendered.
        Returns:
          A string, the rendered directive.
        """
        oss = io.StringIO()
        method = getattr(self, obj.__class__.__name__)
        method(obj, oss)
        return oss.getvalue()

    def Transaction(self, entry, oss):
        # Compute the string for the payee and narration line.
        strings = []
        if entry.payee:
            strings.append('"{}" |'.format(entry.payee))
        if entry.narration:
            strings.append('"{}"'.format(entry.narration))
        elif entry.payee:
            # Ensure we append an empty string for narration if we have a payee.
            strings.append('""')

        if entry.tags:
            for tag in sorted(entry.tags):
                strings.append('#{}'.format(tag))
        if entry.links:
            for link in sorted(entry.links):
                strings.append('^{}'.format(link))

        oss.write('{e.date} {e.flag} {}\n'.format(' '.join(strings), e=entry))

        rows = [self.render_posting_strings(posting)
                for posting in entry.postings]
        strs_account = [row[0] for row in rows]
        width_account = (max(len(flag_account) for flag_account in strs_account)
                         if strs_account
                         else 1)
        strs_position, width_position = align_position_strings(row[1] for row in rows)
        strs_weight, width_weight = align_position_strings(row[2] for row in rows)

        if self.min_width_account and self.min_width_account > width_account:
            width_account = self.min_width_account

        non_trivial_balance = (any(map(interpolate.has_nontrivial_balance, entry.postings))
                               if self.render_weight
                               else False)
        if non_trivial_balance:
            fmt = "  {{:{0}}}  {{:{1}}}  ; {{:{2}}}\n".format(
                width_account, width_position, width_weight).format
            for account, position_str, weight_str in zip(strs_account,
                                                         strs_position,
                                                         strs_weight):
                oss.write(fmt(account,
                              position_str,
                              weight_str if non_trivial_balance else ''))
        else:
            fmt = "  {{:{0}}}  {{:{1}}}\n".format(width_account, width_position).format
            for account, position_str in zip(strs_account, strs_position):
                oss.write(fmt(account, position_str))

    def render_posting_strings(self, posting):
        # This renders the three components of a posting: the account and its
        # optional posting flag, the position, and finally, the weight of the
        # position.

        # Render a string of the flag and hte account.
        flag = '{} '.format(posting.flag) if posting.flag else ''
        flag_account = flag + posting.account

        # Render a string with the amount and cost and optional price, if
        # present. Also render a string with the weight.
        if posting.position:
            position_str = posting.position.to_string(self.dcontext)
            weight_str = interpolate.get_posting_weight(posting).to_string(self.dcontext)
        else:
            position_str = ''
            weight_str = ''

        if posting.price is not None:
            position_str += ' @ {}'.format(posting.price.to_string(self.dcontext))

        return flag_account, position_str, weight_str

    def Posting(self, posting, oss):
        # Note: This is to be used when rendering postings directly only. The
        # method rendering a transaction attempts to align the posting strings
        # together.
        flag_account, position_str, weight_str = self.render_posting_strings(posting)
        oss.write('  {:64} {} {}\n'.format(flag_account, position_str, weight_str).rstrip())

    def Balance(_, entry, oss):
        comment = '   ; Diff: {}'.format(entry.diff_amount) if entry.diff_amount else ''
        oss.write(('{e.date} balance {e.account:47} {amount:>22}'
                   '{comment}\n').format(e=entry,
                                         comment=comment,
                                         amount=str(entry.amount)))

    def Note(_, entry, oss):
        oss.write('{e.date} note {e.account} "{e.comment}"\n'.format(e=entry))

    def Document(_, entry, oss):
        oss.write('{e.date} document {e.account} "{e.filename}"\n'.format(e=entry))

    def Pad(_, entry, oss):
        oss.write('{e.date} pad {e.account} {e.source_account}\n'.format(e=entry))

    def Open(_, entry, oss):
        oss.write('{e.date} open {e.account:47} {currencies}\n'.format(
            e=entry, currencies=','.join(entry.currencies or [])))

    def Close(_, entry, oss):
        oss.write('{e.date} close {e.account}\n'.format(e=entry))

    def Price(_, entry, oss):
        oss.write('{e.date} price {e.currency:<22} {amount:>22}\n'.format(
            e=entry, amount=str(entry.amount)))

    def Event(_, entry, oss):
        oss.write('{e.date} event "{e.type}" "{e.description}"\n'.format(e=entry))


def format_entry(entry, dcontext=None, render_weights=False):
    """Format an entry into a string in the same input syntax the parser accepts.

    Args:
      entry: An entry instance.
      dcontext: An instance of DisplayContext used to format the numbers.
      render_weights: A boolean, true to render the weights for debugging.
    Returns:
      A string, the formatted entry.
    """
    return EntryPrinter(dcontext, render_weights)(entry)


def print_entry(entry, dcontext=None, render_weights=False, file=None):
    """A convenience function that prints a single entry to a file.

    Args:
      entry: A directive entry.
      dcontext: An instance of DisplayContext used to format the numbers.
      render_weights: A boolean, true to render the weights for debugging.
      file: An optional file object to write the entries to.
    """
    output = file or sys.stdout
    output.write(format_entry(entry, dcontext, render_weights))
    output.write('\n')


def print_entries(entries, dcontext=None, render_weights=False, file=None, prefix=None):
    """A convenience function that prints a list of entries to a file.

    Args:
      entries: A list of directives.
      dcontext: An instance of DisplayContext used to format the numbers.
      render_weights: A boolean, true to render the weights for debugging.
      file: An optional file object to write the entries to.
    """
    assert isinstance(entries, list)
    output = file or sys.stdout
    if prefix:
        output.write(prefix)
    previous_type = type(entries[0]) if entries else None
    eprinter = EntryPrinter(dcontext, render_weights)
    for entry in entries:
        # Insert a newline between transactions and between blocks of directives
        # of the same type.
        entry_type = type(entry)
        if entry_type is data.Transaction or entry_type is not previous_type:
            output.write('\n')
            previous_type = entry_type

        output.write(eprinter(entry))


def render_source(meta):
    """Render the source for errors in a way that it will be both detected by
    Emacs and align and rendered nicely.

    Args:
      meta: an instance of AttrDict.
    Returns:
      A string, rendered to be interpretable as a message location for Emacs or
      other editors.
    """
    return '{}:{:8}'.format(meta.filename, '{}:'.format(meta.lineno))


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
    oss.write('{} {}\n'.format(render_source(error.source), error.message))
    if error.entry is not None:
        entries = error.entry if isinstance(error.entry, list) else [error.entry]
        error_string = '\n'.join(format_entry(entry) for entry in entries)
        oss.write('\n')
        oss.write(textwrap.indent(error_string, '   '))
        oss.write('\n')
    return oss.getvalue()


def print_error(error, file=None):
    """A convenience function that prints a single error to a file.

    Args:
      error: An error object.
      file: An optional file object to write the errors to.
    """
    output = file or sys.stdout
    output.write(format_error(error))
    output.write('\n')


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
        output.write('\n')
