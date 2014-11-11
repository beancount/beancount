"""Conversion from internal data structures to text.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import io
import sys
import textwrap

from beancount.core import amount
from beancount.core import interpolate
from beancount.core import data


class EntryPrinter:
    "Multi-method for printing an entry."

    # pylint: disable=invalid-name

    @classmethod
    def __call__(cls, obj):
        oss = io.StringIO()
        getattr(cls, obj.__class__.__name__)(cls, obj, oss)
        return oss.getvalue()

    def Transaction(cls, entry, oss):
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

        non_trivial_balance = any(map(interpolate.has_nontrivial_balance, entry.postings))
        for posting in entry.postings:
            cls.Posting(cls, posting, oss, non_trivial_balance)

    def Posting(_, posting, oss, print_balance=False):
        flag = '{} '.format(posting.flag) if posting.flag else ''
        assert posting.account is not None

        flag_posting = '{:}{:62}'.format(flag, posting.account)

        if posting.position:
            amount_str, cost_str = posting.position.strs()
        else:
            amount_str, cost_str = '', ''

        price_str = ('@ {}'.format(posting.price.str(amount.MAXDIGITS_PRINTER))
                     if posting.price is not None
                     else '')

        if print_balance:
            if posting.position:
                balance_amount = interpolate.get_posting_weight(posting)
                balance_amount_str = balance_amount.str(amount.MAXDIGITS_PRINTER)
            else:
                balance_amount_str = 'UNKNOWN'
            balance_str = '; {:>16}'.format(balance_amount_str)
        else:
            balance_str = ''

        # assert all(len(string) <= 25
        #            for string in (amount_str, cost_str, price_str, balance_str)), (
        #                    amount_str, cost_str, price_str, balance_str)
        oss.write('  {:64} {:>22} {:>22} {:>22} {:>22}'.format(
            flag_posting, amount_str, cost_str, price_str, balance_str).rstrip())

        oss.write('\n')

    def Balance(_, entry, oss):
        comment = '   ; Diff: {}'.format(entry.diff_amount) if entry.diff_amount else ''
        oss.write(('{e.date} balance {e.account:47} {e.amount:>22}'
                   '{comment}\n').format(e=entry, comment=comment))

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


def format_entry(entry):
    """Format an entry into a string in the same input syntax the parser accepts.

    Args:
      entry: An entry instance.
    Returns:
      A string, the formatted entry.
    """
    return EntryPrinter()(entry)


def print_entry(entry, file=None):
    """A convenience function that prints a single entry to a file.

    Args:
      entry: A directive entry.
      file: An optional file object to write the entries to.
    """
    output = file or sys.stdout
    output.write(format_entry(entry))
    output.write('\n')


def print_entries(entries, file=None, prefix=None):
    """A convenience function that prints a list of entries to a file.

    Args:
      entries: A list of directives.
      file: An optional file object to write the entries to.
    """
    assert isinstance(entries, list)
    output = file or sys.stdout
    if prefix:
        output.write(prefix)
    previous_type = type(entries[0]) if entries else None
    for entry in entries:
        # Insert a newline between transactions and between blocks of directives
        # of the same type.
        entry_type = type(entry)
        if entry_type is data.Transaction or entry_type is not previous_type:
            output.write('\n')
            previous_type = entry_type

        output.write(format_entry(entry))


def render_source(source):
    """Render the source for errors in a way that it will be both detected by
    Emacs and align and rendered nicely.

    Args:
      source: an instance of Source.
    Returns:
      A string, rendered to be interpretable as a message location for Emacs or
      other editors.
    """
    return '{}:{:8}'.format(source.filename, '{}:'.format(source.lineno))


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
