"""Format converter reports.

This module contains reports that can convert an input file into other formats,
such as Ledger.
"""
import datetime
import re
import io
import textwrap

import dateutil.parser

from beancount.reports import report
from beancount.reports import table
from beancount.reports import tree_table
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import realization
from beancount.core import getters
from beancount.core import account_types
from beancount.core import complete
from beancount.core import amount
from beancount.utils import misc_utils


class LedgerReport(report.Report):
    """Print out the entries in a format that can be parsed by Ledger."""

    names = ['ledger']
    default_format = 'ledger'

    def render_ledger(self, entries, errors, options_map, file):
        ledger_printer = LedgerPrinter()
        for entry in entries:
            file.write(ledger_printer(entry))
            file.write('\n')


def quote(match):
    """Add quotes around a re.MatchObject.

    Args:
      match: A MatchObject from the re module.
    Returns:
      A quoted string of the match contents.
    """
    currency = match.group(1)
    return '"{}"'.format(currency) if re.search(r'[0-9\.]', currency) else currency


def quote_currency(string):
    """Quote all the currencies with numbers from the given string.

    Args:
      string: A string of text.
    Returns:
      A string of text, with the commodity expressions surrounded with quotes.
    """
    return re.sub(r'\b([A-Z][A-Z0-9\'\.\_\-]{0,10}[A-Z0-9])\b', quote, string)


class LedgerPrinter:
    "Multi-method for printing directives in Ledger format."

    # pylint: disable=invalid-name

    @classmethod
    def __call__(cls, obj):
        oss = io.StringIO()
        getattr(cls, obj.__class__.__name__)(cls, obj, oss)
        return oss.getvalue()

    def Transaction(cls, entry, oss):
        strings = []

        if entry.tags:
            for tag in sorted(entry.tags):
                strings.append(';; Tag: #{}'.format(tag))
        if entry.links:
            for link in sorted(entry.links):
                strings.append(';; Link: ^{}'.format(link))

        # Compute the string for the payee and narration line.
        if entry.payee:
            strings.append('{} |'.format(entry.payee))
        if entry.narration:
            strings.append(entry.narration)

        oss.write('{e.date:%Y/%m/%d} {flag} {}\n'.format(' '.join(strings),
                                                         flag=entry.flag or '',
                                                         e=entry))

        for posting in entry.postings:
            cls.Posting(cls, posting, oss)

    def Posting(_, posting, oss):
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

        posting_str = '  {:64} {:>16} {:>16} {:>16}'.format(flag_posting,
                                                            quote_currency(amount_str),
                                                            quote_currency(cost_str),
                                                            quote_currency(price_str))
        oss.write(posting_str.rstrip())

        oss.write('\n')

    def Balance(_, entry, oss):
        # We cannot output balance directive equivalents because Ledger only
        # supports file assertions and not dated assertions. See "Balance
        # Assertions for Beancount" for details:
        # https://docs.google.com/document/d/1vyemZFox47IZjuBrT2RjhSHZyTgloYOUeJb73RxMRD0/
        pass

    def Note(_, entry, oss):
        oss.write(';; Note: {e.date:%Y/%m/%d} {e.account} {e.comment}\n'.format(e=entry))

    def Document(_, entry, oss):
        oss.write(';; Document: {e.date:%Y/%m/%d} {e.account} {e.filename}\n'.format(
            e=entry))

    def Pad(_, entry, oss):
        # Note: We don't need to output these because when we're loading the
        # Beancount file explicit padding entries will be generated
        # automatically, thus balancing the accounts. Ledger does not support
        # automatically padding, so we can just output this as a comment.
        oss.write(';; Pad: {e.date:%Y/%m/%d} {e.account} {e.source_account}\n'.format(
            e=entry))

    def Open(_, entry, oss):
        oss.write('account {e.account:47}\n'.format(e=entry))
        if entry.currencies:
            oss.write('  assert {}\n'.format(' | '.join('commodity == "{}"'.format(currency)
                                                        for currency in entry.currencies)))

    def Close(_, entry, oss):
        oss.write(';; Close: {e.date:%Y/%m/%d} close {e.account}\n'.format(e=entry))

    def Price(_, entry, oss):
        price_directive = 'P {e.date:%Y/%m/%d} 00:00:00 {e.currency:<16} {amount:>16}\n'.format(
            e=entry, amount=str(entry.amount))
        oss.write(quote_currency(price_directive))

    def Event(_, entry, oss):
        oss.write(
            ';; Event: {e.date:%Y/%m/%d} "{e.type}" "{e.description}"\n'.format(e=entry))


__reports__ = [
    LedgerReport,
    ]
