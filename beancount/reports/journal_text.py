"""Text rendering routines for serving a lists of postings/entries.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import csv
import itertools
import math
import textwrap

from beancount.core.number import ZERO
from beancount.core import data
from beancount.core import realization
from beancount.core import convert


# Name mappings for text rendering, no more than 5 characters to save space.
TEXT_SHORT_NAME = {
    data.Open: 'open',
    data.Close: 'close',
    data.Pad: 'pad',
    data.Balance: 'bal',
    data.Transaction: 'txn',
    data.Note: 'note',
    data.Event: 'event',
    data.Query: 'query',
    data.Price: 'price',
    data.Document: 'doc',
    }


class AmountColumnSizer:
    """A class that computes minimal sizes for columns of numbers and their currencies.
    """

    def __init__(self, prefix):
        self.prefix = prefix
        self.max_number = ZERO
        self.max_currency_width = 0

    def update(self, number, currency):
        """Update the sizer with the given number and currency.

        Args:
          number: A Decimal instance.
          currency: A string, the currency to render for it.
        """
        abs_number = abs(number)
        if abs_number > self.max_number:
            self.max_number = abs_number
        currency_width = len(currency)
        if currency_width > self.max_currency_width:
            self.max_currency_width = currency_width

    def get_number_width(self):
        """Return the width of the integer part of the max number.

        Returns:
          An integer, the number of digits required to render the integral part.
        """
        return ((math.floor(math.log10(self.max_number)) + 1)
                if self.max_number > 0
                else 1)

    def get_generic_format(self, precision):
        """Return a generic format string for rendering as wide as required.
        This can be used to render an empty string in-lieu of a number.

        Args:
          precision: An integer, the number of digits to render after the period.
        Returns:
          A new-style Python format string, with PREFIX_number and PREFIX_currency named
          fields.
        """
        return '{{{prefix}:<{width}}}'.format(
            prefix=self.prefix,
            width=1 + self.get_number_width() + 1 + precision + 1 + self.max_currency_width)

    def get_format(self, precision):
        """Return a format string for the column of numbers.

        Args:
          precision: An integer, the number of digits to render after the period.
        Returns:
          A new-style Python format string, with PREFIX_number and PREFIX_currency named
          fields.
        """
        return ('{{0:>{width:d}.{precision:d}f}} {{1:<{currency_width}}}').format(
            width=1 + self.get_number_width() + 1 + precision,
            precision=precision,
            currency_width=self.max_currency_width)


# Verbosity levels.
COMPACT, NORMAL, VERBOSE = 1, 2, 3

# Output formats.
FORMAT_TEXT, FORMAT_CSV = object(), object()


def text_entries_table(oss, postings,
                       width, at_cost, render_balance, precision, verbosity,
                       output_format):
    """Render a table of postings or directives with an accumulated balance.

    This function has three verbosity modes for rendering:
    1. COMPACT: no separating line, no postings
    2. NORMAL: a separating line between entries, no postings
    3. VERBOSE: renders all the postings in addition to normal.

    The output is written to the 'oss' file object. Nothing is returned.

    Args:
      oss: A file object to write the output to.
      postings: A list of Posting or directive instances.
      width: An integer, the width to render the table to.
      at_cost: A boolean, if true, render the cost value, not the actual.
      render_balance: A boolean, if true, renders a running balance column.
      precision: An integer, the number of digits to render after the period.
      verbosity: An integer, the verbosity level. See COMPACT, NORMAL, VERBOSE, etc.
      output_format: A string, either 'text' or 'csv' for the chosen output format.
        This routine's inner loop calculations are complex enough it gets reused by both
        formats.
    Raises:
      ValueError: If the width is insufficient to render the description.
    """
    assert output_format in (FORMAT_TEXT, FORMAT_CSV)
    if output_format is FORMAT_CSV:
        csv_writer = csv.writer(oss)

    # Render the changes and balances to lists of amounts and precompute sizes.
    entry_data, change_sizer, balance_sizer = size_and_render_amounts(postings,
                                                                      at_cost,
                                                                      render_balance)

    # Render an empty line and compute the width the description should be (the
    # description is the only elastic field).
    empty_format = '{{date:10}} {{dirtype:5}} {{description}}  {}'.format(
        change_sizer.get_generic_format(precision))
    if render_balance:
        empty_format += '  {}'.format(balance_sizer.get_generic_format(precision))
    empty_line = empty_format.format(date='', dirtype='', description='',
                                     change='', balance='')
    description_width = width - len(empty_line)
    if description_width <= 0:
        raise ValueError(
            "Width not sufficient to render text report ({} chars)".format(width))

    # Establish a format string for the final format of all lines.
    line_format = '{{date:10}} {{dirtype:5}} {{description:{:d}.{:d}}}  {}'.format(
        description_width, description_width,
        change_sizer.get_generic_format(precision))
    change_format = change_sizer.get_format(precision)
    if render_balance:
        line_format += '  {}'.format(balance_sizer.get_generic_format(precision))
        balance_format = balance_sizer.get_format(precision)
    line_format += '\n'

    # Iterate over all the pre-computed data.
    for (entry, leg_postings, change_amounts, balance_amounts) in entry_data:

        # Render the date.
        date = entry.date.isoformat()

        # Get the directive type name.
        dirtype = TEXT_SHORT_NAME[type(entry)]
        if isinstance(entry, data.Transaction) and entry.flag:
            dirtype = entry.flag

        # Get the description string and split the description line in multiple
        # lines.
        description = get_entry_text_description(entry)
        description_lines = textwrap.wrap(description, width=description_width)

        # Ensure at least one line is rendered (for zip_longuest).
        if not description_lines:
            description_lines.append('')

        # Render all the amounts in the line.
        for (description,
             change_amount,
             balance_amount) in itertools.zip_longest(description_lines,
                                                      change_amounts,
                                                      balance_amounts,
                                                      fillvalue=''):

            change = (change_format.format(change_amount.number,
                                           change_amount.currency)
                      if change_amount
                      else '')

            balance = (balance_format.format(balance_amount.number,
                                             balance_amount.currency)
                       if balance_amount
                       else '')

            if not description and verbosity >= VERBOSE and leg_postings:
                description = '..'

            if output_format is FORMAT_TEXT:
                oss.write(line_format.format(date=date,
                                             dirtype=dirtype,
                                             description=description,
                                             change=change,
                                             balance=balance))
            else:
                change_number, change_currency = '', ''
                if change:
                    change_number, change_currency = change.split()

                if render_balance:
                    balance_number, balance_currency = '', ''
                    if balance:
                        balance_number, balance_currency = balance.split()

                    row = (date, dirtype, description,
                           change_number, change_currency,
                           balance_number, balance_currency)
                else:
                    row = (date, dirtype, description,
                           change_number, change_currency)
                csv_writer.writerow(row)

            # Reset the date, directive type and description. Only the first
            # line renders these; the other lines render only the amounts.
            if date:
                date = dirtype = ''

        if verbosity >= VERBOSE:
            for posting in leg_postings:
                posting_str = render_posting(posting, change_format)
                if len(posting_str) > description_width:
                    posting_str = posting_str[:description_width-3] + '...'

                if output_format is FORMAT_TEXT:
                    oss.write(line_format.format(date='',
                                                 dirtype='',
                                                 description=posting_str,
                                                 change='',
                                                 balance=''))
                else:
                    row = ('', '', posting_str)
                    csv_writer.writerow(row)

        if verbosity >= NORMAL:
            oss.write('\n')


def render_posting(posting, number_format):
    """Render a posting compactly, for text report rendering.

    Args:
      posting: An instance of Posting.
    Returns:
      A string, the rendered posting.
    """
    # Note: there's probably no need to redo the work of rendering here... see
    # if you can't just simply replace this by Position.to_string().

    units = posting.units
    strings = [
        posting.flag if posting.flag else ' ',
        '{:32}'.format(posting.account),
        number_format.format(units.number, units.currency)
        ]

    cost = posting.cost
    if cost:
        strings.append('{{{}}}'.format(number_format.format(cost.number,
                                                            cost.currency).strip()))

    price = posting.price
    if price:
        strings.append('@ {}'.format(number_format.format(price.number,
                                                          price.currency).strip()))

    return ' '.join(strings)


def size_and_render_amounts(postings, at_cost, render_balance):
    """Iterate through postings and compute sizers and render amounts.

    Args:
      postings: A list of Posting or directive instances.
      at_cost: A boolean, if true, render the cost value, not the actual.
      render_balance: A boolean, if true, renders a running balance column.
    """

    # Compute the maximum width required to render the change and balance
    # columns. In order to carry this out, we will pre-compute all the data to
    # render this and save it for later.
    change_sizer = AmountColumnSizer('change')
    balance_sizer = AmountColumnSizer('balance')

    entry_data = []
    for entry_line in realization.iterate_with_balance(postings):
        entry, leg_postings, change, balance = entry_line

        # Convert to cost if necessary. (Note that this agglutinates currencies,
        # so we'd rather do make the conversion at this level (inventory) than
        # convert the positions or amounts later.)
        if at_cost:
            change = change.reduce(convert.get_cost)
            if render_balance:
                balance = balance.reduce(convert.get_cost)

        # Compute the amounts and maximum widths for the change column.
        change_amounts = []
        for position in change.get_positions():
            units = position.units
            change_amounts.append(units)
            change_sizer.update(units.number, units.currency)

        # Compute the amounts and maximum widths for the balance column.
        balance_amounts = []
        if render_balance:
            for position in balance.get_positions():
                units = position.units
                balance_amounts.append(units)
                balance_sizer.update(units.number, units.currency)

        entry_data.append((entry, leg_postings, change_amounts, balance_amounts))

    return (entry_data, change_sizer, balance_sizer)


def get_entry_text_description(entry):
    """Return the text of a description.

    Args:
      entry: A directive, of any type.
    Returns:
      A string to use for the filling the description field in text reports.
    """
    if isinstance(entry, data.Transaction):
        description = ' | '.join([field
                                  for field in [entry.payee, entry.narration]
                                  if field is not None])
    elif isinstance(entry, data.Balance):
        if entry.diff_amount is None:
            description = 'PASS - In {}'.format(entry.account)
        else:
            description = ('FAIL - In {}; '
                           'expected = {}, difference = {}').format(
                               entry.account,
                               entry.amount,
                               entry.diff_amount)
    elif isinstance(entry, (data.Open, data.Close)):
        description = entry.account
    elif isinstance(entry, data.Note):
        description = entry.comment
    elif isinstance(entry, data.Document):
        description = entry.filename
    else:
        description = '-'
    return description
