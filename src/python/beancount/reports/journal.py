"""HTML rendering routines for serving a lists of postings/entries.
"""
import collections
import datetime
import itertools
import math
from os import path

from beancount.core.amount import ZERO
from beancount.core import data
from beancount.core import complete
from beancount.core import realization
from beancount.core import flags


def balance_html(balance_inventory):
    """Render a list of balance positions for an HTML table cell.

    Each position is rendered on its own HTML row.

    Args:
      balance_inventory: An instance of Inventory.
    Return:
      A string, a snippet of HTML.
    """
    return ('<br/>'.join(map(str, balance_inventory.get_positions()))
            if not balance_inventory.is_empty()
            else '')


# Names to render for transaction rows.
FLAG_ROWTYPES = {
    flags.FLAG_PADDING  : 'Padding',
    flags.FLAG_SUMMARIZE: 'Summarize',
    flags.FLAG_TRANSFER : 'Transfer',
}


# A rendered row.
#
# Attributes:
#   entry: The parent entry we are rendering postings for. This can be of
#     any valid directive type, not just Transaction instances.
#   leg_postings: A list of postings that apply to this row.
#   rowtype: A renderable string that dscribes the type of this row's directive.
#   extra_class: A CSS class to be add to the row. This is used for marking some
#     rows as warning rows.
#   flag: The flag attached to this row, if Transaction, or an empty string.
#   description: A string, the narration to render on the row.
#   links: A list of link strings to render, if desired.
#   change_str: A string, the rendered inventory of changes being posted by this row.
#   balance_str: A string, the rendered inventory of the resulting balance after
#     applying the change to this row.
#
Row = collections.namedtuple('Row',
                             'entry leg_postings rowtype extra_class flag '
                             'description links amount_str balance_str')


def iterate_html_postings(postings, formatter):
    """Iterate through the list of transactions with rendered HTML strings for each cell.

    This pre-renders all the data for each row to HTML. This is reused by the entries
    table rendering routines.

    Args:
      postings: A list of Posting or directive instances.
      formatter: An instance of HTMLFormatter, to be render accounts, links and docs.
    Yields:
      Instances of Row tuples. See above.
    """
    for entry_line in realization.iterate_with_balance(postings):
        entry, leg_postings, change, entry_balance = entry_line

        # Prepare the data to be rendered for this row.
        balance_str = balance_html(entry_balance)

        rowtype = entry.__class__.__name__
        flag = ''
        extra_class = ''
        links = None

        if isinstance(entry, data.Transaction):
            rowtype = FLAG_ROWTYPES.get(entry.flag, 'Transaction')
            extra_class = 'warning' if entry.flag == flags.FLAG_WARNING else ''
            flag = entry.flag
            description = '<span class="narration">{}</span>'.format(entry.narration)
            if entry.payee:
                description = ('<span class="payee">{}</span>'
                               '<span class="pnsep">|</span>'
                               '{}').format(entry.payee, description)
            amount_str = balance_html(change)

            if entry.links and formatter:
                links = [formatter.render_link(link) for link in entry.links]

        elif isinstance(entry, data.Balance):
            # Check the balance here and possibly change the rowtype
            if entry.diff_amount is None:
                description = 'Balance {} has {}'.format(
                    formatter.render_account(entry.account),
                    entry.amount)
            else:
                description = ('Balance in {} fails; '
                               'expected = {}, balance = {}, difference = {}').format(
                                   formatter.render_account(entry.account),
                                   entry.amount,
                                   entry_balance.get_amount(entry.amount.currency),
                                   entry.diff_amount)
                extra_class = 'fail'

            amount_str = str(entry.amount)

        elif isinstance(entry, (data.Open, data.Close)):
            description = '{} {}'.format(entry.__class__.__name__,
                                         formatter.render_account(entry.account))
            amount_str = ''

        elif isinstance(entry, data.Note):
            description = '{} {}'.format(entry.__class__.__name__, entry.comment)
            amount_str = ''
            balance_str = ''

        elif isinstance(entry, data.Document):
            assert path.isabs(entry.filename)
            description = 'Document for {}: {}'.format(
                formatter.render_account(entry.account),
                formatter.render_doc(entry.filename))
            amount_str = ''
            balance_str = ''

        else:
            description = entry.__class__.__name__
            amount_str = ''
            balance_str = ''

        yield Row(entry, leg_postings,
                  rowtype, extra_class,
                  flag, description, links, amount_str, balance_str)


def html_entries_table_with_balance(oss, account_postings, formatter, render_postings=True):
    """Render a list of entries into an HTML table, with a running balance.

    (This function returns nothing, it write to oss as a side-effect.)

    Args:
      oss: A file object to write the output to.
      account_postings: A list of Posting or directive instances.
      formatter: An instance of HTMLFormatter, to be render accounts, links and docs.
      render_postings: A boolean; if true, render the postings as rows under the
        main transaction row.
    """
    write = lambda data: (oss.write(data), oss.write('\n'))

    write('''
      <table class="entry-table">
      <thead>
        <tr>
         <th class="datecell">Date</th>
         <th class="flag">F</th>
         <th class="description">Narration/Payee</th>
         <th class="position">Position</th>
         <th class="price">Price</th>
         <th class="cost">Cost</th>
         <th class="change">Change</th>
         <th class="balance">Balance</th>
      </thead>
    ''')

    for row in iterate_html_postings(account_postings, formatter):
        entry = row.entry

        description = row.description
        if row.links:
            description += render_links(row.links)

        # Render a row.
        write('''
          <tr class="{} {}" title="{}">
            <td class="datecell">{}</td>
            <td class="flag">{}</td>
            <td class="description" colspan="4">{}</td>
            <td class="change num">{}</td>
            <td class="balance num">{}</td>
          <tr>
        '''.format(row.rowtype, row.extra_class,
                   '{}:{}'.format(entry.source.filename, entry.source.lineno),
                   entry.date, row.flag, description,
                   row.amount_str, row.balance_str))

        if render_postings and isinstance(entry, data.Transaction):
            for posting in entry.postings:

                classes = ['Posting']
                if posting.flag == flags.FLAG_WARNING:
                    classes.append('warning')
                if posting in row.leg_postings:
                    classes.append('leg')

                write('''
                  <tr class="{}">
                    <td class="datecell"></td>
                    <td class="flag">{}</td>
                    <td class="description">{}</td>
                    <td class="position num">{}</td>
                    <td class="price num">{}</td>
                    <td class="cost num">{}</td>
                    <td class="change num"></td>
                    <td class="balance num"></td>
                  <tr>
                '''.format(' '.join(classes),
                           posting.flag or '',
                           formatter.render_account(posting.account),
                           posting.position,
                           posting.price or '',
                           complete.get_balance_amount(posting)))

    write('</table>')


def html_entries_table(oss, account_postings, formatter, render_postings=True):
    """Render a list of entries into an HTML table, with no running balance.

    This is appropriate for rendering tables of entries for postings with
    multiple accounts, whereby computing the running balances makes little
    sense.

    (This function returns nothing, it write to oss as a side-effect.)

    Args:
      oss: A file object to write the output to.
      account_postings: A list of Posting or directive instances.
      formatter: An instance of HTMLFormatter, to be render accounts, links and docs.
      render_postings: A boolean; if true, render the postings as rows under the
        main transaction row.
    """
    write = lambda data: (oss.write(data), oss.write('\n'))

    write('''
      <table class="entry-table">
      <thead>
        <tr>
         <th class="datecell">Date</th>
         <th class="flag">F</th>
         <th class="description">Narration/Payee</th>
         <th class="amount">Amount</th>
         <th class="cost">Cost</th>
         <th class="price">Price</th>
         <th class="balance">Balance</th>
      </thead>
    ''')

    for row in iterate_html_postings(account_postings, formatter):
        entry = row.entry

        description = row.description
        if row.links:
            description += render_links(row.links)

        # Render a row.
        write('''
          <tr class="{} {}" title="{}">
            <td class="datecell">{}</td>
            <td class="flag">{}</td>
            <td class="description" colspan="5">{}</td>
          <tr>
        '''.format(row.rowtype, row.extra_class,
                   '{}:{}'.format(entry.source.filename, entry.source.lineno),
                   entry.date, row.flag, description))

        if render_postings and isinstance(entry, data.Transaction):
            for posting in entry.postings:

                classes = ['Posting']
                if posting.flag == flags.FLAG_WARNING:
                    classes.append('warning')

                write('''
                  <tr class="{}">
                    <td class="datecell"></td>
                    <td class="flag">{}</td>
                    <td class="description">{}</td>
                    <td class="amount num">{}</td>
                    <td class="cost num">{}</td>
                    <td class="price num">{}</td>
                    <td class="balance num">{}</td>
                  <tr>
                '''.format(' '.join(classes),
                           posting.flag or '',
                           formatter.render_account(posting.account),
                           posting.position.get_amount(),
                           posting.position.lot.cost or '',
                           posting.price or '',
                           complete.get_balance_amount(posting)))

    write('</table>')


def render_links(links):
    """Render Transaction links to HTML.

    Args:
      links: A list of set of strings, transaction "links" to be rendered.
    Returns:
      A string, a snippet of HTML to be rendering somewhere.
    """
    return '<span class="links">{}</span>'.format(
        ''.join('<a href="{}">^</a>'.format(link)
                for link in links))


# Name mappings for text rendering, no more than 5 characters to save space.
TEXT_SHORT_NAME = {
    data.Open: 'open',
    data.Close: 'close',
    data.Pad: 'pad',
    data.Balance: 'bal',
    data.Transaction: 'txn',
    data.Note: 'note',
    data.Event: 'event',
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

    def get_generic_format(self, precision):
        """Return a generic format string for rendering as wide as required.
        This can be used to render an empty string in-lieu of a number.

        Args:
          precision: An integer, the number of digits to render after the period.
        Returns:
          A new-style Python format string, with PREFIX_number and PREFIX_currency named
          fields.
        """
        int_number_width = math.floor(math.log10(self.max_number)) + 1
        return '{{{prefix}:<{width}}}'.format(
            prefix=self.prefix,
            width=1 + int_number_width + 1 + precision + 1 + self.max_currency_width)

    def get_format(self, precision):
        """Return a format string for the column of numbers.

        Args:
          precision: An integer, the number of digits to render after the period.
        Returns:
          A new-style Python format string, with PREFIX_number and PREFIX_currency named
          fields.
        """
        int_number_width = math.floor(math.log10(self.max_number)) + 1
        return ('{{0:>{width:d}.{precision:d}f}} {{1:<{currency_width}}}').format(
                    prefix=self.prefix,
                    width=1 + int_number_width + 1 + precision,
                    precision=precision,
                    currency_width=self.max_currency_width)


def text_entries_table(oss, postings, width, render_balance, at_cost, precision):
    """Render a table of postings or directives with an accumulated balance.

    The output is written to the 'oss' file object.

    Args:
      oss: A file object to write the output to.
      postings: A list of Posting or directive instances.
      width: An integer, the width to render the table to.
      render_balance: A boolean, if true, renders a running balance column.
      at_cost: A boolean, if true, render the cost value, not the actual.
      precision: An integer, the number of digits to render after the period.
    Raises:
      ValueError: If the width is insufficient to render the description.
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
            change = change.get_cost()
            if render_balance:
                balance = balance.get_cost()

        # Compute the amounts and maximum widths for the change column.
        change_amounts = [position.get_amount()
                          for position in change.get_positions()]
        for amount in change_amounts:
            change_sizer.update(amount.number, amount.currency)

        # Compute the amounts and maximum widths for the balance column.
        if render_balance:
            balance_amounts = [position.get_amount()
                               for position in balance.get_positions()]
            for amount in balance_amounts:
                balance_sizer.update(amount.number, amount.currency)
        else:
            balance_amounts = []

        entry_data.append((entry, leg_postings, change_amounts, balance_amounts))

    # Render an empty line and compute the width the description should be (the
    # description is the only elastic field).
    empty_format = '{{date:10}} {{dirtype:5}} {{description}}  {}'.format(
        change_sizer.get_generic_format(precision))
    if render_balance:
        empty_format += '  {}'.format(balance_sizer.get_generic_format(precision))
    empty_line = empty_format.format(date='', dirtype='', description='', change='', balance='')
    description_width = width - len(empty_line)
    if description_width <= 0:
        raise ValueError("Width not sufficient to render text report ({} chars)".format(width))

    # Establish a format string for the final format of all lines.
    FORMAT = '{{date:10}} {{dirtype:5}} {{description:{:d}.{:d}}}  {}'.format(
        description_width, description_width,
        change_sizer.get_generic_format(precision))
    change_format = change_sizer.get_format(precision)
    if render_balance:
        FORMAT += '  {}'.format(balance_sizer.get_generic_format(precision))
        balance_format = balance_sizer.get_format(precision)
    FORMAT += '\n'

    # Iterate over all the pre-computed data.
    for (entry, leg_postings, change_amounts, balance_amounts) in entry_data:

        # Render the date and directive type.
        date = entry.date.isoformat()
        dirtype = TEXT_SHORT_NAME[type(entry)]

        # Get the description string.
        if isinstance(entry, data.Transaction):
            dirtype = '  {:1}  '.format(entry.flag) if entry.flag else dirtype[type(entry)]
            description = ' | '.join([field
                                      for field in [entry.payee, entry.narration]
                                      if field is not None])
        elif isinstance(entry, data.Balance):
            if entry.diff_amount is None:
                description = 'Balance PASS in {}'.format(entry.account)
            else:
                description = ('Balance FAIL in {}: '
                               'expected = {}, balance = {}, difference = {}').format(
                                   entry.account,
                                   entry.amount,
                                   entry_balance.get_amount(entry.amount.currency),
                                   entry.diff_amount)
        elif isinstance(entry, (data.Open, data.Close)):
            description = entry.account
        elif isinstance(entry, data.Note):
            description = entry.comment
        elif isinstance(entry, data.Document):
            description = entry.filename
        else:
            description = '-'

        # Ensure at least one line is rendered.
        if not change_amounts and not balance_amounts:
            change_amounts.append(None)

        # Render all the amounts in the line.
        for (change_amount, balance_amount) in itertools.zip_longest(change_amounts,
                                                                     balance_amounts):

            change = (change_format.format(change_amount.number,
                                           change_amount.currency)
                      if change_amount is not None
                      else '')

            balance = (balance_format.format(balance_amount.number,
                                             balance_amount.currency)
                       if balance_amount is not None
                       else '')

            oss.write(FORMAT.format(date=date,
                                    dirtype=dirtype,
                                    description=description,
                                    change=change,
                                    balance=balance))

            # Reset the date, directive type and description. Only the first
            # line renders these; the other lines render only the amounts.
            if date:
                date = dirtype = description = ''

        oss.write('\n')


    # FIXME: make rendering the balance optional.
    # FIXME: make rendering the lots, including full detail, possible. (maybe... for debugging?)

    ## FIXME(blais): implement this.
    # This function has three modes for rendering:
    # 1. compact: no separating line, no postings detail
    # 2. normal: a separating line, no postings detail
    # 3. verbose: renders all the postings in addition to normal.

    # FIXME: Add terminal colors (optional)

    # FIXME: Render an arbitrary expression of accounts, not just one. Keep in
    # mind that filtering will be supported eventually.

    # FIXME: Support CSV mode

    # FIXME: Render description line on multiple lines
