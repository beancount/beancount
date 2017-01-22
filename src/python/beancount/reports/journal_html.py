"""HTML rendering routines for serving a lists of postings/entries.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections
from os import path

from beancount.core import data
from beancount.core import position
from beancount.core import convert
from beancount.core import realization
from beancount.core import flags


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


def iterate_html_postings(txn_postings, formatter):
    """Iterate through the list of transactions with rendered HTML strings for each cell.

    This pre-renders all the data for each row to HTML. This is reused by the entries
    table rendering routines.

    Args:
      txn_postings: A list of TxnPosting or directive instances.
      formatter: An instance of HTMLFormatter, to be render accounts,
        inventories, links and docs.
    Yields:
      Instances of Row tuples. See above.
    """
    for entry_line in realization.iterate_with_balance(txn_postings):
        entry, leg_postings, change, entry_balance = entry_line

        # Prepare the data to be rendered for this row.
        balance_str = formatter.render_inventory(entry_balance)

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
            amount_str = formatter.render_inventory(change)

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
                                   entry_balance.get_currency_units(entry.amount.currency),
                                   entry.diff_amount)
                extra_class = 'fail'

            amount_str = formatter.render_amount(entry.amount)

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


def html_entries_table_with_balance(oss, txn_postings, formatter, render_postings=True):
    """Render a list of entries into an HTML table, with a running balance.

    (This function returns nothing, it write to oss as a side-effect.)

    Args:
      oss: A file object to write the output to.
      txn_postings: A list of Posting or directive instances.
      formatter: An instance of HTMLFormatter, to be render accounts,
        inventories, links and docs.
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
        </tr>
      </thead>
    ''')

    for row in iterate_html_postings(txn_postings, formatter):
        entry = row.entry

        description = row.description
        if row.links:
            description += render_links(row.links)

        # Render a row.
        write('''
          <tr class="{} {}" title="{}">
            <td class="datecell"><a href="{}">{}</a></td>
            <td class="flag">{}</td>
            <td class="description" colspan="4">{}</td>
            <td class="change num">{}</td>
            <td class="balance num">{}</td>
          </tr>
        '''.format(row.rowtype, row.extra_class,
                   '{}:{}'.format(entry.meta["filename"], entry.meta["lineno"]),
                   formatter.render_context(entry), entry.date,
                   row.flag, description,
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
                  </tr>
                '''.format(' '.join(classes),
                           posting.flag or '',
                           formatter.render_account(posting.account),
                           position.to_string(posting),
                           posting.price or '',
                           convert.get_weight(posting)))

    write('</table>')


def html_entries_table(oss, txn_postings, formatter, render_postings=True):
    """Render a list of entries into an HTML table, with no running balance.

    This is appropriate for rendering tables of entries for postings with
    multiple accounts, whereby computing the running balances makes little
    sense.

    (This function returns nothing, it write to oss as a side-effect.)

    Args:
      oss: A file object to write the output to.
      txn_postings: A list of Posting or directive instances.
      formatter: An instance of HTMLFormatter, to be render accounts,
        inventories, links and docs.
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
        </tr>
      </thead>
    ''')

    for row in iterate_html_postings(txn_postings, formatter):
        entry = row.entry

        description = row.description
        if row.links:
            description += render_links(row.links)

        # Render a row.
        write('''
          <tr class="{} {}" title="{}">
            <td class="datecell"><a href="{}">{}</a></td>
            <td class="flag">{}</td>
            <td class="description" colspan="5">{}</td>
          </tr>
        '''.format(row.rowtype, row.extra_class,
                   '{}:{}'.format(entry.meta["filename"], entry.meta["lineno"]),
                   formatter.render_context(entry), entry.date,
                   row.flag, description))

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
                  </tr>
                '''.format(' '.join(classes),
                           posting.flag or '',
                           formatter.render_account(posting.account),
                           posting.units or '',
                           posting.cost or '',
                           posting.price or '',
                           convert.get_weight(posting)))

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
