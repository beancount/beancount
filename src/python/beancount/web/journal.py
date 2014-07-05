"""HTML rendering routines for serving a lists of postings/entries.
"""
import collections
from os import path

from beancount.core.data import Open, Close, Balance, Transaction, Note, Document
from beancount.core import account
from beancount.core import complete
from beancount.core import realization
from beancount.core import flags


def account_link(account_, build_url, leafonly=False):
    """Render an HTML anchor for the given account name.

    The conversion to string is memoized, as it never changes. An actual link to
    an account is only rendered if a build_url callable is provided. Otherwise,
    a snippet of HTML that will just render the name of the account without a
    link is rendered.

    Args:
      account_name: A string, the name of the account to render, or an
        instance of RealAccount. Both are accepted.
      build_url: A function used to build a URL. If not specified, a snippet of
        HTML to at least render the account name without a link is produced.
      leafonly: A boolean, if true, render only the name of the leaf, not the
        entire account name.
    Returns:
      A string, a snippet of HTML that renders and links to the account name.
    """
    if isinstance(account_, str):
        account_name = account_
    elif isinstance(account_, realization.RealAccount):
        account_name = account_.account

    slashed_name = account_name.replace(account.sep, '/')

    if leafonly:
        account_name = account.leaf(account_name)

    if build_url is None:
        link = '<span class="account">{}</a>'.format(account_name)
    else:
        link = '<a href="{}" class="account">{}</a>'.format(
            build_url('account', slashed_account_name=slashed_name),
            account_name)

    return link


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


def iterate_render_postings(postings, build_url):
    """Iterate through the list of transactions with rendered strings for each cell.

    This pre-renders all the data for each row to HTML. This is reused by the entries
    table rendering routines.

    Args:
      postings: A list of Posting or directive instances.
      build_url: A function that builds/renders URLs based on a Bottle app.
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

        if isinstance(entry, Transaction):
            rowtype = FLAG_ROWTYPES.get(entry.flag, 'Transaction')
            extra_class = 'warning' if entry.flag == flags.FLAG_WARNING else ''
            flag = entry.flag
            description = '<span class="narration">{}</span>'.format(entry.narration)
            if entry.payee:
                description = ('<span class="payee">{}</span>'
                               '<span class="pnsep">|</span>'
                               '{}').format(entry.payee, description)
            amount_str = balance_html(change)

            if entry.links:
                links = [build_url('link', link=link)
                         for link in entry.links]

        elif isinstance(entry, Balance):
            # Check the balance here and possibly change the rowtype
            if entry.diff_amount is None:
                description = 'Balance {} has {}'.format(
                    account_link(entry.account, build_url),
                    entry.amount)
            else:
                description = ('Balance in {} fails; '
                               'expected = {}, balance = {}, difference = {}').format(
                                   account_link(entry.account, build_url),
                                   entry.amount,
                                   entry_balance.get_amount(entry.amount.currency),
                                   entry.diff_amount)
                extra_class = 'fail'

            amount_str = str(entry.amount)

        elif isinstance(entry, (Open, Close)):
            description = '{} {}'.format(entry.__class__.__name__,
                                         account_link(entry.account, build_url))
            amount_str = ''

        elif isinstance(entry, Note):
            description = '{} {}'.format(entry.__class__.__name__, entry.comment)
            amount_str = ''
            balance_str = ''

        elif isinstance(entry, Document):
            assert path.isabs(entry.filename)
            description = 'Document for {}: "<a href="{}" class="filename">{}</a>"'.format(
                account_link(entry.account, build_url),
                build_url('doc', filename=entry.filename.lstrip('/')),
                path.basename(entry.filename))
            amount_str = ''
            balance_str = ''

        else:
            description = entry.__class__.__name__
            amount_str = ''
            balance_str = ''

        yield Row(entry, leg_postings,
                  rowtype, extra_class,
                  flag, description, links, amount_str, balance_str)


def entries_table_with_balance(oss, account_postings, build_url, render_postings=True):
    """Render a list of entries into an HTML table, with a running balance.

    (This function returns nothing, it write to oss as a side-effect.)

    Args:
      oss: A file object to write the output to.
      account_postings: A list of Posting or directive instances.
      build_url: If not None, a function to build/render URLs to.
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

    for row in iterate_render_postings(account_postings, build_url):
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
                   '{}:{}'.format(entry.fileloc.filename, entry.fileloc.lineno),
                   entry.date, row.flag, description,
                   row.amount_str, row.balance_str))

        if render_postings and isinstance(entry, Transaction):
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
                           account_link(posting.account, build_url),
                           posting.position,
                           posting.price or '',
                           complete.get_balance_amount(posting)))

    write('</table>')


def entries_table(oss, account_postings, build_url, render_postings=True):
    """Render a list of entries into an HTML table, with no running balance.

    This is appropriate for rendering tables of entries for postings with
    multiple accounts, whereby computing the running balances makes little
    sense.

    (This function returns nothing, it write to oss as a side-effect.)

    Args:
      oss: A file object to write the output to.
      account_postings: A list of Posting or directive instances.
      build_url: If not None, a function to build/render URLs to.
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

    for row in iterate_render_postings(account_postings, build_url):
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
                   '{}:{}'.format(entry.fileloc.filename, entry.fileloc.lineno),
                   entry.date, row.flag, description))

        if render_postings and isinstance(entry, Transaction):
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
                           account_link(posting.account, build_url),
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
