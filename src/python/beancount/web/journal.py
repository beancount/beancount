"""HTML rendering routines for serving a lists of postings/entries.
"""
from os import path

from bottle import request

from beancount.core.data import Open, Close, Balance, Transaction, Note, Document
from beancount.core import data
from beancount.core import account
from beancount.core import complete
from beancount.core.account import account_name_leaf
from beancount.core import realization
from beancount.core import flags
from beancount.utils import misc_utils


@misc_utils.staticvar('cache', {})
def account_link(account_, leafonly=False, request=request):
    """Render an HTML anchor for the given account name.

    The conversion to string is memoized, as it never changes.
    An actual link to an account is only rendered if the request is currently
    for a page with a resolved view. Otherwise, a snippet of HTML that will
    just render the name of the account without a link is rendered.

    Args:
      account_name: A string, the name of the account to render, or an
        instance of RealAccount. Both are accepted.
      leafonly: A boolean, if true, render only the name of the leaf, not the
        entire account name.
      request: An instance of Bottle.Request. You can override this for testing,
        otherwise just keep the default. This request object is inspected to
        figure out whether the page being rendered is in a view or not.
    Returns:
      A string, a snippet of HTML that renders and links to the account name..
    """
    if isinstance(account_, str):
        account_name = account_
    elif isinstance(account_, realization.RealAccount):
        account_name = account_.account
    try:
        return account_link.cache[(request.app, account_name, leafonly)]
    except KeyError:
        slashed_name = account_name.replace(account.sep, '/')

        if leafonly:
            account_name = account_name_leaf(account_name)

        if hasattr(request, 'view') and request.view is not None:
            link = '<a href="{}" class="account">{}</a>'.format(
                request.app.get_url('account', slashed_account_name=slashed_name),
                account_name)
            account_link.cache[account_name] = link
            return link
        else:
            return '<span class="account">{}</a>'.format(account_name)


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


FLAG_ROWTYPES = {
    flags.FLAG_PADDING  : 'Padding',
    flags.FLAG_SUMMARIZE: 'Summarize',
    flags.FLAG_TRANSFER : 'Transfer',
}


def iterate_render_transactions(app, postings):
    """Iterate through the list of transactions with rendered strings
    for each cell.
    Yields:
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
            change_str = balance_html(change)

            if entry.links:
                links = [app.router.build('link', link=link)
                         for link in entry.links]

        elif isinstance(entry, Balance):
            # Check the balance here and possibly change the rowtype
            if entry.diff_amount is None:
                description = 'Balance {} has {}'.format(account_link(entry.account),
                                                         entry.amount)
            else:
                description = ('Balance in {} fails; '
                               'expected = {}, balance = {}, difference = {}').format(
                                   account_link(entry.account), entry.amount,
                                   entry_balance.get_amount(entry.amount.currency),
                                   entry.diff_amount)
                rowtype = 'CheckFail'

            change_str = str(entry.amount)

        elif isinstance(entry, (Open, Close)):
            description = '{} {}'.format(entry.__class__.__name__,
                                         account_link(entry.account))
            change_str = ''

        elif isinstance(entry, Note):
            description = '{} {}'.format(entry.__class__.__name__, entry.comment)
            change_str = ''
            balance_str = ''

        elif isinstance(entry, Document):
            assert path.isabs(entry.filename)
            description = 'Document for {}: "<a href="{}" class="filename">{}</a>"'.format(
                account_link(entry.account),
                app.router.build('doc', filename=entry.filename.lstrip('/')),
                path.basename(entry.filename))
            change_str = ''
            balance_str = ''

        else:
            description = entry.__class__.__name__
            change_str = ''
            balance_str = ''

        yield (entry, leg_postings,
               rowtype, extra_class,
               flag, description, links, change_str, balance_str)


def entries_table_with_balance(app, oss, account_postings, render_postings=True):
    """Render a list of entries into an HTML table.
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

    for (entry, leg_postings,
         rowtype, extra_class,
         flag, description, links,
         change_str, balance_str) in iterate_render_transactions(app, account_postings):

        if links:
            description += render_links(links)

        # Render a row.
        write('''
          <tr class="{} {}" title="{}">
            <td class="datecell">{}</td>
            <td class="flag">{}</td>
            <td class="description" colspan="4">{}</td>
            <td class="change num">{}</td>
            <td class="balance num">{}</td>
          <tr>
        '''.format(rowtype, extra_class,
                   '{}:{}'.format(entry.fileloc.filename, entry.fileloc.lineno),
                   entry.date, flag, description, change_str, balance_str))

        if render_postings and isinstance(entry, Transaction):
            for posting in entry.postings:

                classes = ['Posting']
                if posting.flag == flags.FLAG_WARNING:
                    classes.append('warning')
                if posting in leg_postings:
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
                           account_link(posting.account),
                           posting.position,
                           posting.price or '',
                           complete.get_balance_amount(posting)))

    write('</table>')


def entries_table(app, oss, account_postings, render_postings=True):
    """Render a list of entries into an HTML table.
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

    for (entry, leg_postings,
         rowtype, extra_class,
         flag, description, links,
         _, _) in iterate_render_transactions(app, account_postings):

        if links:
            description += render_links(links)

        # Render a row.
        write('''
          <tr class="{} {}" title="{}">
            <td class="datecell">{}</td>
            <td class="flag">{}</td>
            <td class="description" colspan="5">{}</td>
          <tr>
        '''.format(rowtype, extra_class,
                   '{}:{}'.format(entry.fileloc.filename, entry.fileloc.lineno),
                   entry.date, flag, description))

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
                           account_link(posting.account),
                           posting.position.get_amount(),
                           posting.position.lot.cost or '',
                           posting.price or '',
                           complete.get_balance_amount(posting)))

    write('</table>')


def render_links(links):
    "Render the Transaction links as part of the description."
    return '<span class="links">{}</span>'.format(
        ''.join('<a href="{}">^</a>'.format(link)
                for link in links))
