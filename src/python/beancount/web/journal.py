"""HTML rendering routines for serving a lists of postings/entries.
ck"""
from os import path

from bottle import request

from beancount.core import data
from beancount.core import account
from beancount.core.data import Open, Close, Balance, Transaction, Note, Document
from beancount.core.balance import get_balance_amount
from beancount.core.inventory import Inventory
from beancount.core.account import Account, account_name_leaf
from beancount.core.realization import RealAccount
from beancount.core import realization
from beancount.core import flags


_account_link_cache = {}

def account_link(account_name, leafonly=False):
    "Render an anchor for the given account name."
    if isinstance(account_name, Account):
        account_name = account_name.name
    elif isinstance(account_name, RealAccount):
        account_name = account_name.fullname
    try:
        return _account_link_cache[(request.app, account_name)]
    except KeyError:
        slashed_name = account_name.replace(account.sep, '/')

        if leafonly:
            account_name = account_name_leaf(account_name)

        if hasattr(request, 'view'):
            link = '<a href="{}" class="account">{}</a>'.format(
                request.app.get_url('account', slashed_account_name=slashed_name),
                account_name)
            _account_link_cache[account_name] = link
            return link
        else:
            return account_name


FLAG_ROWTYPES = {
    flags.FLAG_PADDING  : 'Padding',
    flags.FLAG_SUMMARIZE: 'Summarize',
    flags.FLAG_TRANSFER : 'Transfer',
}

def balance_html(balance):
    """Render a list of balance position for an HTML table cell."""
    return ('<br/>'.join(map(str, balance.get_positions()))
            if not balance.is_empty()
            else '')


def iterate_render_transactions(app, postings):
    """Iterate through the list of transactions with rendered strings
    for each cell.
    Yields:
    """
    balance = Inventory()
    for entry, leg_postings, change, balance in realization.iterate_with_balance(postings):
        # Prepare the data to be rendered for this row.
        balance_str = balance_html(balance)

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
                description = '<span class="payee">{}</span><span class="pnsep">|</span>{}'.format(entry.payee, description)
            change_str = balance_html(change)

            if entry.links:
                links = [app.router.build('link', link=link)
                         for link in entry.links]

        elif isinstance(entry, Balance):
            # Check the balance here and possibly change the rowtype
            if entry.diff_amount is None:
                description = 'Balance {} has {}'.format(account_link(entry.account), entry.amount)
            else:
                description = 'Balance in {} fails; expected = {}, balance = {}, difference = {}'.format(
                    account_link(entry.account), entry.amount,
                    balance.get_amount(entry.amount.currency),
                    entry.diff_amount)
                rowtype = 'CheckFail'

            change_str = str(entry.amount)

        elif isinstance(entry, (Open, Close)):
            description = '{} {}'.format(entry.__class__.__name__, account_link(entry.account))
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
                           get_balance_amount(posting)))

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
                           get_balance_amount(posting)))

    write('</table>')


def render_links(links):
    "Render the Transaction links as part of the description."
    return '<span class="links">{}</span>'.format(
        ''.join('<a href="{}">^</a>'.format(link)
                for link in links))
