"""
Web server for Beancount ledgers.
This uses the Bottle single-file micro web framework (with no plugins).
"""
import argparse
import datetime
from os import path
import io
import re
from collections import defaultdict
from collections import defaultdict

import bottle
from bottle import response, request

from beancount.web.bottle_utils import AttrMapper, internal_redirect
from beancount.core.account import account_type
from beancount.core import data
from beancount.core.account import Account, account_leaf_name, is_account_root
from beancount.core.position import Lot
from beancount.core.data import Open, Close, Check, Transaction, Note, Document, Posting
from beancount.ops import summarize
from beancount.ops.balance import get_balance_amount
from beancount.core.inventory import Inventory
from beancount.core import realization
from beancount.core.realization import RealAccount
from beancount.ops import prices
from beancount import parser
from beancount import utils
from beancount.utils import index_key
from beancount.utils.text_utils import replace_numbers
from beancount.core.account import Account, account_type
from beancount.core.account import is_balance_sheet_account_name, is_income_statement_account
from beancount.core import flags


#--------------------------------------------------------------------------------
# Global application pages.


app = bottle.Bottle()
A = AttrMapper(app.router.build)


def render_global(*args, **kw):
    """Render the title and contents in our standard template."""
    response.content_type = 'text/html'
    kw['A'] = A # Application mapper
    kw['V'] = V # View mapper
    kw['title'] = app.options['title']
    kw['view_title'] = ''
    kw['navigation'] = GLOBAL_NAVIGATION
    return template.render(*args, **kw)


@app.route('/', name='root')
def root():
    "Redirect the root page to the home page."
    bottle.redirect(app.get_url('toc'))


@app.route('/toc', name='toc')
def toc():
    mindate, maxdate = data.get_min_max_dates([entry for entry in app.entries
                                               if not isinstance(entry, (Open, Close))])

    # Create links to all the possible views.
    views = []
    views.append((app.router.build('all', path=''), 'All Transactions'))

    for year in reversed(list(data.get_active_years(app.entries))):
        views.append((app.get_url('year', path='', year=year), 'Year: {}'.format(year)))

    for tag in get_all_tags(app.entries):
        views.append((app.get_url('tag', path='', tag=tag), 'Tag: {}'.format(tag)))

    view_items = ['<li><a href="{}">{}</a></li>'.format(url, title)
                  for url, title in views]
    return render_global(
        pagetitle = "Table of Contents",
        contents = """
          <h2>Views</h2>
          <ul>
            {view_items}
          </ul>
        """.format(view_items='\n'.join(view_items)))


@app.route('/errors', name='errors')
def errors():
    "Report error encountered during parsing, checking and realization."
    return render_global(
        pagetitle = "Errors",
        contents = ""
        )


@app.route('/stats', name='stats')
def stats():
    "Compute and render statistics about the input file."
    # Note: maybe the contents of this can fit on the home page, if this is simple.
    return render_global(
        pagetitle = "Statistics",
        contents = ""
        )


@app.route('/source', name='source')
def source():
    "Render the source file, allowing scrolling at a specific line."
    return render_global(
        pagetitle = "Source",
        contents = ""
        )


@app.route('/update', name='update')
def update():
    "Render the update activity."
    return render_global(
        pagetitle = "Update Activity",
        contents = ""
        )


@app.route('/events', name='events')
def update():
    "Render an index for the various kinds of events."
    return render_global(
        pagetitle = "Events",
        contents = ""
        )


@app.route('/prices', name='prices')
def prices_():
    "Render a list of links to instruments, to list their prices."

    links = ['<a href="{link}">{0} ({1})</a>'.format(
        base, quote,
        link = request.app.get_url('prices_graph', base=base, quote=quote))
             for (base, quote) in app.price_db]

    return render_global(
        pagetitle = "Prices",
        contents = """
          <ul>
            {}
          </ul>
        """.format('\n'.join(links)))

@app.route('/prices/<base:re:[A-Z]+>_<quote:re:[A-Z]+>', name='prices_graph')
def prices_graph(base=None, quote=None):


    print('prices_graph')

    # FIXME: TODO - Render as a gviz graph.







GLOBAL_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{A.toc}}">Table of Contents</a></li>
  <li><a href="{{A.errors}}">Errors</a></li>
  <li><a href="{{A.source}}">Source</a></li>
  <li><a href="{{A.stats}}">Statistics</a></li>
  <li><a href="{{A.update}}">Update Activity</a></li>
  <li><a href="{{A.events}}">Events</a></li>
  <li><a href="{{A.prices}}">Prices</a></li>
</ul>
""").render(A=A)


@app.route('/web.css', name='style')
def style():
    "Stylesheet for the entire document."
    response.content_type = 'text/css'
    if app.args.debug:
        with open(path.join(path.dirname(__file__), 'web.css')) as f:
            global STYLE; STYLE = f.read()
    return STYLE


@app.route('/doc/<filename:re:.*>', name='doc')
def doc(filename=None):
    "Serve static filenames for documents directives."

    filename = '/' + filename

    # Check that there is a document directive that has this filename.
    # This is for security; we don't want to be able to serve just any file.
    for entry in utils.filter_type(app.entries, Document):
        if entry.filename == filename:
            break
    else:
        raise bottle.HTTPError(404, "Not found.")

    # Just serve the file ourselves.
    return bottle.static_file(path.basename(filename),
                              path.dirname(filename))


#--------------------------------------------------------------------------------
# Realization application pages.


viewapp = bottle.Bottle()
V = AttrMapper(lambda *args, **kw: request.app.get_url(*args, **kw))


def handle_view(path_depth):
    """A decorator for handlers which create views lazily.
    If you decorate a method with this, the wrapper does the redirect
    handling and your method is just a factory for a View instance,
    which is cached."""

    def view_populator(callback):
        def wrapper(*args, **kwargs):
            components = request.path.split('/')
            viewid = '/'.join(components[:path_depth+1])
            try:
                # Try fetching the view from the cache.
                view = app.views[viewid]
            except KeyError:
                # We need to create the view.
                view = app.views[viewid] = callback(*args, **kwargs)

            # Save for hte subrequest and redirect. populate_view() picks this
            # up and saves it in request.view.
            request.environ['VIEW'] = view
            return internal_redirect(viewapp, path_depth)
        return wrapper
    return view_populator


def populate_view(callback):
    "A plugin that will populate the request with the current view instance."
    def wrapper(*args, **kwargs):
        request.view = request.environ['VIEW']
        return callback(*args, **kwargs)
    return wrapper

viewapp.install(populate_view)


def render_app(*args, **kw):
    """Render the title and contents in our standard template."""
    response.content_type = 'text/html'
    kw['A'] = A # Application mapper
    kw['V'] = V # View mapper
    kw['title'] = app.options['title']
    kw['view_title'] = ' - ' + request.view.title
    kw['navigation'] = APP_NAVIGATION.render(A=A, V=V, view_title=request.view.title)
    return template.render(*args, **kw)

APP_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{A.toc}}">Table of Contents</a></li>
  <li><span class="ledger-name">{{view_title}}:</span></li>
  <li><a href="{{V.openbal}}">Opening Balances</a></li>
  <li><a href="{{V.balsheet}}">Balance Sheet</a></li>
  <li><a href="{{V.income}}">Income Statement</a></li>
  <li><a href="{{V.trial}}">Trial Balance</a></li>
  <li><a href="{{V.journal}}">Journal</a></li>
  <li><a href="{{V.positions}}">Positions</a></li>
  <li><a href="{{V.conversions}}">Conversions</a></li>
  <li><a href="{{V.documents}}">Documents</a></li>
</ul>
""")


@viewapp.route('/', name='approot')
def approot():
    bottle.redirect(request.app.get_url('balsheet'))









EMS_PER_SPACE = 2.5
_account_link_cache = {}

def account_link(account_name, leafonly=False):
    "Render an anchor for the given account name."
    if isinstance(account_name, (Account, RealAccount)):
        account_name = account_name.name
    try:
        return _account_link_cache[(request.app, account_name)]
    except KeyError:
        slashed_name = account_name.replace(':', '/')

        if leafonly:
            account_name = account_leaf_name(account_name)

        link = '<a href="{}" class="account">{}</a>'.format(
            request.app.get_url('account', slashed_account_name=slashed_name),
            account_name)
        _account_link_cache[account_name] = link
        return link


# A special enum for the "Totals" line at the bottom of the table.
TOTALS_LINE = object()

def tree_table(oss, tree, start_node_name, header=None, classes=None):
    """Generator to a tree of accounts as an HTML table.
    Render only all the nodes under 'start_node_name'.
    This yields the real_account object for each line and a
    list object used to return the values for multiple cells.
    """
    write = lambda data: (oss.write(data), oss.write('\n'))

    write('<table class="tree-table {}">'.format(
        ' '.join(classes) if classes else ''))

    if header:
        write('<thead>')
        write('</tr>')
        header_iter = iter(header)
        write('<th class="first">{}</th>'.format(next(header_iter)))
        for column in header_iter:
            write('<th>{}</th>'.format(column))
        write('</tr>')
        write('</thead>')

    if start_node_name not in tree:
        write('</table>')
        return

    lines = list(tree.render_lines(start_node_name))

    # Yield with a None for the final line.
    lines.append((None, None, None, TOTALS_LINE))

    for line_first, _, account_name, real_account in lines:

        # Let the caller fill in the data to be rendered by adding it to a list
        # objects. The caller may return multiple cell values; this will create
        # multiple columns.
        cells = []
        row_classes = []
        yield real_account, cells, row_classes

        # If no cells were added, skip the line. If you want to render empty
        # cells, append empty strings.
        if not cells:
            continue

        # Render the row
        write('<tr class="{}">'.format(' '.join(row_classes)))

        if real_account is TOTALS_LINE:
            indent = '0'
            label = '<span class="totals-label">Totals</span>'
        else:
            indent = '{:.1f}'.format(len(line_first)/EMS_PER_SPACE)
            label = account_link(real_account, leafonly=True)

        write('<td class="tree-node-name" style="padding-left: {}em">{}</td>'.format(
            indent, label))

        # Add columns for each value rendered.
        for cell in cells:
            write('<td class="num">{}</td>'.format(cell))

        write('</tr>')

    write('</table>')


def is_account_active(real_account):
    """Return true if the account should be rendered. An inactive account only has
    an Open directive and nothing else."""

    for entry in real_account.postings:
        if isinstance(entry, Open):
            continue
        return True
    return False


def table_of_balances(tree, start_node_name, currencies, classes=None):
    """Render a table of balances."""

    header = ['Account'] + currencies + ['Other']

    # Pre-calculate which accounts should be rendered.
    active_accounts = tree.mark_from_leaves(is_account_active)
    active_set = set(real_account.name for real_account in active_accounts)

    balance_totals = Inventory()
    oss = io.StringIO()
    for real_account, cells, row_classes in tree_table(oss, tree, start_node_name,
                                                       header, classes):

        if real_account is TOTALS_LINE:
            balance = balance_totals
            row_classes.append('totals')
        else:
            # Check if this account has had activity; if not, skip rendering it.
            if (real_account.name not in active_set and
                not is_account_root(real_account.name)):
                continue

            if real_account.account is None:
                row_classes.append('parent-node')

            # For each account line, get the final balance of the account (at cost).
            balance = real_account.balance.get_cost()

            # Update the total balance for the totals line.
            balance_totals += balance

        # Extract all the positions that the user has identified as home
        # currencies.
        positions = list(balance.get_positions())

        for currency in currencies:
            position = balance.get_position(Lot(currency, None, None))
            if position:
                positions.remove(position)
                cells.append('{:,.2f}'.format(position.number))
            else:
                cells.append('')

        # Render all the rest of the inventory in the last cell.
        cells.append('<br/>'.join(map(str, positions)))

    return oss.getvalue()





@viewapp.route('/trial', name='trial')
def trial():
    "Trial balance / Chart of Accounts."

    view = request.view
    real_accounts = view.real_accounts
    operating_currencies = view.options['operating_currency']
    table = table_of_balances(real_accounts, '', operating_currencies, classes=['trial'])


    ## FIXME: After conversions is fixed, this should always be zero.
    total_balance = summarize.compute_total_balance(view.entries)
    table += """
      Total Balance: <span class="num">{}</span>
    """.format(total_balance.get_cost())

    return render_app(
        pagetitle = "Trial Balance",
        contents = table
        )


def balance_sheet_table(real_accounts, options):
    """Render an HTML balance sheet of the real_accounts tree."""

    operating_currencies = options['operating_currency']
    assets      = table_of_balances(real_accounts, options['name_assets'], operating_currencies)
    liabilities = table_of_balances(real_accounts, options['name_liabilities'], operating_currencies)
    equity      = table_of_balances(real_accounts, options['name_equity'], operating_currencies)

    return """
           <div class="halfleft">

             <div id="assets">
              <h3>Assets</h3>
              {assets}
             </div>

           </div>
           <div class="halfright">

             <div id="liabilities">
              <h3>Liabilities</h3>
              {liabilities}
             </div>
             <div class="spacer">
             </div>
             <div id="equity">
              <h3>Equity</h3>
              {equity}
             </div>

           </div>
        """.format(**vars())


@viewapp.route('/balsheet', name='balsheet')
def balsheet():
    "Balance sheet."

    view = request.view
    real_accounts = request.view.closing_real_accounts
    contents = balance_sheet_table(real_accounts, view.options)

    return render_app(pagetitle = "Balance Sheet",
                      contents = contents)


@viewapp.route('/openbal', name='openbal')
def openbal():
    "Opening balances."

    view = request.view
    real_accounts = request.view.opening_real_accounts
    if real_accounts is None:
        contents = 'N/A'
    else:
        contents = balance_sheet_table(real_accounts, view.options)

    return render_app(pagetitle = "Opening Balances",
                      contents = contents)


@viewapp.route('/income', name='income')
def income():
    "Income statement."

    view = request.view
    real_accounts = request.view.real_accounts

    # Render the income statement tables.
    operating_currencies = view.options['operating_currency']
    income   = table_of_balances(real_accounts, view.options['name_income'], operating_currencies)
    expenses = table_of_balances(real_accounts, view.options['name_expenses'], operating_currencies)

    contents = """
       <div id="income" class="halfleft">

         <div id="income">
          <h3>Income</h3>
          {income}
         </div>

       </div>
       <div class="halfright">

         <div id="expenses">
          <h3>Expenses</h3>
          {expenses}
         </div>

       </div>
    """.format(**vars())

    return render_app(pagetitle = "Income Statement",
                      contents = contents)















## FIXME: This deserves to be somewhere else, I'm thinking realization.py
def iterate_with_balance(entries):
    """Iterate over the entries accumulating the balance.
    For each entry, it yields

      (entry, change, balance)

    'entry' is the entry for this line. If the list contained Posting instance,
    this yields the corresponding Transaction object.

    'change' is an Inventory object that reflects the change due to this entry
    (this may be multiple positions in the case that a single transaction has
    multiple legs).

    The 'balance' yielded is never None; it's up to the one displaying the entry
    to decide whether to render for a particular type.

    Also, multiple postings for the same transaction are de-duped
    and when a Posting is encountered, the parent Transaction entry is yielded,
    with the balance updated for just the postings that were in the list.
    (We attempt to preserve the original ordering of the postings as much as
    possible.)
    """

    # The running balance.
    balance = Inventory()

    # Previous date.
    prev_date = None

    # A list of entries at the current date.
    date_entries = []

    first = lambda pair: pair[0]
    for entry in entries:

        # Get the posting if we are dealing with one.
        if isinstance(entry, Posting):
            posting = entry
            entry = posting.entry
        else:
            posting = None

        if entry.date != prev_date:
            prev_date = entry.date

            # Flush the dated entries.
            for date_entry, date_postings in date_entries:
                if date_postings:
                    # Compute the change due to this transaction and update the
                    # total balance at the same time.
                    change = Inventory()
                    for date_posting in date_postings:
                        change.add_position(date_posting.position, True)
                        balance.add_position(date_posting.position, True)
                else:
                    change = None
                yield date_entry, date_postings, change, balance

            date_entries.clear()
            assert not date_entries

        if posting is not None:
            # De-dup multiple postings on the same transaction entry by
            # grouping their positions together.
            index = index_key(date_entries, entry, key=first)
            if index is None:
                date_entries.append( (entry, [posting]) )
            else:
                # We are indeed de-duping!
                postings = date_entries[index][1]
                postings.append(posting)
        else:
            # This is a regular entry; nothing to add/remove.
            date_entries.append( (entry, None) )

    # Flush the final dated entries if any, same as above.
    for date_entry, date_postings in date_entries:
        if date_postings:
            change = Inventory()
            for date_posting in date_postings:
                change.add_position(date_posting.position, True)
                balance.add_position(date_posting.position, True)
        else:
            change = None
        yield date_entry, date_postings, change, balance
    date_entries.clear()








FLAG_ROWTYPES = {
    flags.FLAG_PADDING  : 'Padding',
    flags.FLAG_SUMMARIZE: 'Summarize',
    flags.FLAG_TRANSFER : 'Transfer',
}

def balance_html(balance):
    return ('<br/>'.join(map(str, balance.get_positions()))
            if balance
            else '')

def entries_table_with_balance(oss, account_postings, render_postings=True):
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

    balance = Inventory()
    for entry, leg_postings, change, balance in iterate_with_balance(account_postings):

        # Prepare the data to be rendered for this row.
        date = entry.date
        balance_str = balance_html(balance)

        rowtype = entry.__class__.__name__
        flag = ''
        extra_class = ''

        if isinstance(entry, Transaction):
            rowtype = FLAG_ROWTYPES.get(entry.flag, 'Transaction')
            extra_class = 'warning' if entry.flag == flags.FLAG_WARNING else ''
            flag = entry.flag
            description = '<span class="narration">{}</span>'.format(entry.narration)
            if entry.payee:
                description = '<span class="payee">{}</span><span class="pnsep">|</span>{}'.format(entry.payee, description)
            change_str = balance_html(change)

        elif isinstance(entry, Check):
            # Check the balance here and possibly change the rowtype
            if entry.errdiff is None:
                description = 'Check {} has {}'.format(account_link(entry.account), entry.amount)
            else:
                description = 'Check in {} fails; expected = {}, balance = {}, difference = {}'.format(
                    account_link(entry.account), entry.amount,
                    balance.get_amount(entry.amount.currency),
                    entry.errdiff)
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
                   date, flag, description, change_str, balance_str))

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


def entries_table(oss, account_postings, render_postings=True):
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

    balance = Inventory()
    for entry, leg_postings, change, balance in iterate_with_balance(account_postings):

        # Prepare the data to be rendered for this row.
        date = entry.date
        rowtype = entry.__class__.__name__
        flag = ''
        extra_class = ''

        if isinstance(entry, Transaction):
            rowtype = FLAG_ROWTYPES.get(entry.flag, 'Transaction')
            extra_class = 'warning' if entry.flag == flags.FLAG_WARNING else ''
            flag = entry.flag
            description = '<span class="narration">{}</span>'.format(entry.narration)
            if entry.payee:
                description = '<span class="payee">{}</span><span class="pnsep">|</span>{}'.format(entry.payee, description)
            change_str = balance_html(change)

        elif isinstance(entry, Check):
            # Check the balance here and possibly change the rowtype
            if entry.errdiff is None:
                description = 'Check {} has {}'.format(account_link(entry.account), entry.amount)
            else:
                description = 'Check in {} fails; expected = {}, balance = {}, difference = {}'.format(
                    account_link(entry.account), entry.amount,
                    balance.get_amount(entry.amount.currency),
                    entry.errdiff)
                rowtype = 'CheckFail'

        elif isinstance(entry, (Open, Close)):
            description = '{} {}'.format(entry.__class__.__name__, account_link(entry.account))

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

        # Render a row.
        write('''
          <tr class="{} {}" title="{}">
            <td class="datecell">{}</td>
            <td class="flag">{}</td>
            <td class="description" colspan="5">{}</td>
          <tr>
        '''.format(rowtype, extra_class,
                   '{}:{}'.format(entry.fileloc.filename, entry.fileloc.lineno),
                   date, flag, description))

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


@viewapp.route('/journal', name='journal')
def journal():
    "A list of all the entries in this realization."
    bottle.redirect(request.app.get_url('account', slashed_account_name=''))


@viewapp.route('/account/<slashed_account_name:re:[^:]*>', name='account')
def account(slashed_account_name=None):
    "A list of all the entries for this account realization."

    # Get the appropriate realization: if we're looking at the balance sheet, we
    # want to include the net-income transferred from the exercise period.
    account_name = slashed_account_name.strip('/').replace('/', ':')
    options = app.options
    if is_balance_sheet_account_name(account_name, options):
        real_accounts = request.view.closing_real_accounts
    else:
        real_accounts = request.view.real_accounts

    account_postings = realization.get_subpostings(real_accounts[account_name])

    oss = io.StringIO()
    entries_table_with_balance(oss, account_postings)
    return render_app(
        pagetitle = '{}'.format(account_name), # Account:
        contents = oss.getvalue())


def get_conversion_entries(entries):
    """Return the subset of transaction entries which have a conversion."""
    return [entry
            for entry in utils.filter_type(entries, Transaction)
            if data.transaction_has_conversion(entry)]


@viewapp.route('/conversions', name='conversions')
def conversions():
    "Render the list of transactions with conversions."

    view = request.view

    oss = io.StringIO()
    conversion_entries = get_conversion_entries(view.entries)
    entries_table(oss, conversion_entries, render_postings=True)

    balance = summarize.compute_total_balance(conversion_entries)

    return render_app(
        pagetitle = "Conversions",
        contents = """
          <div id="table">
            {}
          </div>
          <h3>Conversion Total:<span class="num">{}</span></h3>
        """.format(oss.getvalue(), balance))


#--------------------------------------------------------------------------------


FORMATTERS = {
    'number': '{:.3f}'.format,
    'book_value': '{:.0f}'.format,
    'market_value': '{:.0f}'.format,
    'pnl': '{:.2f}'.format,
    'avg_cost': '{:.3f}'.format,
    'price_number': '{:.3f}'.format,
    }


@viewapp.route('/positions', name='positions')
def positions():
    "Render an index of the pages detailing positions."
    return render_app(
        pagetitle = "Positions",
        contents = """
          <ul>
            <li><a href="{V.positions_detail}">Detailed Positions List</a></li>
            <li><a href="{V.positions_byinstrument}">By Instrument</a></li>
          </ul>
        """.format(V=V))


@viewapp.route('/positions/detail', name='positions_detail')
def positions_detail():
    "Render a detailed table of all positions."

    dataframe = prices.get_positions_as_dataframe(request.view.entries)
    if dataframe is None:
        return "You must install Pandas in order to render this page."

    oss = io.StringIO()
    oss.write("<center>\n")
    oss.write(dataframe.to_html(classes=['positions', 'detail-table']))
    oss.write("</center>\n")

    return render_app(
        pagetitle = "Positions - Detailed List",
        contents = oss.getvalue())


@viewapp.route('/positions/byinstrument', name='positions_byinstrument')
def positions_byinstrument():
    "Render a table of positions by instrument."

    dataframe = prices.get_positions_as_dataframe(request.view.entries)
    if dataframe is None:
        return "You must install Pandas in order to render this page."

    oss = io.StringIO()

    oss.write('<h2>With Account</h2>\n')

    byinst = dataframe.groupby(['account', 'currency', 'cost_currency'])
    byinst_agg = byinst['number', 'book_value', 'market_value', 'pnl'].sum()
    byinst_agg['avg_cost'] = byinst['cost_number'].mean()
    byinst_agg['price_number'] = byinst['price_number'].mean()
    byinst_agg = byinst_agg.sort('market_value', ascending=False)
    oss.write("<center>\n")
    oss.write(byinst_agg.to_html(classes=['positions', 'byinst-account-table'],
                                 formatters=FORMATTERS))
    oss.write("</center>\n")

    oss.write('<h2>Aggregated by Instrument Only</h2>\n')

    byinst = dataframe.groupby(['currency', 'cost_currency'])
    byinst_agg = byinst['number', 'book_value', 'market_value', 'pnl'].sum()
    byinst_agg['avg_cost'] = byinst['cost_number'].mean()
    byinst_agg['price_number'] = byinst['price_number'].mean()
    byinst_agg = byinst_agg.sort('market_value', ascending=False)
    oss.write("<center>\n")
    oss.write(byinst_agg.to_html(classes=['positions', 'byinst-table'],
                                 formatters=FORMATTERS))
    oss.write("</center>\n")

    return render_app(
        pagetitle = "Positions - By Instrument",
        contents = oss.getvalue())


#--------------------------------------------------------------------------------


@viewapp.route('/trades', name='trades')
def trades():
    "Render a list of the transactions booked against inventory-at-cost."
    return render_app(
        pagetitle = "Trades",
        contents = ""
        )


@viewapp.route('/documents', name='documents')
def documents():
    "Render a tree with all the documents found."
    document_entries = utils.filter_type(request.view.entries, Document)
    oss = io.StringIO()
    entries_table(oss, document_entries)
    return render_app(
        pagetitle = "Documents",
        contents = oss.getvalue())


#--------------------------------------------------------------------------------
# Views.


# A cache for views that have been created (on access).
app.views = {}


class View:
    """A container for filtering a subset of entries and realizing that for
    display."""

    def __init__(self, all_entries, options, title):

        # A reference to the full list of padded entries.
        self.all_entries = all_entries

        # List of filterered entries for this view, and index at the beginning
        # of the period transactions, past the opening balances. These are
        # computed in _realize().
        self.entries = None
        self.opening_entries = None
        self.closing_entries = None

        # Title.
        self.title = title

        # A reference to the global list of options and the account type names.
        self.options = options
        self.account_types = parser.get_account_types(options)

        # Realization of the filtered entries to display. These are computed in
        # _realize().
        self.real_accounts = None
        self.opening_real_accounts = None
        self.closing_real_accounts = None

        # Realize now, we don't need to do this lazily because we create these
        # view objects on-demand and cache them.
        self._realize()

    def _realize(self):
        """Compute the list of filtered entries and transaction tree."""

        # Get the filtered list of entries.
        self.entries, self.begin_index = self.apply_filter(self.all_entries, self.options)

        # Compute the list of entries for the opening balances sheet.
        self.opening_entries = (self.entries[:self.begin_index]
                                if self.begin_index is not None
                                else None)


        # Compute the list of entries that includes transfer entries of the
        # income/expenses amounts to the balance sheet's equity (as "net
        # income"). This is used to render the end-period balance sheet, with
        # the current period's net income, closing the period.
        equity = self.options['name_equity']
        account_netincome = '{}:{}'.format(equity, self.options['account_netincome'])
        account_netincome = Account(account_netincome,
                                    account_type(account_netincome))

        self.closing_entries = summarize.transfer(self.entries, None,
                                                  is_income_statement_account, account_netincome)

        # Realize the three sets of entries.
        do_check = False
        if self.opening_entries:
            with utils.print_time('realize_opening'):
                self.opening_real_accounts = realization.realize(self.opening_entries, do_check, self.account_types)
        else:
            self.opening_real_accounts = None

        with utils.print_time('realize'):
            self.real_accounts = realization.realize(self.entries, do_check, self.account_types)

        with utils.print_time('realize_closing'):
            self.closing_real_accounts = realization.realize(self.closing_entries, do_check, self.account_types)

        assert self.real_accounts is not None
        assert self.closing_real_accounts is not None

    def apply_filter(self, entries):
        "Filter the list of entries."
        raise NotImplementedError



class AllView(View):

    def apply_filter(self, entries, options):
        "Return the list of entries unmodified."
        return (entries, None)

@app.route(r'/view/all/<path:re:.*>', name='all')
@handle_view(2)
def all(path=None):
    return AllView(app.entries, app.options, 'All Transactions')



class YearView(View):

    def __init__(self, entries, options, title, year):
        self.year = year
        View.__init__(self, entries, options, title)

    def apply_filter(self, entries, options):
        "Return entries for only that year."

        # Get the transfer account objects.
        (account_opening,
         account_earnings,
         account_conversions) = parser.get_equity_accounts(options)

        # Clamp to the desired period.
        begin_date = datetime.date(self.year, 1, 1)
        end_date = datetime.date(self.year+1, 1, 1)
        with utils.print_time('clamp'):
            entries, index = summarize.clamp(entries,
                                             begin_date, end_date,
                                             account_earnings,
                                             account_opening,
                                             account_conversions)

        return entries, index

@app.route(r'/view/year/<year:re:\d\d\d\d>/<path:re:.*>', name='year')
@handle_view(3)
def year(year=None, path=None):
    year = int(year)
    return YearView(app.entries, app.options, 'Year {:4d}'.format(year), year)



class TagView(View):

    def __init__(self, entries, options, title, tags):
        # The tags we want to include.
        assert isinstance(tags, (set, list, tuple))
        self.tags = tags

        View.__init__(self, entries, options, title)

    def apply_filter(self, entries, options):
        "Return only entries with the given tag."

        tags = self.tags
        tagged_entries = [entry
                          for entry in entries
                          if isinstance(entry, data.Transaction) and entry.tags and (entry.tags & tags)]

        return tagged_entries, None

@app.route(r'/view/tag/<tag:re:[^/]*>/<path:re:.*>', name='tag')
@handle_view(3)
def tag(tag=None, path=None):
    return TagView(app.entries, app.options, 'Tag {}'.format(tag), set([tag]))


# ## FIXME: We need to figure out how to deal with id-ification for paths.
# We need some sort of mapping from idified tag to "real" tag. Either of don't idify at all.
# Is the syntax compatible?
#     # Create views for all tags.
#     for tagid, tag in compute_ids(get_all_tags(entries)):




class PayeeView(View):

    def __init__(self, entries, options, title, payee):
        # The payee to filter.
        assert isinstance(payee, str)
        self.payee = payee

        View.__init__(self, entries, options, title)

    def apply_filter(self, entries, options):
        "Return only transactions for the given payee."

        payee = self.payee
        payee_entries = [entry
                         for entry in entries
                         if isinstance(entry, data.Transaction) and (entry.payee == payee)]

        return payee_entries, None

@app.route(r'/view/payee/<payee:re:[^/]*>/<path:re:.*>', name='payee')
@handle_view(3)
def payee(payee=None, path=None):
    return PayeeView(app.entries, app.options, 'Payee {}'.format(payee), payee)



#--------------------------------------------------------------------------------
# Bootstrapping and main program.


# A global list of all available ledgers (apps).
VIEWS = []


def auto_reload_input_file(callback):
    """A plugin that automatically reloads the input file if it changed since the
    last page was loaded."""
    def wrapper(*posargs, **kwargs):

        filename = app.args.filename
        mtime = path.getmtime(filename)
        if mtime > app.last_mtime:
            app.last_mtime = mtime

            print('RELOADING')

            # Parse the beancount file.
            entries, errors, options = parser.load(filename,
                                                   add_unrealized_gains=True,
                                                   do_print_errors=True)

            # Save globals in the global app.
            app.entries = entries
            app.errors = errors
            app.options = options

            # Pre-compute the price database.
            app.price_db = prices.PriceDatabase(app.entries)

            # Reset the view cache.
            app.views.clear()

        return callback(*posargs, **kwargs)
    return wrapper

app.install(auto_reload_input_file)


def incognito(callback):
    """A plugin that converts all numbers rendered into X's, in order
    to hide the actual values in the ledger. This is used for doing
    public demos using my real ledger, where I don't necessarily
    want to share the detail of my financial life with the viewers
    but when I still want an interesting ledger, with enough
    detail that looks realistic."""

    def wrapper(*posargs, **kwargs):
        contents = callback(*posargs, **kwargs)
        if (response.content_type in ('text/html', '') and
            isinstance(contents, str)):
            contents = replace_numbers(contents)
        return contents

    return wrapper


def main():
    argparser = argparse.ArgumentParser(__doc__.strip())

    argparser.add_argument('filename', help="Beancount input filename to serve.")

    argparser.add_argument('--debug', action='store_true',
                           help="Enable debugging features (auto-reloading of css).")

    argparser.add_argument('--incognito', action='store_true',
                           help=("Filter the output in order to hide all the numbers. "
                                 "This is great for demos using my real file."))

    args = argparser.parse_args()
    app.args = args

    # Hide the numbers in incognito mode. We do this on response text via a plug-in.
    if args.incognito:
        app.install(incognito)
        viewapp.install(incognito)


    # Initialize to a small value in order to insure a reload on the first page.
    app.last_mtime = 0

    # Load templates.
    with open(path.join(path.dirname(__file__), 'web.html')) as f:
        global template
        template = bottle.SimpleTemplate(f)

    with open(path.join(path.dirname(__file__), 'web.css')) as f:
        global STYLE; STYLE = f.read()

    # # Create all the basic realizations.
    # create_realizations(clean_entries, options)

    # Run the server.
    app.run(host='localhost', port=8080, debug=args.debug, reloader=False)









# FIXME: move this to data.py.

def get_all_tags(entries):
    "Return a list of all the tags seen in the given entries."
    all_tags = set()
    for entry in utils.filter_type(entries, data.Transaction):
        if entry.tags:
            all_tags.update(entry.tags)
    return all_tags


def get_all_payees(entries):
    "Return a list of all the unique payees seen in the given entries."
    all_payees = set()
    for entry in utils.filter_type(entries, data.Transaction):
        all_payees.add(entry.payee)
    all_payees.discard(None)
    return all_payees


def compute_ids(strings):
    """Given a sequence of strings, reduce them to corresponding ids without any
    funny characters and insure that the list of ids is unique. Yields pairs
    of (id, string) for the result."""

    string_set = set(strings)

    # Try multiple methods until we get one that has no collisions.
    for regexp, replacement in [('[^A-Za-z0-9-.]', '_'),
                                ('[^A-Za-z0-9]', ''),]:

        # Map ids to strings.
        idmap = defaultdict(list)
        for string in string_set:
            id = re.sub(regexp, replacement, string)
            idmap[id].append(string)

        # Check for collisions.
        if all(len(stringlist) == 1 for stringlist in idmap.values()):
            break
    else:
        raise RuntimeError("Could not find a unique mapping for {}".format(string_set))

    return sorted((id, stringlist[0]) for id, stringlist in idmap.items())










# FIXME: Move the table rendering routines to their own files.
