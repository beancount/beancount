"""
Web server for Beancount ledgers.
This uses the Bottle single-file micro web framework (with no plugins).
"""
import argparse
import datetime
from os import path
from textwrap import dedent
import copy
import io
import re
import functools
from collections import defaultdict

import bottle
from bottle import response, request
import pandas
import numpy

from beancount2 import parser
from beancount2 import validation
from beancount2 import data
from beancount2.data import account_leaf_name, Account, Lot
from beancount2 import realization
from beancount2.realization import RealAccount
from beancount2 import summarize
from beancount2 import utils
from beancount2.utils import index_key
from beancount2.inventory import Inventory
from beancount2.data import Open, Close, Pad, Check, Transaction, Event, Note, Price, Posting


#--------------------------------------------------------------------------------
# Generic functions

escape = bottle.html_escape


class AppMapper:
    """A mapper used to format urls directly into templates.
    Use it in strings like this:

       <a href="{A.balsheet}">Balance Sheet</a>

    Where 'xxxx' in 'A.xxxxx' refers to a page by its name.
    """
    def __init__(self, url_function):
        self.url_function = url_function

    def __getattr__(self, aname):
        return self.url_function(aname)

G = AppMapper(bottle.default_app().router.build)


def app_url(name, *args, **kw):
    "Return a URL to the given name, in the request app."
    return request.app.get_url(name, *args, **kw)

A = AppMapper(app_url)


## FIXME: remove?
# def get_mount():
#     """Return the mountpoint of this application request call."""
#     return request.urlparts.path[:-len(request.path)]


#--------------------------------------------------------------------------------
# Global application pages.


def render_global(*args, **kw):
    """Render the title and contents in our standard template."""
    kw['G'] = G # Global mapper
    kw['A'] = A # Application mapper
    kw['title'] = bottle.default_app().contents.options['title']
    kw['view_title'] = ''
    kw['navigation'] = GLOBAL_NAVIGATION
    return template.render(*args, **kw)


@bottle.route('/', name='root')
def root():
    "Redirect the root page to the home page."
    bottle.redirect(bottle.url('toc'))


@bottle.route('/toc', name='toc')
def toc():
    mindate, maxdate = data.get_min_max_dates([entry for entry in clean_entries
                                               if not isinstance(entry, (Open, Close))])

    view_items = []
    for view in VIEWS:
        view_items.append('<li><a href="{}">{}</a></li>'.format(getattr(G, view.id),
                                                                view.title))

    return render_global(
        pagetitle = "Table of Contents",
        contents = """
          <h2>Views</h2>
          <ul>
            {view_items}
          </ul>
        """.format(view_items='\n'.join(view_items)))


@bottle.route('/errors', name='errors')
def errors():
    "Report error encountered during parsing, checking and realization."
    return render_global(
        pagetitle = "Errors",
        contents = ""
        )


@bottle.route('/stats', name='stats')
def stats():
    "Compute and render statistics about the input file."
    # Note: maybe the contents of this can fit on the home page, if this is simple.
    return render_global(
        pagetitle = "Statistics",
        contents = ""
        )


@bottle.route('/source', name='source')
def source():
    "Render the source file, allowing scrolling at a specific line."
    return render_global(
        pagetitle = "Source",
        contents = ""
        )


@bottle.route('/update', name='update')
def update():
    "Render the update activity."
    return render_global(
        pagetitle = "Update Activity",
        contents = ""
        )


@bottle.route('/events', name='events')
def update():
    "Render an index for the various kinds of events."
    return render_global(
        pagetitle = "Events",
        contents = ""
        )


@bottle.route('/prices', name='prices')
def prices():
    "Render information about prices."
    return render_global(
        pagetitle = "Prices",
        contents = ""
        )


GLOBAL_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{G.toc}}">Table of Contents</a></li>
  <li><a href="{{G.errors}}">Errors</a></li>
  <li><a href="{{G.source}}">Source</a></li>
  <li><a href="{{G.stats}}">Statistics</a></li>
  <li><a href="{{G.update}}">Update Activity</a></li>
  <li><a href="{{G.events}}">Events</a></li>
  <li><a href="{{G.prices}}">Prices</a></li>
</ul>
""").render(G=G)


@bottle.route('/style.css', name='style')
def style():
    "Stylesheet for the entire document."
    response.content_type = 'text/css'
    if bottle.default_app().args.debug:
        with open(path.join(path.dirname(__file__), 'style.css')) as f:
            global STYLE; STYLE = f.read()
    return STYLE


#--------------------------------------------------------------------------------
# Realization application pages.


app = bottle.Bottle()


def render_app(*args, **kw):
    """Render the title and contents in our standard template."""
    kw['G'] = G # Global mapper
    kw['A'] = A # Application mapper
    kw['title'] = bottle.default_app().contents.options['title']
    kw['view_title'] = ' - ' + request.app.view.title
    kw['navigation'] = APP_NAVIGATION.render(G=G, A=A, view_title=request.app.view.title)
    return template.render(*args, **kw)


@app.route('/', name='approot')
def approot():
    bottle.redirect(request.app.get_url('balsheet'))


@app.route('/reports', name='reports')
def reports():
    "The index of all the available reports for this realization."
    return render_app(
        pagetitle = "Index",
        contents = APP_NAVIGATION.render(G=G, A=A, view_title=request.app.view.title))


@app.route('/trial', name='trial')
def trial():
    "Trial balance / Chart of Accounts."
    return render_app(
        pagetitle = "Trial Balance",
        contents = ""
        )


EMS_PER_SPACE = 3
_account_link_cache = {}

def account_link(account_name):
    "Render an anchor for the given account name."
    if isinstance(account_name, (Account, RealAccount)):
        account_name = account_name.name
    try:
        return _account_link_cache[(request.app, account_name)]
    except KeyError:
        slashed_name = account_name.replace(':', '/')
        link = '<a href="{}" class="account">{}</a>'.format(
            request.app.get_url('account', slashed_account_name=slashed_name),
            account_leaf_name(account_name))
        _account_link_cache[account_name] = link
        return link


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

    lines = list(tree.render_lines(start_node_name))
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
        write('<td class="tree-node-name" style="padding-left: {}em">{}</td>'.format(
            len(line_first)/EMS_PER_SPACE,
            account_link(real_account)))

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

    oss = io.StringIO()
    for real_account, cells, row_classes in tree_table(oss, tree, start_node_name,
                                                       header, classes):

        # Check if this account has had activity; if not, skip rendering it.
        if real_account.name not in active_set:
            continue

        if real_account.account is None:
            row_classes.append('parent-node')

        # For each account line, get the final balance of the account (at cost).
        balance_cost = real_account.balance.get_cost()

        # Extract all the positions that the user has identified as home
        # currencies.
        positions = list(balance_cost.get_positions())
        for currency in currencies:
            position = balance_cost.get_position(Lot(currency, None, None))
            if position:
                positions.remove(position)
                cells.append('{:,.2f}'.format(position.number))
            else:
                cells.append('')

        # Render all the rest of the inventory in the last cell.
        cells.append('\n<br/>'.join(map(str, positions)))

    return oss.getvalue()







def balance_sheet_table(real_accounts, options):
    """Render an HTML balance sheet of the real_accounts tree."""

    currencies = options['currency']
    assets      = table_of_balances(real_accounts, options['name_assets'], currencies)
    liabilities = table_of_balances(real_accounts, options['name_liabilities'], currencies)
    equity      = table_of_balances(real_accounts, options['name_equity'], currencies)

    return """
           <div id="assets" class="halfleft">
            <h3>Assets</h3>
            {assets}
           </div>

           <div id="liabilities" class="halfright">
            <h3>Liabilities</h3>
            {liabilities}
           </div>

           <div class="spacer halfright">
           </div>

           <div id="equity" class="halfright">
            <h3>Equity</h3>
            {equity}
           </div>
        """.format(**vars())


@app.route('/balsheet', name='balsheet')
def balsheet():
    "Balance sheet."

    view = request.app.view
    real_accounts = request.app.view.get_closing_realization()
    contents = balance_sheet_table(real_accounts, view.options)

    return render_app(pagetitle = "Balance Sheet",
                      contents = contents)


@app.route('/openbal', name='openbal')
def openbal():
    "Opening balances."

    view = request.app.view
    real_accounts = request.app.view.get_opening_realization()
    if real_accounts is None:
        contents = 'N/A'
    else:
        contents = balance_sheet_table(real_accounts, view.options)

    return render_app(pagetitle = "Opening Balances",
                      contents = contents)


@app.route('/income', name='income')
def income():
    "Income statement."

    view = request.app.view
    real_accounts = request.app.view.get_realization()

    # Render the income statement tables.
    currencies = view.options['currency']
    income   = table_of_balances(real_accounts, view.options['name_income'], currencies)
    expenses = table_of_balances(real_accounts, view.options['name_expenses'], currencies)

    contents = """
       <div id="income" class="halfleft">
        <h3>Income</h3>
        {income}
       </div>

       <div id="expenses" class="halfright">
        <h3>Expenses</h3>
        {expenses}
       </div>
    """.format(**vars())

    return render_app(pagetitle = "Income Statement",
                      contents = contents)


@app.route('/conversions', name='conversions')
def conversions():
    "Render the list of transactions with conversions."
    return render_app(
        pagetitle = "Conversions",
        contents = ""
        )









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
            for date_entry, positions in date_entries:
                if positions:
                    # Compute the change due to this transaction and update the
                    # total balance at the same time.
                    change = Inventory()
                    for position in positions:
                        change.add_position(position, True)
                        balance.add_position(position, True)
                else:
                    change = None
                yield date_entry, change, balance
            date_entries.clear()

        if posting:

            # De-dup multiple postings on the same transaction entry by
            # grouping their positions together.
            index = index_key(date_entries, entry, key=first)
            if index is None:
                date_entries.append( (entry, [posting.position]) )
            else:
                # We are indeed de-duping!
                positions = date_entries[index][1]
                positions.append(posting.position)
        else:
            # This is a regular entry; nothing to add/remove.
            date_entries.append( (entry, None) )

    # Flush the final dated entries if any, same as above.
    for date_entry, positions in date_entries:
        if positions:
            change = Inventory()
            for position in positions:
                change.add_position(position, True)
                balance.add_position(position, True)
        yield date_entry, change, balance
    date_entries.clear()







FLAG_ROWTYPES = {
    data.FLAG_PADDING  : 'Padding',
    data.FLAG_SUMMARIZE: 'Summarize',
    data.FLAG_TRANSFER : 'Transfer',
    data.FLAG_WARNING : 'TransactionWarning',
}

def balance_html(balance):
    return ('\n<br/>'.join(map(str, balance.get_positions()))
            if balance
            else '')

def entries_table(oss, real_account):
    """Render a list of entries into an HTML table.
    """
    write = lambda data: (oss.write(data), oss.write('\n'))

    write('''
      <table class="entry-table">
      <thead>
        <tr>
         <th>Date</th>
         <th>F</th>
         <th>Narration/Payee</th>
         <th></th>
         <th>Change</th>
         <th>Balance</th>
      </thead>
    ''')


    postings = realization.get_subpostings(real_account)
    balance = Inventory()
    for entry, change, balance in iterate_with_balance(postings):

        # Prepare the data to be rendered for this row.
        date = entry.date
        balance_str = balance_html(balance)

        if isinstance(entry, Transaction):
            rowtype = FLAG_ROWTYPES.get(entry.flag, 'Transaction')
            flag = entry.flag
            description = '<span class="narration">{}</span>'.format(entry.narration)
            if entry.payee:
                description = '<span class="payee">{}</span><span class="pnsep">|</span>{}'.format(entry.payee, description)
            change_str = balance_html(change)
            cost_str = ''

        elif isinstance(entry, Check):
            # Check the balance here and possibly change the rowtype
            rowtype = entry.__class__.__name__

            flag = 'C'
            description = 'Check that {} contains {}'.format(entry.account.name, entry.position)
            change_str = str(entry.position)
            cost_str = ''

        else:
            rowtype = entry.__class__.__name__

            flag = ''
            description = entry.__class__.__name__
            change_str = ''
            cost_str = ''

        # Render a row.
        write('''
          <tr class="{}">
            <td class="datecell">{}</td>
            <td class="flag">{}</td>
            <td class="description">{}</td>
            <td class="number num">{}</td>
            <td class="change num">{}</td>
            <td class="balance num">{}</td>
          <tr>
        '''.format(rowtype, date, flag, description, cost_str, change_str, balance_str))

    write('</table>')


@app.route('/journal', name='journal')
def journal():
    "A list of all the entries in this realization."

    real_accounts = request.app.view.get_realization()

    oss = io.StringIO()
    entries_table(oss, real_accounts[''])

    return render_app(
        pagetitle = "Journal",
        contents = oss.getvalue())


@app.route('/account/<slashed_account_name:re:[^:]*>', name='account')
def account(slashed_account_name=None):
    "A list of all the entries for this account realization."

    real_accounts = request.app.view.get_realization()

    oss = io.StringIO()
    account_name = slashed_account_name.strip('/').replace('/', ':')
    entries_table(oss, real_accounts[account_name])

    return render_app(
        pagetitle = '{}'.format(account_name), # Account:
        contents = oss.getvalue())











@app.route('/positions', name='positions')
def positions():
    "Render information about positions at the end of all entries."

    entries = request.app.view.get_entries()

    total_balance = realization.compute_total_balance(entries)

    # FIXME: Make this into a nice table.

    # FIXME: Group the positions by currency, show the price of each position in
    # the inventory (see year 2006 for a good sample input).

    oss = io.StringIO()
    for position in total_balance.get_positions():
        if position.lot.cost or position.lot.lot_date:
            cost = position.get_cost()

            # print(('{p.number:12.2f} {p.lot.currency:8} '
            #        '{p.lot.cost.number:12.2f} {p.lot.cost.currency:8} '
            #        '{c.number:12.2f} {c.currency:8}').format(p=position, c=cost))
            # rows.append((position.lot.currency, position.lot.cost.currency,
            #              position.number, position.lot.cost.number, cost.number))

            oss.write('''
              <div class="position num">
                 {position}     {cost}
              </div>
            '''.format(position=position, cost=position.get_cost()))


    if 0:
        # Manipulate it a bit with Pandas.
        df = pandas.DataFrame(rows,
                              columns=['ccy', 'cost_ccy', 'units', 'unit_cost', 'total_cost'])

        # print(df.to_string())

        sums = df.groupby(['ccy', 'cost_ccy']).sum()

        total_cost = sums['total_cost'].astype(float)
        sums['percent'] = 100 * total_cost / total_cost.sum()

        sums.insert(2, 'average_cost', total_cost / sums['units'].astype(float))

    return render_app(
        pagetitle = "Positions",
        contents = oss.getvalue()
        )


@app.route('/trades', name='trades')
def trades():
    "Render a list of the transactions booked against inventory-at-cost."
    return render_app(
        pagetitle = "Trades",
        contents = ""
        )


@app.route('/documents', name='documents')
def documents():
    "Render a tree with the documents found for each."
    return render_app(
        pagetitle = "Documents",
        contents = ""
        )


APP_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{G.toc}}">Table of Contents</a></li>
  <li><span class="ledger-name">{{view_title}}:</span></li>
  <li><a href="{{A.openbal}}">Opening Balances</a></li>
  <li><a href="{{A.balsheet}}">Balance Sheet</a></li>
  <li><a href="{{A.income}}">Income Statement</a></li>
  <li><a href="{{A.trial}}">Trial Balance</a></li>
  <li><a href="{{A.journal}}">Journal</a></li>
  <li><a href="{{A.positions}}">Positions</a></li>
  <li><a href="{{A.conversions}}">Conversions</a></li>
  <li><a href="{{A.documents}}">Documents</a></li>
  <li><a href="{{A.reports}}">Reports</a></li>
</ul>
""")


#--------------------------------------------------------------------------------
# Views.


class View:
    """A container for filtering a subset of entries and realizing that for
    display."""

    def __init__(self, all_entries, options, id, title):

        # A reference to the full list of padded entries.
        self.all_entries = all_entries

        # List of filterered entries for this view, and index at the beginning
        # of the period transactions, past the opening balances. These are
        # computed lazily.
        self.entries = None
        self.opening_entries = None
        self.closing_entries = None

        # Id and title.
        self.id = id
        self.title = title

        # A reference to the global list of options.
        self.options = options

        # Realization of the filtered entries to display.
        self.real_accounts = None
        self.opening_real_accounts = None
        self.closing_real_accounts = None

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
        account_netincome = data.Account(account_netincome,
                                         data.account_type(account_netincome))

        self.closing_entries = summarize.transfer(self.entries, None,
                                                  data.is_income_statement_account, account_netincome)

        # Realize the three sets of entries.
        do_check = False
        if self.opening_entries:
            with utils.print_time('realize_opening'):
                self.opening_real_accounts, real_errors = realization.realize(self.opening_entries, do_check)
        else:
            self.opening_real_accounts = None
            data.print_errors(real_errors)

        with utils.print_time('realize'):
            self.real_accounts, real_errors = realization.realize(self.entries, do_check)
            data.print_errors(real_errors)

        with utils.print_time('realize_closing'):
            self.closing_real_accounts, real_errors = realization.realize(self.closing_entries, do_check)
            data.print_errors(real_errors)

        assert self.real_accounts is not None
        assert self.closing_real_accounts is not None

    def get_entries(self):
        """Return the list of entries for this view."""
        if self.real_accounts is None:
            self._realize()
        return self.entries

    def get_realization(self):
        """Return the realization associated with an app. This runs lazily; it filters
        the entries and realizes when the realization is needed. Thereafter, the
        realized accounts is cached in the app. """

        if self.real_accounts is None:
            self._realize()
        return self.real_accounts

    def get_opening_realization(self):
        """Return the realization at the index time."""

        if self.real_accounts is None:
            self._realize()
        return self.opening_real_accounts

    def get_closing_realization(self):
        """Return the realization that includes the closing transfers."""

        if self.real_accounts is None:
            self._realize()
        return self.closing_real_accounts

    def apply_filter(self, entries):
        "Filter the list of entries."
        raise NotImplementedError


class AllView(View):

    def apply_filter(self, entries, options):
        "Return the list of entries unmodified."
        return (entries, None)


class YearView(View):

    def __init__(self, entries, options, id, title, year):
        View.__init__(self, entries, options, id, title)

        # The year of filtering.
        self.year = year

    def apply_filter(self, entries, options):
        "Return entries for only that year."

        # Get the transfer account objects.
        #
        # FIXME: We should probably create these globally and then all fetch the
        # same instances.
        equity = options['name_equity']
        account_earnings = '{}:{}'.format(equity, options['account_earnings'])
        account_earnings = data.Account(account_earnings, data.account_type(account_earnings))

        account_opening = '{}:{}'.format(equity, options['account_opening'])
        account_opening = data.Account(account_opening, data.account_type(account_opening))

        # Clamp to the desired period.
        begin_date = datetime.date(self.year, 1, 1)
        end_date = datetime.date(self.year+1, 1, 1)
        with utils.print_time('clamp'):
            entries, index = summarize.clamp(entries,
                                             begin_date, end_date,
                                             account_earnings, account_opening)

        return entries, index


class TagView(View):

    def __init__(self, entries, options, id, title, tags):
        View.__init__(self, entries, options, id, title)

        # The tags we want to include.
        assert isinstance(tags, (set, list, tuple))
        self.tags = tags

    def apply_filter(self, entries, options):
        "Return only entries with the given tag."

        tags = self.tags
        tagged_entries = [entry
                          for entry in entries
                          if isinstance(entry, data.Transaction) and (entry.tags & tags)]

        return tagged_entries, None


class PayeeView(View):

    def __init__(self, entries, options, id, title, payee):
        View.__init__(self, entries, options, id, title)

        # The payee to filter.
        assert isinstance(payee, str)
        self.payee = payee

    def apply_filter(self, entries, options):
        "Return only transactions for the given payee."

        payee = self.payee
        payee_entries = [entry
                         for entry in entries
                         if isinstance(entry, data.Transaction) and (entry.payee == payee)]

        return payee_entries, None


#--------------------------------------------------------------------------------
# Bootstrapping and main program.


# A global list of all available ledgers (apps).
VIEWS = []


def app_mount(view):
    "Create and mount a new app for a view."

    # Create and customize the new app.
    app_copy = copy.copy(app)
    app_copy.view = view

    # Mount it on the root application.
    bottle.mount('/view/{}'.format(view.id), app_copy, name=view.id)

    # Update the global list of ledgers.
    VIEWS.append(view)


def create_realizations(entries, options):
    """Create apps for all the realizations we want to be able to render."""

    # The global realization, with all entries.
    app_mount(AllView(entries, options,
                      'all', 'All Transactions'))

    # One realization by-year.
    for year in reversed(list(data.get_active_years(entries))):
        view = YearView(entries, options,
                        'year{:4d}'.format(year), 'Year {:4d}'.format(year), year)
        app_mount(view)

    # Create views for all tags.
    for tagid, tag in compute_ids(get_all_tags(entries)):
        view = TagView(entries, options, tagid, 'Tag "{}"'.format(tag), {tag})
        app_mount(view)

    # FIXME: We need to make the payee mount different and "dynamic", createing
    # a new view automatically. We should do the same for the years and tags as
    # well. Creating the mounts is too expensive; views need to be created
    # on-demand, we need a special mount.
    if 0:
        # Create views for all payees.
        for payeeid, payee in compute_ids(get_all_payees(entries)):
            view = PayeeView(entries, options, payeeid, 'Payee "{}"'.format(payee), payee)
            app_mount(view)



def load_input_file(filename):
    """Parse the input file, pad the entries and validate it.
    This also prints out the error messages."""

    # Parse the input file.
    with utils.print_time('parse'):
        contents = parser.parse(filename)
        parse_errors = contents.parse_errors
        data.print_errors(contents.parse_errors)

    # Pad the resulting entries (create synthetic Pad entries to balance checks
    # where desired).
    with utils.print_time('pad'):
        entries, pad_errors = realization.pad(contents.entries)
        data.print_errors(pad_errors)

    # Validate the list of entries.
    with utils.print_time('validation'):
        valid_errors = validation.validate(entries, contents.accounts)
        data.print_errors(valid_errors)

    return contents, entries


def main():
    argparser = argparse.ArgumentParser(__doc__.strip())
    argparser.add_argument('filename', help="Beancount input filename to serve.")
    argparser.add_argument('--debug', action='store_true',
                           help="Enable debugging features (auto-reloading of css).")
    args = argparser.parse_args()

    # Parse the beancount file.
    #
    ## FIXME: maybe we can do away with this, and attach it to
    ## the global application class.
    global contents, clean_entries
    contents, clean_entries = load_input_file(args.filename)

    ## FIXME: Not sure what to do with errors yet.

    # Save globals in the global app.
    global_app = bottle.default_app()
    global_app.args = args
    global_app.contents = contents
    global_app.entries = clean_entries

    # Load templates.
    with open(path.join(path.dirname(__file__), 'template.html')) as f:
        global template
        template = bottle.SimpleTemplate(f)

    with open(path.join(path.dirname(__file__), 'style.css')) as f:
        global STYLE; STYLE = f.read()

    # Create all the basic realizations.
    create_realizations(clean_entries, contents.options)

    # Run the server.
    bottle.run(host='localhost', port=8080, debug=args.debug) # reloader=True









# FIXME: move this to data.py.

def get_all_tags(entries):
    "Return a list of all the tags seen in the given entries."
    all_tags = set()
    for entry in utils.filter_type(entries, data.Transaction):
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
