"""
Web server for Beancount ledgers.
This uses the Bottle single-file micro web framework (with no plugins).
"""
import argparse
from os import path
import io

import bottle
from bottle import response, request

from beancount.web.bottle_utils import AttrMapper, internal_redirect
from beancount.web import views
from beancount.web import journal
from beancount.web import acctree
from beancount.core import data
from beancount.core.data import Open, Close, Transaction, Document
from beancount.core import getters
from beancount.ops import summarize
from beancount.core import realization
from beancount.ops import prices
from beancount import utils
from beancount.utils.text_utils import replace_numbers
from beancount.core.account import is_balance_sheet_account_name
from beancount.loader import load


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
    mindate, maxdate = getters.get_min_max_dates([entry for entry in app.entries
                                                  if not isinstance(entry, (Open, Close))])

    # Create links to all the possible views.
    views = []
    views.append((app.router.build('all', path=''), 'All Transactions'))

    for year in reversed(list(getters.get_active_years(app.entries))):
        views.append((app.get_url('year', path='', year=year), 'Year: {}'.format(year)))

    for tag in getters.get_all_tags(app.entries):
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
# View application pages.


viewapp = bottle.Bottle()
V = AttrMapper(lambda *args, **kw: request.app.get_url(*args, **kw))


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
  <li><a href="{{V.equity}}">Shareholder's Equity</a></li>
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
















@viewapp.route('/trial', name='trial')
def trial():
    "Trial balance / Chart of Accounts."

    view = request.view
    real_accounts = view.real_accounts
    operating_currencies = view.options['operating_currency']
    table = acctree.table_of_balances(real_accounts, '', operating_currencies, classes=['trial'])


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
    assets      = acctree.table_of_balances(real_accounts, options['name_assets'], operating_currencies)
    liabilities = acctree.table_of_balances(real_accounts, options['name_liabilities'], operating_currencies)
    equity      = acctree.table_of_balances(real_accounts, options['name_equity'], operating_currencies)

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
    income   = acctree.table_of_balances(real_accounts, view.options['name_income'], operating_currencies)
    expenses = acctree.table_of_balances(real_accounts, view.options['name_expenses'], operating_currencies)

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


@viewapp.route('/equity', name='equity')
def equity():
    "Render a table of the net worth at the beginning, end, and net income."

    view = request.view

    balance = summarize.compute_balance_for_prefix(view.closing_entries,
                                                   view.options['name_equity'] + ':')
    header = io.StringIO()
    header.write('<th>Currency</th>\n')
    header.write('<th>Amount</th>\n')
    operating_currencies = view.options['operating_currency']
    header.write('\n'.join('<th>{}</th>\n'.format(currency)
                           for currency in operating_currencies))

    body = io.StringIO()
    for position in balance.get_positions():
        body.write('<tr>')
        body.write('<td>{}</td>'.format(position.lot.currency))
        body.write('<td>{}</td>'.format(position.number))

        ## FIXME: complete this.
        body.write('\n'.join('<th>{}</th>\n'.format('FIXME')
                             for currency in operating_currencies))
        body.write('</tr>')

    contents = """
       <div id="equity-table">
         <table>
           <thead>
             {header}
           </thead>
           <tbody>
             {body}
           </tbody>
         </table>
       </div>
    """.format(header=header.getvalue(), body=body.getvalue())

    ## FIXME: Render the equity at opening too.
    ## FIXME: Insert a summary of the net income.

    return render_app(pagetitle = "Shareholder's Equity",
                      contents = contents)


@viewapp.route('/journal', name='journal')
def journal_():
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
    journal.entries_table_with_balance(app, oss, account_postings)
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
    journal.entries_table(app, oss, conversion_entries, render_postings=True)

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
    journal.entries_table(app, oss, document_entries)
    return render_app(
        pagetitle = "Documents",
        contents = oss.getvalue())


#--------------------------------------------------------------------------------
# Views.


# A cache for views that have been created (on access).
app.views = {}


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


@app.route(r'/view/all/<path:re:.*>', name='all')
@handle_view(2)
def all(path=None):
    return views.AllView(app.entries, app.options, 'All Transactions')


@app.route(r'/view/year/<year:re:\d\d\d\d>/<path:re:.*>', name='year')
@handle_view(3)
def year(year=None, path=None):
    year = int(year)
    return views.YearView(app.entries, app.options, 'Year {:4d}'.format(year), year)


@app.route(r'/view/tag/<tag:re:[^/]*>/<path:re:.*>', name='tag')
@handle_view(3)
def tag(tag=None, path=None):
    return views.TagView(app.entries, app.options, 'Tag {}'.format(tag), set([tag]))


@app.route(r'/view/payee/<payee:re:[^/]*>/<path:re:.*>', name='payee')
@handle_view(3)
def payee(payee=None, path=None):
    return views.PayeeView(app.entries, app.options, 'Payee {}'.format(payee), payee)


# ## FIXME: We need to figure out how to deal with id-ification for paths.
# We need some sort of mapping from idified tag to "real" tag. Either of don't idify at all.
# Is the syntax compatible?
#     # Create views for all tags.
#     for tagid, tag in utils.compute_ids(get_all_tags(entries)):



#--------------------------------------------------------------------------------
# Bootstrapping and main program.


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
            entries, errors, options = load(filename,
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

    # Run the server.
    app.run(host='localhost', port=8080, debug=args.debug, reloader=False)
