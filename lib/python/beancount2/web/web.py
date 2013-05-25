"""
Web server for Beancount ledgers.
This uses the Bottle single-file micro web framework (with no plugins).
"""
import argparse
from os import path
from textwrap import dedent
import copy
import io
import functools

import bottle
from bottle import response, request

from beancount2 import parser
from beancount2 import checks
from beancount2 import data
from beancount2 import realization
from beancount2.data import Open, Close


#--------------------------------------------------------------------------------
# Generic functions


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
    kw['real_title'] = ''
    kw['navigation'] = GLOBAL_NAVIGATION
    return template.render(*args, **kw)


@bottle.route('/', name='root')
def root():
    "Redirect the root page to the home page."
    bottle.redirect(bottle.url('toc'))


@bottle.route('/toc', name='toc')
def toc():
    mindate, maxdate = data.get_min_max_dates([entry for entry in contents.entries
                                               if not isinstance(entry, (Open, Close))])

    ledger_items = []
    for ledger in LEDGERS:
        ledger_items.append('<li><a href="{}">{}</a></li>'.format(getattr(G, ledger.real_id),
                                                                  ledger.real_title))

    return render_global(
        pagetitle = "Table of Contents",
        contents = """
          <h2>Ledgers</h2>
          <ul>
            {ledger_items}
          </ul>
        """.format(ledger_items='\n'.join(ledger_items)))


@bottle.route('/errors', name='errors')
def errors():
    "Report error encountered during parsing, checking and realization."
    return render_global(
        pagetitle = "Errors",
        contents = ""
        )


@bottle.route('/stats', name='stats')
def stats():
    "Compute and render statistics about the ledger."
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


@bottle.route('/positions', name='positions')
def positions():
    "Render information about positions."
    return render_global(
        pagetitle = "Positions",
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
  <li><a href="{{G.positions}}">Positions</a></li>
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
    kw['real_title'] = ' - ' + request.app.real_title
    kw['navigation'] = APP_NAVIGATION.render(G=G, A=A, real_title=request.app.real_title)
    return template.render(*args, **kw)


@app.route('/', name='approot')
def approot():
    bottle.redirect(request.app.get_url('reports'))


@app.route('/reports', name='reports')
def reports():
    "The index of all the available reports for this realization."
    return render_app(
        pagetitle = "Index",
        contents = APP_NAVIGATION.render(G=G, A=A, real_title=request.app.real_title))


@app.route('/journal', name='journal')
def journal():
    "A list of all the entries in this realization."

    real_accounts = get_realization(request.app)
    real_postings = realization.get_real_subpostings(real_accounts[''])

    oss = io.StringIO()
    for real_posting in real_postings:
        if isinstance(real_posting, (RealPosting, RealPadPosting, RealCheck)):
            entry = real_posting.entry
        else:
            entry = real_posting
        oss.write(str(entry))

    return render_app(
        pagetitle = "Journal",
        contents = '<pre>{}</pre>'.format(oss.getvalue())
        )


@app.route('/trial', name='trial')
def trial():
    "Trial balance / Chart of Accounts."
    return render_app(
        pagetitle = "Trial Balance",
        contents = ""
        )


@app.route('/balsheet', name='balsheet')
def balsheet():
    "Balance sheet."

    real_accounts = get_realization(request.app)
    oss = io.StringIO()
    realization.dump_tree_balances(real_accounts, oss)

    return render_app(
        pagetitle = "Balance Sheet",
        contents = '<pre>{}</pre>'.format(oss.getvalue())
        )


@app.route('/income', name='income')
def income():
    "Income statement."
    return render_app(
        pagetitle = "Income Statement",
        contents = ""
        )


@app.route('/conversions', name='conversions')
def positions():
    "Render the list of transactions with conversions."
    return render_app(
        pagetitle = "Conversions",
        contents = ""
        )


@app.route('/account/<account_name>', name='account')
def account(account_name=None):
    "A list of all the entries for this account realization."

    return render_app(
        pagetitle = 'Account: {}'.format(account_name),
        contents = "(Account entries)"
        )


# Opening Balance Sheet
#         LI(A("Capital Statement", href=umap('@@CapitalStatement')),
#         LI(A('Cash Flow Statement', href=umap('@@CashFlow'))),
# Payees
# Tags
# Trades



APP_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{G.toc}}">Table of Contents</a></li>
  <li><span class="ledger-name">{{real_title}}:</span></li>
  <li><a href="{{A.reports}}">Reports</a></li>
  <li><a href="{{A.balsheet}}">Balance Sheet</a></li>
  <li><a href="{{A.income}}">Income Statement</a></li>
  <li><a href="{{A.trial}}">Trial Balance</a></li>
  <li><a href="{{A.journal}}">Journal</a></li>
  <li><a href="{{A.conversions}}">Conversions</a></li>
</ul>
""")


#--------------------------------------------------------------------------------
# Bootstrapping and main program.


# A global list of all available ledgers (apps).
LEDGERS = []


def app_mount(real_id, real_title, filter_function):
    "Create and mount a new app for a ledger."

    # Create and customize the new app.
    app_copy = copy.copy(app)
    app_copy.real_id = real_id
    app_copy.real_title = real_title
    app_copy.filter_function = filter_function
    app_copy.entries = None
    app_copy.real_accounts = None

    # Mount it on the root application.
    bottle.mount('/real/{}'.format(real_id), app_copy, name=real_id)

    # Update the global list of ledgers.
    LEDGERS.append(app_copy)


def get_realization(app):
    """Return the realization associated with an app. This runs lazily; it filters
    the entries and realizes when the realization is needed. Thereafter, the
    realized accounts is cached in the app. """

    if app.real_accounts is None:
        # Get the filtered list of entries.
        contents = bottle.default_app().contents
        app.entries = app.filter_function(contents.entries)

        # Realize them in a hierarchy.
        app.real_accounts, _ = realization.realize(app.entries, False)
        assert app.real_accounts is not None

    return app.real_accounts


def create_realizations(contents):
    """Create apps for all the realizations we want to be able to render."""

    # The global realization, with all entries.
    app_mount('all', 'All Transactions', filter_entries_all)

    # One realization by-year.
    for year in reversed(list(data.get_active_years(contents.entries))):

        # FIXME: We need to somehow attach the list of particular entries to the
        # app, to provide a unique title and a function that will
        # lazy-compute the filtered list of entries and its associated
        # realization.
        app_mount('year{:4d}'.format(year),
                  'Year {:4d}'.format(year),
                  functools.partial(filter_entries_byyear, year))


def filter_entries_all(entries):
    "Return the list of entries unmodified."
    return entries

def filter_entries_byyear(year, entries):
    "Return entries for only that year."
    return [entry
            for entry in entries
            if entry.date.year == year]


def main():
    argparser = argparse.ArgumentParser(__doc__.strip())
    argparser.add_argument('filename', help="Beancount input filename to serve.")
    argparser.add_argument('--debug', action='store_true',
                           help="Enable debugging features (auto-reloading of css).")
    args = argparser.parse_args()

    # Parse the beancount file.
    global contents ## FIXME: maybe we can do away with this, and attach it to
                    ## the global application class.
    contents = parser.parse(args.filename)

    # Check for errors.
    errors = checks.check(contents.entries, contents.accounts)
    ## FIXME: Not sure what to do with errors yet.

    # Save globals in the global app.
    global_app = bottle.default_app()
    global_app.args = args
    global_app.contents = contents

    # Load templates.
    with open(path.join(path.dirname(__file__), 'template.html')) as f:
        global template
        template = bottle.SimpleTemplate(f)

    with open(path.join(path.dirname(__file__), 'style.css')) as f:
        global STYLE; STYLE = f.read()

    # Create all the basic realizations.
    create_realizations(contents)

    # Run the server.
    bottle.run(host='localhost', port=8080, debug=args.debug) # reloader=True
