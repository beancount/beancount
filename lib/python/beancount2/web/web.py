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

import bottle
from bottle import response, request
import pandas
import numpy

from beancount2 import parser
from beancount2 import validation
from beancount2 import data
from beancount2 import realization
from beancount2 import summarize
from beancount2 import utils
from beancount2.inventory import Inventory
from beancount2.data import Open, Close, Pad, Check, Transaction, Event, Note, Price


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
    bottle.redirect(request.app.get_url('reports'))


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


@app.route('/openbal', name='openbal')
def openbal():
    "Opening balances."

    real_accounts = request.app.view.get_opening_realization()
    if real_accounts is None:
        contents = 'N/A'
    else:
        oss = io.StringIO()
        realization.dump_tree_balances(real_accounts, oss)
        contents = '<pre>{}</pre>'.format(escape(oss.getvalue()))

    return render_app(
        pagetitle = "Opening Balances",
        contents = contents
        )


@app.route('/balsheet', name='balsheet')
def balsheet():
    "Balance sheet."

    real_accounts = request.app.view.get_realization()
    oss = io.StringIO()
    realization.dump_tree_balances(real_accounts, oss)

    return render_app(
        pagetitle = "Balance Sheet",
        contents = '<pre>{}</pre>'.format(escape(oss.getvalue()))
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


@app.route('/journal', name='journal')
def journal():
    "A list of all the entries in this realization."

    real_accounts = request.app.view.get_realization()
    temp = temporary_render_real_account(real_accounts[''], transactions_only=True)

    return render_app(
        pagetitle = "Journal",
        contents = '<pre>{}</pre>'.format(escape(temp))
        )


@app.route('/account/<slashed_account_name:re:[^:]*>', name='account')
def account(slashed_account_name=None):
    "A list of all the entries for this account realization."

    account_name = slashed_account_name.strip('/').replace('/', ':')
    real_accounts = request.app.view.get_realization()
    temp = temporary_render_real_account(real_accounts[account_name], transactions_only=True)

    return render_app(
        pagetitle = 'Account: {}'.format(account_name),
        contents = '<pre>{}</pre>'.format(escape(temp))
        )

## FIXME: remove - this is temporary, until we get all rendering nicely, this will do for now
def temporary_render_real_account(real_account, transactions_only=False):

    real_postings = realization.get_real_subpostings(real_account)

    oss = io.StringIO()
    for real_posting in real_postings:
        entry = real_posting.entry
        entry_type = type(entry)
        if entry_type is Transaction:
            oss.write('{:%Y-%m-%d} {}  "{}"  {}\n'.format(
                entry.date, entry_type.__name__, entry.narration, real_posting.balance))
        elif not transactions_only:
            oss.write('{:%Y-%m-%d} {}\n'.format(entry.date, entry))

    return oss.getvalue()


@app.route('/positions', name='positions')
def positions():
    "Render information about positions at the end of all entries."

    real_accounts = request.app.view.get_realization()

    total_balance = Inventory()
    for real_account in real_accounts.values():
        for real_posting in reversed(real_account.postings):
            if isinstance(real_posting, realization.RealPosting):
                break
        else:
            continue
        total_balance += real_posting.balance

    rows = []
    for position in total_balance.get_positions():
        if position.lot.cost or position.lot.lot_date:
            cost = position.get_cost()
            # print(('{p.number:12.2f} {p.lot.currency:8} '
            #        '{p.lot.cost.number:12.2f} {p.lot.cost.currency:8} '
            #        '{c.number:12.2f} {c.currency:8}').format(p=position, c=cost))
            rows.append((position.lot.currency, position.lot.cost.currency,
                         position.number, position.lot.cost.number, cost.number))

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
        contents = '<pre>{}</pre>'.format(escape(sums.to_string()))
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
  <li><span class="ledger-name">{{view_title}}:</span></li>
  <li><a href="{{A.reports}}">Reports</a></li>
  <li><a href="{{A.openbal}}">Opening Balances</a></li>
  <li><a href="{{A.balsheet}}">Balance Sheet</a></li>
  <li><a href="{{A.income}}">Income Statement</a></li>
  <li><a href="{{A.trial}}">Trial Balance</a></li>
  <li><a href="{{A.journal}}">Journal</a></li>
  <li><a href="{{A.positions}}">Positions</a></li>
  <li><a href="{{A.conversions}}">Conversions</a></li>
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
        self.begin_index = None

        # Id and title.
        self.id = id
        self.title = title

        # A reference to the global list of options.
        self.options = options

        # Realization of the filtered entries to display.
        self.real_accounts = None

    def get_realization(self):
        """Return the realization associated with an app. This runs lazily; it filters
        the entries and realizes when the realization is needed. Thereafter, the
        realized accounts is cached in the app. """

        if self.real_accounts is None:
            # Get the filtered list of entries.
            self.entries, self.begin_index = self.apply_filter(self.all_entries, self.options)

            # Realize the full set of entries for the balance sheet.
            with utils.print_time('realize'):
                self.real_accounts, _ = realization.realize(self.entries, False)

            assert self.real_accounts is not None

        return self.real_accounts

    def get_opening_realization(self):
        """Return the realization at the index time. This is not cached for now, as this
        should be fast."""

        # Make sure we've realized.
        self.get_realization()

        # Check if it is supported by this filter. (Sometimes this doesn't make
        # sense: if the filtered entries haven't been summarized or don't have a
        # period of exercise, the opening balances is "all empty". The concept
        # of opening balances only makes sense for a summarized balance sheet.)
        if self.begin_index is None:
            return None

        # Realize the full set of entries for the balance sheet.
        with utils.print_time('realize'):
            openbal_entries = self.entries[:self.begin_index]
            openbal_real_accounts, _ = realization.realize(openbal_entries, False)
        assert openbal_real_accounts is not None
        return openbal_real_accounts

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
        account_earnings = options['account_earnings']
        account_earnings = data.Account(account_earnings, data.account_type(account_earnings))

        account_opening = options['account_opening']
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


#--------------------------------------------------------------------------------
# Bootstrapping and main program.


# A global list of all available ledgers (apps).
VIEWS = []


def app_mount(view):
    "Create and mount a new app for a ledger."

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
    all_tags = set()
    for entry in utils.filter_type(entries, data.Transaction):
        all_tags.update(entry.tags)

    for tag in sorted(all_tags):
        tagid = re.sub('[^A-Za-z0-9\-\.]', '', tag)
        view = TagView(entries, options, tagid, 'Tag {}'.format(tag), {tag})
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
