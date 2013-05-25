"""
Web server for Beancount ledgers.
This uses the Bottle single-file micro web framework (with no plugins).
"""
import argparse
from os import path
from textwrap import dedent
import copy

import bottle
from bottle import response, request

from beancount2 import parser
from beancount2 import checks
from beancount2 import data
from beancount2.data import Open, Close


#--------------------------------------------------------------------------------
# Generic functions


# def get_mount():
#     """Return the mountpoint of this application request call."""
#     return request.urlparts.path[:-len(request.path)]


def app_url(name, *args, **kw):
    """Return a URL to the given name.
    This returns the URL for the current realization app."""
    return request.app.get_url(name, *args, **kw)


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

A = AppMapper(app_url)
G = AppMapper(bottle.default_app().router.build)


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



GLOBAL_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{G.toc}}">Table of Contents</a></li>
  <li><a href="{{G.errors}}">Errors</a></li>
  <li><a href="{{G.source}}">Source</a></li>
  <li><a href="{{G.stats}}">Statistics</a></li>
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
    bottle.redirect(app_url('reports'))


@app.route('/reports', name='reports')
def reports():
    "The index of all the available reports for this realization."
    return render_app(
        pagetitle = "Index",
        contents = """
        <h2></h2>
        <ul>
          <li><a href="{A.balsheet}">Balance Sheet</a></li>
          <li><a href="{A.income}">Income Statement</a></li>
          <li><a href="{A.trial}">Trial Balance</a></li>
          <li><a href="{A.journal}">General Journal</a></li>
        </ul>
    """.format(A=A))


@app.route('/journal', name='journal')
def journal():
    "A list of all the entries in this realization."
    return render_app(
        pagetitle = "Journal",
        contents = ""
        )


@app.route('/trial', name='trial')
def trial():
    "Trial balance."
    return render_app(
        pagetitle = "Trial Balance",
        contents = ""
        )


@app.route('/balsheet', name='balsheet')
def balsheet():
    "Balance sheet."
    return render_app(
        pagetitle = "Balance Sheet",
        contents = ""
        )


@app.route('/income', name='income')
def income():
    "Income statement."
    return render_app(
        pagetitle = "Income Statement",
        contents = ""
        )


APP_NAVIGATION = bottle.SimpleTemplate("""
<ul>
  <li><a href="{{G.toc}}">Table of Contents</a></li>
  <li><span class="ledger-name">{{real_title}}:</span></li>
  <li><a href="{{A.reports}}">Reports</a></li>
  <li><a href="{{A.balsheet}}">Balance Sheet</a></li>
  <li><a href="{{A.income}}">Income Statement</a></li>
  <li><a href="{{A.trial}}">Trial Balance</a></li>
  <li><a href="{{A.journal}}">Journal</a></li>
</ul>
""")


#--------------------------------------------------------------------------------
# Bootstrapping and main program.


# A global list of all available ledgers (apps).
LEDGERS = []


def app_mount(real_id, real_title):
    "Create and mount a new app for a ledger."

    # Create and customize the new app.
    app_copy = copy.copy(app)
    app_copy.real_id = real_id
    app_copy.real_title = real_title

    # Mount it on the root application.
    bottle.mount('/real/{}'.format(real_id), app_copy, name=real_id)

    # Update the global list of ledgers.
    LEDGERS.append(app_copy)


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

    # print(template.render(G=G,
    #                       pagetitle='DOCTITLE',
    #                       navigation="<span>Nav</span>"
    #                       ))
    # raise SystemExit

    app_mount('all', 'All Transactions')

    for year in reversed(list(data.get_active_years(contents.entries))):

        # FIXME: We need to somehow attach the list of particular entries to the
        # app, to provide a unique title and a function that will
        # lazy-compute the filtered list of entries and its associated
        # realization.
        app_mount('year{:4d}'.format(year), 'Year {:4d}'.format(year))

    bottle.run(host='localhost', port=8080,
               # reloader=True,
               debug=True)





# print(bottle.url('balsheet', no=10))
# print(app.get_url('balsheet', no=11))

# Failed attempts at modifying the decorator so that it inserts named route with
# the name of the wrapped callback automatically. This is impossible; you need
# to modify bottle itself.

# def route(*args, **kw):
#     def named_route(fun):
#         print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX', fun.__name__)
#         kw['name'] = fun.__name__
#         return bottle.route(*args, **kw)
#     return named_route

# def route(*args, **kw):
#     wrapper = bottle.route(*args, **kw)
#     def named_route(fun):
#         kw['name'] = fun.__name__
#         return wrapper(fun)
#     return named_route

# def autoroute(*args, **kw):
#     """Invoke a modified decorator that will have the name of the wrapped function
#     as the name of the route."""
#     wrapper = bottle.route(*args, **kw)
#     def named_wrapper(callback):
#         name = callback.__name__
#         print(name)
#         return wrapper(callback)
#     return named_wrapper

#FIXME: I want to be able to render links with a global index of symbols, not as links, just like I did in my own thing.
# This makes everything nicer...
# def route(path, *args):
#     def decorator(fun):
#         fun = route(path, *args)


# FIXME: Offer 'autoroute' patch to Bottle.


# def render(pagetitle, contents, **kw):
#     """Render the title and contents in our standard template."""
#     output = bottle.template(template,
#                              pagetitle=pagetitle)
#     return output.format(navigation="", contents=dedent(contents))
