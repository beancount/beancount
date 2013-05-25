"""
Web server for Beancount ledgers.
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

# FIXME: Offer 'autoroute' patch to Bottle.

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
G = AppMapper(bottle.url)


# def render(doctitle, contents, **kw):
#     """Render the title and contents in our standard template."""
#     output = bottle.template(template,
#                              doctitle=doctitle)
#     return output.format(navigation="", contents=dedent(contents))

def render(*args, **kw):
    """Render the title and contents in our standard template."""
    kw['G'] = G # Global mapper
    kw['A'] = A # Application mapper
    return template.render(*args, **kw)


#--------------------------------------------------------------------------------
# Global application pages.

@bottle.route('/', name='groot')
def groot():
    "Redirect the root page to the home page."
    bottle.redirect(bottle.url('ghome'))

@bottle.route('/home', name='ghome')
def ghome():
    mindate, maxdate = data.get_min_max_dates([entry for entry in contents.entries
                                               if not isinstance(entry, (Open, Close))])

    years = '\n'.join(['<li><a href="{{G.year{:4d}}}">Year {}</a></li>'.format(year, year)
                       for year in range(mindate.year, maxdate.year + 1)])

    print('GHOME', G.ghome)

    return render(
        doctitle="Overview",
        contents="""
          <h2>Ledgers</h2>
          <ul>
            <li><a href="{{G.all}}">All Transactions</a></li>
            {years}
          </ul>
        """.format(years=years))



#--------------------------------------------------------------------------------
# Realization application pages.

app = bottle.Bottle()

def app_mount(name):
    app_copy = copy.copy(app)
    app_copy.title = name
    bottle.mount('/real/{}'.format(name), app_copy, name=name)


@app.route('/', name='root')
def root():
    bottle.redirect(app_url('index'))

@app.route('/index', name='index')
def index():
    return render(
        doctitle="Index",
        contents="""
        <h2>{{title}}</h2>
        <ul>
          <li><a href="{A.balsheet}">Balance Sheet</a></li>
          <li><a href="{A.income}">Income Statement</a></li>
          <li><a href="{A.trial}">Trial Balance</a></li>
          <li><a href="{A.journal}">General Journal</a></li>
        </ul>
    """,
        title='Index for {}'.format(request.app.title))

@app.route('/journal', name='journal')
def journal():
    return "JOURNAL"

@app.route('/trial', name='trial')
def trial():
    return "TRIAL"

@app.route('/balsheet', name='balsheet')
def balsheet():
    return "BALSHEET"

@app.route('/income', name='income')
def income():
    return "INCOME"




#--------------------------------------------------------------------------------
# Bootstrapping and main program.

def main():
    argparser = argparse.ArgumentParser(__doc__.strip())
    argparser.add_argument('filename', help="Beancount input filename to serve.")
    opts = argparser.parse_args()

    # Parse the beancount file.
    global contents ## FIXME: maybe we can do away with this, and attach it to
                    ## the global application class.
    contents = parser.parse(opts.filename)

    # Check for errors.
    errors = checks.check(contents.entries, contents.accounts)
    ## FIXME: Not sure what to do with errors yet.


    # Load templates.
    global template
    template = bottle.SimpleTemplate(open(path.join(path.dirname(__file__), 'template.html')))
    # print(template.render(G=G,
    #                       doctitle='DOCTITLE',
    #                       navigation="<span>Nav</span>"
    #                       ))
    # raise SystemExit

    app_mount('all')

    mindate, maxdate = data.get_min_max_dates([entry for entry in contents.entries
                                               if not isinstance(entry, (Open, Close))])
    for year in range(mindate.year, maxdate.year + 1):

        # FIXME: We need to somehow attach the list of particular entries to the
        # app, to provide a unique title and a function that will
        # lazy-compute the filtered list of entries and its associated
        # realization.
        app_mount('year{:4d}'.format(year))

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
