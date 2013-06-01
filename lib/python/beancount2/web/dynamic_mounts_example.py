#!/usr/bin/env python3
"""
Example Bottle application that mounts an app on dynamic routes.
"""
__author__ = 'Martin Blais <blais@furius.ca>'
__date__ = '2013-06-01'

import collections

import bottle
from bottle import request

from beancount2.web.bottle_utils import AppMapper, internal_redirect, populate_view


#--------------------------------------------------------------------------------
# Global pages.


template = bottle.SimpleTemplate("""
<html>
<body>

<h1>Global Page</h1>

{{!contents}}

</body>
</html>
""")


app = bottle.Bottle()


@app.route('/', name='root')
def root():
    "Redirect the root page to the home page."

    return template.render(contents="""
      <ul>

      <li><a href="{}">View: All</a></li>

      <li><a href="{}">View: 2010</a></li>
      <li><a href="{}">View: 2011</a></li>
      <li><a href="{}">View: 2012</a></li>
      <li><a href="{}">View: 2013</a></li>

      <li><a href="{}">Tag: Apples</a></li>
      <li><a href="{}">Tag: Oranges</a></li>
      <li><a href="{}">Tag: Bananas</a></li>

      </ul>
    """.format(
        app.router.build('all', path=''),
        app.get_url('year', path='', year=2010),
        app.get_url('year', path='', year=2011),
        app.get_url('year', path='', year=2012),
        app.get_url('year', path='', year=2013),
        app.get_url('tag', path='', tag='apples'),
        app.get_url('tag', path='', tag='oranges'),
        app.get_url('tag', path='', tag='bananas'),
    ))



#--------------------------------------------------------------------------------
# Views.




template_view = bottle.SimpleTemplate("""
<html>
<body>

<a href="{{!toc}}">Table of Contents</a>

<h1>View: {{!view}} - {{!contents}}</h1>

{{!navigation}}

</body>
</html>
""")


viewapp = bottle.Bottle()
viewapp.install(populate_view)


def render_view(*args, **kw):
    "Render a view's template with appropriate links for it."

    M = AppMapper(request.app)

    navigation = """
      <ul id="navigation">
      <li><a href="{M.balsheet}">Balance Sheet</a></li>
      <li><a href="{M.income}">Income Statement</a></li>
      <li><a href="{positions}">Positions at 2013-05-01</a></li>
      </ul>
    """.format(M=M,
               positions=M.build('positions', date='20130501'))

    return template_view.render(view=request.view.name,
                                navigation=navigation,
                                toc=app.router.build('root'),
                                *args, **kw)



@viewapp.route('/', name='viewroot')
def viewroot():
    bottle.redirect(request.app.get_url('balsheet'))


@viewapp.route('/balsheet', name='balsheet')
def balsheet():
    return render_view(contents="Balance Sheet")


@viewapp.route('/income', name='income')
def income():
    return render_view(contents="Income Statement")


@viewapp.route('/positions/<date:re:\d\d\d\d\d\d\d\d>', name='positions')
def positions(date):
    return render_view(contents="Positions at {}".format(date))


#--------------------------------------------------------------------------------
# Setup.


# Get views (with caching).
# The real version of this class actually contains a lot of stuff.
View = collections.namedtuple('View', 'name')

VIEW_CACHE = {}

def get_view(name):
    try:
        view = VIEW_CACHE[name]
    except KeyError:
        print("MATERIALIZING VIEW", name)
        view = VIEW_CACHE[name] = View(name)
    return view


# Map /view/all/...

@app.route(r'/view/all/<path:re:.*>', name='all')
def all(path=None):
    request.environ['VIEW'] = get_view('all')
    return internal_redirect(viewapp, 2)


# Map /view/year/YYYY/...

@app.route(r'/view/year/<year:re:\d\d\d\d>/<path:re:.*>', name='year')
def year(year=None, path=None):
    request.environ['VIEW'] = get_view('year/{:4}'.format(int(year)))
    return internal_redirect(viewapp, 3)


# Map /view/tag/TAGNAME/...

@app.route(r'/view/tag/<tag:re:[^/]*>/<path:re:.*>', name='tag')
def year(tag=None, path=None):
    request.environ['VIEW'] = get_view('tag/{}'.format(tag))
    return internal_redirect(viewapp, 3)




def main():
    app.run(host='localhost', port=8080, debug=True)

if __name__ == '__main__':
    main()
