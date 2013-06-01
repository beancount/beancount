#!/usr/bin/env python3
"""
Example Bottle application that mounts an app on dynamic routes.
"""
__author__ = 'Martin Blais <blais@furius.ca>'
__date__ = '2013-06-01'

import collections

import bottle
from bottle import request


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


class Mapper:
    "A URL mapper that is required to render view links from global pages."

    def __init__(self, name, **kwargs):
        self.global_name = name
        self.global_kwargs = kwargs
        if self.global_kwargs is None:
            self.global_kwargs = {}

    def __getattr__(self, aname):
        "Convenience for using the new Python formatting syntax with attribute access."
        return self.build(name)

    def build(self, name, **kwargs):
        # Render the within-view url.
        if kwargs is None:
            kwargs = {}
        view_url = viewapp.router.build(name, **kwargs)
        view_url = view_url.lstrip('/')

        # Render the global url for that specific view.
        return app.router.build(self.global_name, view_url, **self.global_kwargs)


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
        Mapper('all').build('balsheet'),
        Mapper('year', year=2010).build('balsheet'),
        Mapper('year', year=2011).build('balsheet'),
        Mapper('year', year=2012).build('balsheet'),
        Mapper('year', year=2013).build('balsheet'),
        Mapper('tag', tag='apples').build('balsheet'),
        Mapper('tag', tag='oranges').build('balsheet'),
        Mapper('tag', tag='bananas').build('balsheet'),
    ))



#--------------------------------------------------------------------------------
# Views.


class ViewMapper:
    "A URL mapper that allows attribute access for view-links."

    def __init__(self, app):
        self.app = app

    def __getattr__(self, aname):
        return self.app.get_url(aname)

    def build(self, name, **kwargs):
        return self.app.get_url(name, **kwargs)


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


def get_request_view():
    "Return the current request's associated view."
    return request.environ['VIEW']


def render_view(*args, **kw):
    "Render a view's template with appropriate links for it."

    M = ViewMapper(request.app)

    navigation = """
      <ul id="navigation">
      <li><a href="{M.balsheet}">Balance Sheet</a></li>
      <li><a href="{M.income}">Income Statement</a></li>
      <li><a href="{positions}">Positions at 2013-05-01</a></li>
      </ul>
    """.format(M=M,
               positions=M.build('positions', date='20130501'))

    return template_view.render(view=get_request_view().name,
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

@app.route(r'/view/all/<:re:.*>', name='all')
def all():
    request.environ['VIEW'] = get_view('all')
    return internal_redirect(viewapp, 2)


# Map /view/year/YYYY/...

@app.route(r'/view/year/<year:re:\d\d\d\d>/<:re:.*>', name='year')
def year(year=None):
    request.environ['VIEW'] = get_view('year/{:4}'.format(int(year)))
    return internal_redirect(viewapp, 3)


# Map /view/tag/TAGNAME/...

@app.route(r'/view/tag/<tag:re:[^/]*>/<:re:.*>', name='tag')
def year(tag=None):
    request.environ['VIEW'] = get_view('tag/{}'.format(tag))
    return internal_redirect(viewapp, 3)


def internal_redirect(app, path_depth):
    """A version of mountpoint_wrapper() that we call internally.
    This is directly lifted from Bottle.mount() and edited
    minimally."""
    try:
        request.path_shift(path_depth)
        rs = bottle.HTTPResponse([])
        def start_response(status, headerlist, exc_info=None):
            if exc_info:
                try:
                    _raise(*exc_info)
                finally:
                    exc_info = None
            rs.status = status
            for name, value in headerlist: rs.add_header(name, value)
            return rs.body.append
        body = app(request.environ, start_response)
        if body and rs.body: body = itertools.chain(rs.body, body)
        rs.body = body or rs.body
        return rs
    finally:
        request.path_shift(-path_depth)


def main():
    app.run(host='localhost', port=8080, debug=True)

if __name__ == '__main__':
    main()
