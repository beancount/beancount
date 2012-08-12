"""
Start a simple web server to display the contents of some Ledger.

We keep this code simplistic, and away from growing into a full-fledged web app
framework as much as possible on purpose! The web server code need not be super
powerful: as simple as possible.
"""

# stdlib imports
import sys, re, cgitb, logging, cgi, traceback
from time import time
from random import randint
from wsgiref.simple_server import make_server
from wsgiref.util import request_uri, application_uri
from wsgiref.headers import Headers
from StringIO import StringIO
from os.path import *
from copy import copy
from decimal import Decimal
import Cookie

# beancount imports
from beancount import cmdline
from beancount.utils import TimerUtil


__all__ = ('main', 'Mapper',
           'HttpError', 'HttpNotFound', 'HttpRedirect')


# HTTP errors.

class HttpError(Exception):
    code = None

class HttpNotFound(Exception):
    code = 404
    status = '404 Not Found'

class HttpRedirect(Exception):
    code = 302
    status = '302 Found'



class BeanServer(object):
    "A really, really simple application server."

    default_headers = [('Content-Type', 'text/html')]

    def __init__(self, ledger, opts):
        self.ledger = ledger

        self.data = []
        self.load()

        # Map of session to dict.
        self.cookiejar = {}

        # Prototype for context object.
        ctx = self.ctx = Context()
        self.opts = ctx.opts = opts
        ctx.debug = opts.debug

    def setHeader(self, name, value):
        self.headers[name] = value

    def write(self, data):
        assert isinstance(data, str), data
        self.data.append(data)

    def load(self):
        "Load the application pages."
        import app
        reload(app)
        self.mapper = app.mapper

    def __call__(self, environ, start_response):
        if self.ctx.debug:
            self.load()

        self.environ = environ
        self.response = start_response
        del self.data[:]
        self.headers = Headers(self.default_headers)

        ctx = copy(self.ctx) # shallow
        ctx.ledger = self.ledger

        path = environ['PATH_INFO']

        ishtml = '.' not in basename(path) or path.endswith('.html')
        if ishtml:
            # Load cookie (session is only in memory).
            cookie = Cookie.SimpleCookie(environ.get('HTTP_COOKIE', ''))
            has_cookie = (bool(cookie) and
                          'session' in cookie and
                          cookie["session"].value in self.cookiejar)
            if has_cookie:
                session_id = cookie["session"].value
                session = self.cookiejar[session_id]
            else:
                session_id = '%x' % randint(0, 16**16)
                cookie["session"] = session_id
                session = self.cookiejar[session_id] = {}
            ctx.session = session

        try:
            # Linear search in the regexp to match the request path.
            page, vardict = self.mapper.match(path)
            if page is None:
                raise HttpNotFound(path)
            else:
                # Update the context object with components of the request and
                # with the query parameters.
                ctx.environ = environ

                form = cgi.parse(environ=environ)
## FIXME: make this wsgi compatible.
                ## conlen = int(self.environ['CONTENT_LENGTH'])
                ## s = self.environ['wsgi.input'].read(conlen)
                ## form = cgi.parse_qs(s)

                ctx.__dict__.update(form)
                ctx.__dict__.update(vardict)

                page(self, ctx)

                # Add session cookie to headers, if necessary.
                if ishtml and not has_cookie:
                    for k, v in sorted(cookie.items()):
                        self.headers.add_header('Set-Cookie', v.OutputString())

                start_response('200 OK', self.headers.items())
                return self.data

        except HttpRedirect, e:
            location = str(e)
            start_response(e.status, [('Location', location)])
            return [str(e)]

        except HttpError, e:
            status = getattr(e, 'status', '500 Internal Server Error')
            start_response(status, [('Content-Type', 'text/html')])
            return [str(e)]

        except Exception, e:
            # Print out a nicely rendered traceback of the error.
            status = getattr(e, 'status', '200 OK')
            start_response(status, [('Content-Type', 'text/html')])
            failsafe = traceback.format_exc()
            try:
                return [cgitb.html(sys.exc_info())]
            except Exception:
                return ['<pre>', failsafe, '</pre>']




class Context(object):
    """
    An object that contains whatever input parameters or path components for a
    request.
    """


class Mapper(object):
    """Given a desdcription of the pages in the system, build a simple mapper
    object."""
    def __init__(self, page_directory):
        self.direc = page_directory

        self.match_expressions = []
        self.fwd_map = {}

        for rid, handler, render, regexp in self.direc:
            assert handler is not None
            assert render is not None
            if rid:
                self.fwd_map[rid] = render

            if regexp is None:
                regexp = '^%s$' % render
            self.match_expressions.append( (re.compile(regexp), handler) )

    def match(self, path):
        """Try to match the given path to one of our page handlers.
        Return the (handler, var-dict) as a result."""
        for xre, page in self.match_expressions:
            mo = xre.match(path)
            if mo:
                return page, mo.groupdict()
        else:
            return None, None

    def map(self, rid, *args, **kwds):
        """Map a URL forward."""
        url = self.fwd_map[rid] % args
        if kwds:
            query = []
            for kv in kwds.iteritems():
                query.append('%s=%s' % kv)
            url += '?' + '&'.join(query)
        return url





def main():
    import optparse
    parser = optparse.OptionParser(__doc__.strip())

    cmdline.addopts(parser)
    parser.add_option('-d', '--debug', '--devel', action='store_true',
                      help="Debug/development mode: don't cache styles and "
                      "reload code on every request.")

    parser.add_option('-p', '--port', action='store', type='int',
                      default=8080,
                      help="Port to use for local web server.")

    parser.add_option('-T', '--title', action='store',
                      help="Title to display in the web interface.")

    parser.add_option('--conversion', '--convert',
                      action='append', metavar='CONVERSION', default=[],
                      help="Apply the given conversion to wallets before "
                      "displaying them. The option's format should like "
                      "this: '1 EUR = 1.28 USD'.")

    t = TimerUtil('main')
    opts, ledger, args = cmdline.main(parser)
    t('load_ledger')

    # Parse the specified conversions.
    opts.conversions = []
    for cstr in opts.conversion:
        side = '([0-9.]+)\s+([A-Z]{3})'
        mo = re.match('\s*%s\s*=\s*%s\s*' % (side, side), cstr)
        amt1, amt2 = map(Decimal, mo.group(1, 3))
        comm1, comm2 = mo.group(2, 4)
        opts.conversions.append( (comm1, comm2, amt2/amt1) )

    # Re-enable interrupts.
    import signal; signal.signal(signal.SIGINT, signal.SIG_DFL)

    # Create and run the web server.
    app = BeanServer(ledger, opts)
    httpd = make_server('', opts.port, app)
    sa = httpd.socket.getsockname()
    t('make_server')
    print t
    t = None
    
    print ("Ready. ( http://%s:%s )" % (sa[0], sa[1]))
    try:
        while 1:
            httpd.handle_request()  # serve one request, then exit
    except KeyboardInterrupt:
        print 'Interrupted.'

