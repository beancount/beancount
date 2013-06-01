"""
Bottle utilities, mostly helpers to do mounts on top of dynamic routes.
"""
import bottle
from bottle import request


class AppMapper:
    "A URL mapper that allows attribute access for view-links."

    def __init__(self, app):
        self.app = app

    def __getattr__(self, aname):
        return self.app.get_url(aname)

    def build(self, name, **kwargs):
        return self.app.get_url(name, **kwargs)


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


def populate_view(callback):
    "A plugin that will populate the request with the current view instance."
    def wrapper(*args, **kwargs):
        request.view = request.environ['VIEW']
        return callback(*args, **kwargs)
    return wrapper
