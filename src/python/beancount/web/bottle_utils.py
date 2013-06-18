"""
Bottle utilities, mostly helpers to do mounts on top of dynamic routes.
"""
import bottle
from bottle import request


class AttrMapper:
    "A URL mapper that allows attribute access for view-links."

    def __init__(self, mapper_function):
        self.mapper_function = mapper_function

    def __getattr__(self, name):
        return self.mapper_function(name)

    def build(self, *args, **kwargs):
        return self.mapper_function(*args, **kwargs)


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
