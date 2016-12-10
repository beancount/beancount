"""
Bottle utilities, mostly helpers to do mounts on top of dynamic routes.
"""
__copyright__ = "Copyright (C) 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"

import bottle
from bottle import request


class AttrMapper:
    """A URL mapper that allows attribute access for view-links.
    This is used in templates."""

    def __init__(self, mapper_function):
        """Constructor for an attribute mapper.

        Args:
          mapper_function: A function to apply on attribute lookup, and
            upon calling .build().
        """
        self.mapper_function = mapper_function

    def __getattr__(self, name):
        return self.mapper_function(name)

    def build(self, *args, **kwargs):
        return self.mapper_function(*args, **kwargs)


# Silence pyflakes errors.
# pylint: disable=invalid-name
itertools = None
_raise = lambda *args: None

def internal_redirect(app, path_depth):
    """A version of bottle's mountpoint_wrapper() that we call explicitly.

    Bottle supports a mount() method that allows on to install an application on
    a subpath of the main application. However, it does this on a fixed path. We
    want to manually intercept the lazy creation or fetching of a view and call
    for a redirect explicitly (via bottle's mountpoint_wrapper() function).
    However, this function is hidden within the scope of a the Bottle.mount()
    method; if it were defined globally we would just use it, but it is not. So
    we copy if here. This is directly lifted from Bottle.mount() and edited
    minimally.

    Args:
      app: A Bottle instance.
      path_depth: The number of request path components to skip for the mount.
        For example, if our subapplication is mount on /view/all, then the path
        depth is 2.
    Returns:
      A Bottle HTTPResponse objet.
    Raises:
      Exception: Any exception, depending on the callback.
    """
    # pylint: disable=invalid-name
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
