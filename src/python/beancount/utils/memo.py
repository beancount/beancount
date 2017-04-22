"""Memoization utilities.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import shelve
import threading
import hashlib
import datetime
import functools
import io


def now():
    "Indirection on datetime.datetime.now() for testing."
    return datetime.datetime.now()


def memoize_recent_fileobj(function, cache_filename, expiration=None):
    """Memoize recent calls to the given function which returns a file object.

    The results of the cache expire after some time.

    Args:
      function: A callable object.
      cache_filename: A string, the path to the database file to cache to.
      expiration: The time during which the results will be kept valid. Use
        'None' to never expire the cache (this is the default).
    Returns:
      A memoized version of the function.
    """
    urlcache = shelve.open(cache_filename, 'c')
    urlcache.lock = threading.Lock()  # Note: 'shelve' is not thread-safe.
    @functools.wraps(function)
    def memoized(*args, **kw):
        # Encode the arguments, including a date string in order to invalidate
        # results over some time.
        md5 = hashlib.md5()
        md5.update(str(args).encode('utf-8'))
        md5.update(str(sorted(kw.items())).encode('utf-8'))

        hash_ = md5.hexdigest()
        time_now = now()
        try:
            with urlcache.lock:
                time_orig, contents = urlcache[hash_]
            if expiration is not None and (time_now - time_orig) > expiration:
                raise KeyError
        except KeyError:
            fileobj = function(*args, **kw)
            if fileobj:
                contents = fileobj.read()
                with urlcache.lock:
                    urlcache[hash_] = (time_now, contents)
            else:
                contents = None

        return io.BytesIO(contents) if contents else None
    return memoized
