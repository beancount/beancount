"""Base class that implements identification using regular expressions.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import itertools
import re

from beancount.ingest import cache
from beancount.ingest import importer


_PARTS = {'mime', 'filename', 'content'}


def identify(remap, converter, file):
    """Identify the contents of a file.

    Args:
      remap: A dict of 'part' to list-of-compiled-regexp objects, where each item is
        a specification to match against its part. The 'part' can be one of 'mime',
        'filename' or 'content'.
      converter: A
    Returns:
      A boolean, true if the file is not rejected by the constraints.
    """
    if remap.get('mime', None):
        mimetype = file.convert(cache.mimetype)
        if not all(regexp.search(mimetype)
                   for regexp in remap['mime']):
            return False

    if remap.get('filename', None):
        if not all(regexp.search(file.name)
                   for regexp in remap['filename']):
            return False

    if remap.get('content', None):
        # If this is a text file, read the whole thing in memory.
        text = file.convert(converter or cache.contents)
        if not all(regexp.search(text)
                   for regexp in remap['content']):
            return False

    return True


class IdentifyMixin(importer.ImporterProtocol):

    def __init__(self, **kwds):
        """Pull 'matchers' and 'converter' from kwds."""

        self.remap = collections.defaultdict(list)
        matchers = kwds.pop('matchers', [])
        cls_matchers = getattr(self, 'matchers', [])
        assert isinstance(matchers, list)
        assert isinstance(cls_matchers, list)
        for part, regexp in itertools.chain(matchers, cls_matchers):
            assert part in _PARTS, repr(part)
            assert isinstance(regexp, str), repr(regexp)
            self.remap[part].append(re.compile(regexp))

        # Converter is a fn(filename: Text) -> contents: Text.
        self.converter = kwds.pop('converter',
                                  getattr(self, 'converter', None))

        super().__init__(**kwds)

    def identify(self, file):
        return identify(self.remap, self.converter, file)
