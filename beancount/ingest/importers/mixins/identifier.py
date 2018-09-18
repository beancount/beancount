"""Base class that implements identification using regular expressions.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import logging
import collections
import re
from typing import List, Tuple

from beancount.ingest import cache
from beancount.ingest import importer


_PARTS = {'mime', 'filename', 'content'}


def identify(remap, converter, file):
    """Identify the contents of a file.

    Args:
      remap: A dict of 'part' to list-of-compiled-regexp objects, where each item is 
        a specification to match against its part. The 'part' can be one of 'mime',
        'filename' or 'content'.
    Returns:
      A boolean, true if the file is not rejected by the constraints.
    """
    if remap['mime']:
        mimetype = file.convert(cache.mimetype)
        if not all(regexp.search(mimetype)
                   for regexp in remap['mime']):
            return False

    if remap['filename']:
        if not all(regexp.search(file.name)
                   for regexp in remap['filename']):
            return False

    if remap['content']:
        # If this is a text file, read the whole thing in memory.
        text = file.convert(converter or cache.contents)
        if not all(regexp.search(text, re.DOTALL)
                   for regexp in remap['content']):
            return False

    return True


class IdentifyBase(importer.ImporterProtocol):
    
    def __init__(self, **kwds):
        """Pull 'matchers' and 'converter' from kwds.""" 

        self.remap = collections.defaultdict(list)
        matchers = kwds.pop('matchers', [])
        assert isinstance(matchers, list)
        for part, regexp in matchers:
            assert part in _PARTS
            assert isinstance(regexp, str)
            self.remap[part].append(re.compile(regexp))
        for part in _PARTS:
            if hasattr(self, part):
                for regexp in getattr(self, part):
                    self.remap[part].append(re.compile(regexp))

        self.converter = kwds.pop('converter', None)

        super().__init__(**kwds)

    def identify(self, file):
        return identify(self.remap, self.converter, file)
