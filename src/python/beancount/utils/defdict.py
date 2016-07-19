"""An instance of collections.defaultdict whose factory accepts a key.

Note: This really ought to be an enhancement to Python itself. I should bother
adding this in eventually.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import collections


class DefaultDictWithKey(collections.defaultdict):
    """A version of defaultdict whose factory accepts the key as an argument.
    Note: collections.defaultdict would be improved by supporting this directly,
    this is a common occurence.
    """
    def __missing__(self, key):
        self[key] = value = self.default_factory(key)  # pylint: disable=not-callable
        return value
