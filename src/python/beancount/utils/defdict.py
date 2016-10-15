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


NOTFOUND = object()

class ImmutableDictWithDefault(dict):
    """An immutable dict which returns a default value for missing keys.

    This differs from a defualtdict in that it does not insert a missing default
    value when one is materialized (from a missing fetch), and furtheremore, the
    set method is make unavailable to prevent mutation beyond construction.
    """
    def __init__(self, default, *args):
        super(ImmutableDictWithDefault, self).__init__(*args)
        self.default = default

    def __setitem__(self, key, value):
        raise NotImplementedError

    def __getitem__(self, key):
        value = dict.get(self, key, NOTFOUND)
        if value is NOTFOUND:
            value = self.default
        return value

    def get(self, key, _=None):
        return self.__getitem__(key)
