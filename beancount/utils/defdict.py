"""An instance of collections.defaultdict whose factory accepts a key.

Note: This really ought to be an enhancement to Python itself. I should bother
adding this in eventually.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections


class DefaultDictWithKey(collections.defaultdict):
    """A version of defaultdict whose factory accepts the key as an argument.
    Note: collections.defaultdict would be improved by supporting this directly,
    this is a common occurrence.
    """
    def __missing__(self, key):
        self[key] = value = self.default_factory(key)  # pylint: disable=not-callable
        return value


NOTFOUND = object()

class ImmutableDictWithDefault(dict):
    """An immutable dict which returns a default value for missing keys.

    This differs from a defaultdict in that it does not insert a missing default
    value when one is materialized (from a missing fetch), and furthermore, the
    set method is make unavailable to prevent mutation beyond construction.
    """
    def __init__(self, *args, default=None):
        super().__init__(*args)
        self.default = default

    def __setitem__(self, key, value):
        """Disallow mutating the dict in the usual way."""
        raise NotImplementedError

    def __getitem__(self, key):
        value = dict.get(self, key, NOTFOUND)
        if value is NOTFOUND:
            value = self.default
        return value

    def get(self, key, _=None):
        return self.__getitem__(key)

    # The next three methods are present in order to support pickling. Note that
    # because this class is a specialization of dict, and that dict has a
    # special handler in pickle, you have to resort to using __reduce__().

    def __getstate__(self):
        return (self.default, list(self.items()))

    def __setstate__(self, state):
        self.default, items = state
        super().update(items)

    def __reduce__(self):
        return (ImmutableDictWithDefault, (), self.__getstate__())
