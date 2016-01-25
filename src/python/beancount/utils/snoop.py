"""Text manipulation utilities.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import functools


class Snoop:
    """A snooper callable that just saves the returned values of a
    function. This is particularly useful for re.match and re.search in
    conditionals, e.g.::

      snoop = Snoop()
      ...
      if snoop(re.match("(\d+)-(\d+)-(\d+)", text)):
        year, month, date = snoop.value.group(1, 2, 3)

    Attributes:
      value: The last value snooped from a function call.
      history: If 'maxlen' was specified, the last few values
        that were snooped.
    """

    def __init__(self, maxlen=None):
        """Create a new snooper.

        Args:
          maxlen: If specified, an integer, which enables the saving of that
          number of last values in the history attribute.
        """
        self.value = None
        self.history = (collections.deque(maxlen=maxlen)
                        if maxlen
                        else None)

    def __call__(self, value):
        """Save a value to the snooper. This is meant to wrap
        a function call.

        Args:
          value: The value to push/save.
        Returns:
          Value itself.
        """
        self.value = value
        if self.history is not None:
            self.history.append(value)
        return value

    def __getattr__(self, attr):
        """Forward the attribute to the value.

        Args:
          attr: A string, the name of the attribute.
        Returns:
          The value of the attribute.
        """
        return getattr(self.value, attr)


# A global instance of a Snoop, for convenience. In many cases you can just
# import and use this.
snooper = Snoop()  # pylint: disable=invalid-name


def snoopify(function):
    """Decorate a function as snoopable.

    This is meant to reassign existing functions to a snoopable version of them.
    For example, if you wanted 're.match' to be automatically snoopable, just
    decorate it like this:

      re.match = snoopify(re.match)

    and then you can just call 're.match' in a conditional and then access
    're.match.value' to get to the last returned value.
    """
    @functools.wraps(function)
    def wrapper(*args, **kw):
        value = function(*args, **kw)
        wrapper.value = value
        return value
    wrapper.value = None
    return wrapper
