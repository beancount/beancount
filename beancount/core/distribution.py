"""A simple accumulator for data about a mathematical distribution."""

__copyright__ = "Copyright (C) 2015-2017, 2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections


class Distribution:
    """A class that computes a histogram of integer values. This is used to compute
    a length that will cover at least some decent fraction of the samples.
    """

    def __init__(self):
        self.hist = collections.defaultdict(int)

    def empty(self):
        """Return true if the distribution is empty.

        Returns:
          A boolean.
        """
        return len(self.hist) == 0

    def update(self, value):
        """Add a sample to the distribution.

        Args:
          value: A value of the function.
        """
        self.hist[value] += 1

    def update_from(self, other):
        """Add samples from the other distribution to this one.

        Args:
          other: Another distribution.
        """
        for value, count in other.hist.items():
            self.hist[value] += count

    def min(self):
        """Return the minimum value seen in the distribution.

        Returns:
          An element of the value type, or None, if the distribution was empty.
        """
        if not self.hist:
            return None
        value, _ = sorted(self.hist.items())[0]
        return value

    def max(self):
        """Return the minimum value seen in the distribution.

        Returns:
          An element of the value type, or None, if the distribution was empty.
        """
        if not self.hist:
            return None
        value, _ = sorted(self.hist.items())[-1]
        return value

    def mode(self):
        """Return the mode of the distribution.

        Returns:
          An element of the value type, or None, if the distribution was empty.
        """
        if not self.hist:
            return None
        max_value = 0
        max_count = 0
        for value, count in sorted(self.hist.items()):
            if count >= max_count:
                max_count = count
                max_value = value
        return max_value
