"""
A version of bisect that accepts a custom key function, like the sorting ones do.
"""
import random


def bisect_left_withkey(sequence, value, key=None):
    """Find the last element before the given date.
    Return the index. May return None"""

    if key is None:
        key = lambda x: x

    lo = 0
    hi = len(sequence)

    while lo < hi:
        mid = (lo + hi) // 2
        if key(sequence[mid]) < value:
            lo = mid + 1
        else:
            hi = mid
    return lo



