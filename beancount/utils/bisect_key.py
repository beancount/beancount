"""A version of bisect that accepts a custom key function, like the sorting ones do.
"""
__copyright__ = "Copyright (C) 2013-2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"



def bisect_left_with_key(sequence, value, key=None):
    """Find the last element before the given value in a sorted list.

    Args:
      sequence: A sorted sequence of elements.
      value: The value to search for.
      key: An optional function used to extract the value from the elements of
        sequence.
    Returns:
      Return the index. May return None.
    """
    if key is None:
        key = lambda x: x  # Identity.

    lo = 0
    hi = len(sequence)

    while lo < hi:
        mid = (lo + hi) // 2
        if key(sequence[mid]) < value:
            lo = mid + 1
        else:
            hi = mid
    return lo


def bisect_right_with_key(a, x, key, lo=0, hi=None):
    """Like bisect.bisect_right, but with a key lookup parameter.

    Args:
      a: The list to search in.
      x: The element to search for.
      key: A function, to extract the value from the list.
      lo: The smallest index to search.
      hi: The largest index to search.
    Returns:
      As in bisect.bisect_right, an element from list 'a'.
    """
    # pylint: disable=invalid-name
    if lo < 0:
        raise ValueError('lo must be non-negative')
    if hi is None:
        hi = len(a)
    while lo < hi:
        mid = (lo+hi)//2
        if x < key(a[mid]):
            hi = mid
        else:
            lo = mid+1
    return lo
