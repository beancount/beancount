"""
A version of bisect that accepts a custom key function, like the sorting ones do.
"""


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
    # pylint: disable=invalid-name
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
