"""A version of bisect that accepts a custom key function, like the sorting ones do."""

from __future__ import annotations

__copyright__ = "Copyright (C) 2013-2014, 2016-2017, 2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

from typing import Callable
from typing import Sequence
from typing import TypeVar
from typing import overload

T = TypeVar("T")
U = TypeVar("U")


@overload
def bisect_left_with_key(sequence: Sequence[T], value: T) -> int: ...


@overload
def bisect_left_with_key(sequence: Sequence[T], value: U, key: Callable[[T], U]) -> int: ...


def bisect_left_with_key(
    sequence: Sequence[T], value: U, key: Callable[[T], U] | None = None
) -> int:
    """Find the last element before the given value in a sorted list.

    Args:
      sequence: A sorted sequence of elements.
      value: The value to search for.
      key: An optional function used to extract the value from the elements of
        sequence.
    Returns:
      Return the index. May return None.
    """

    # see overloads, if no key function is given, T = U
    keyfunc: Callable[[T], U] = key if key is not None else lambda x: x  # type: ignore[assignment,return-value]

    lo = 0
    hi = len(sequence)

    while lo < hi:
        mid = (lo + hi) // 2
        # Python does not yet have a built-in way to add some "Comparable" bound to U
        if keyfunc(sequence[mid]) < value:  # type: ignore[operator]
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

    if lo < 0:
        raise ValueError("lo must be non-negative")
    if hi is None:
        hi = len(a)
    while lo < hi:
        mid = (lo + hi) // 2
        if x < key(a[mid]):
            hi = mid
        else:
            lo = mid + 1
    return lo
