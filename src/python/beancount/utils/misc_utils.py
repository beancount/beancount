"""
Generic utility packages and functions.
"""
import re
from time import time
import contextlib
from collections import defaultdict


@contextlib.contextmanager
def print_time(operation_name, log_function):
    """A context manager that times the block and logs it to info level.

    Args:
      operation_name: A string, a label for the name of the operation.
      log_function: A function to write log messages to. If left to None,
        no timings are written (this becomes a no-op).
    Yields:
      The start time of the operation.
    """
    t1 = time()
    yield t1
    t2 = time()
    if log_function:
        log_function("Operation: {:32}  Time: {:6.0f} ms".format(
            "'{}'".format(operation_name), (t2 - t1)*1000))


def groupby(keyfun, elements):
    """Group the elements as a dict of lists, where the key is computed using the
    function 'keyfun'.

    Args:
      keyfun: A callable, used to obtain the group key from each element.
      elements: An iterable of the elements to group.
    Returns:
      A dict of key to list of sequences.
    """
    grouped = defaultdict(list)
    for element in elements:
        grouped[keyfun(element)].append(element)
    return grouped


def uniquify_last(iterable, keyfunc=None):
    """Given a sequence of elements, remove duplicates of the given key. Keep the
    last element of a sequence of key-identical elements.

    Args:
      iterable: An iterable sequence.
      keyfunc: A function that extracts from the elements the sort key
        to use and uniquify on. If left unspecified, the identify function
        is used and the uniquification occurs on the elements themselves.
    Yields:
      (date, number) tuples.
    """
    if keyfunc is None:
        keyfunc = lambda x: x
    UNSET = object()
    prev_obj = UNSET
    prev_key = UNSET
    for obj in sorted(iterable, key=keyfunc):
        key = keyfunc(obj)
        if key != prev_key and prev_obj is not UNSET:
            yield prev_obj
        prev_obj = obj
        prev_key = key
    if prev_obj is not UNSET:
        yield prev_obj


def filter_type(elist, types):
    """Filter the given list to yield only instances of the given types.

    Args:
      elist: A sequence of elements.
      types: A sequence of types to include in the output list.
    Yields:
      Each element, if it is an instance of 'types'.
    """
    for element in elist:
        if not isinstance(element, types):
            continue
        yield element


def longest(seq):
    """Return the longest of the given subsequences.

    Args:
      seq: An iterable sequence of lists.
    Returns:
      The longest list from the sequence.
    """
    longest, length = None, -1
    for x in seq:
        lenx = len(x)
        if lenx > length:
            longest, length = x, lenx
    return longest


def get_tuple_values(ntuple, predicate, memo=None):
    """Return all members referred to by this namedtuple instance that satisfy the
    given predicate. This function also works recursively on its members which
    are lists or typles, and so it can be used for Transaction instances.

    Args:
      ntuple: A tuple or namedtuple.
      predicate: A predicate function that returns true if an attribute is to be
        output.
    Yields:
      Attributes of the tuple and its sub-elements if the predicate is true.
    """
    if memo is None:
        memo = set()
    if id(ntuple) in memo:
        return
    memo.add(id(ntuple))

    if predicate(ntuple):
        yield
    for attribute in ntuple:
        if predicate(attribute):
            yield attribute
        if isinstance(attribute, (list, tuple)):
            for value in get_tuple_values(attribute, predicate, memo):
                yield value


def compute_unique_clean_ids(strings):
    """Given a sequence of strings, reduce them to corresponding ids without any
    funny characters and insure that the list of ids is unique. Yields pairs
    of (id, string) for the result.

    Args:
      strings: A list of strings.
    Returns:
      A list of (id, string) pairs.
    """
    string_set = set(strings)

    # Try multiple methods until we get one that has no collisions.
    for regexp, replacement in [(r'[^A-Za-z0-9.-]', '_'),
                                (r'[^A-Za-z0-9_]', ''),]:
        seen = set()
        idmap = {}
        mre = re.compile(regexp)
        for string in string_set:
            id = mre.sub(replacement, string)
            if id in seen:
                break  # Collision.
            seen.add(id)
            idmap[id] = string
        else:
            break
    else:
        return # Could not find a unique mapping.

    return idmap


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


def map_namedtuple_attributes(attributes, mapper, object_):
    """Map the value of the named attributes of object by mapper.

    Args:
      attributes: A sequence of string, the attribute names to map.
      mapper: A callable that accepts the value of a field and returns
        the new value.
      object_: Some namedtuple object with attributes on it.
    Returns:
      A new instance of the same namedtuple with the named fields mapped by
      mapper.
    """
    return object_._replace(**{attribute: mapper(getattr(object_, attribute))
                               for attribute in attributes})


def staticvar(varname, initial_value):
    """Returns a decorator that defines a Python function attribute.

    This is used to simulate a static function variable in Python.

    Args:
      varname: A string, the name of the variable to define.
      initial_value: The value to initialize the variable to.
    Returns:
      A function decorator.
    """
    def deco(fun):
        setattr(fun, varname, initial_value)
        return fun
    return deco
