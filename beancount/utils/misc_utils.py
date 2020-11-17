"""
Generic utility packages and functions.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

from collections import defaultdict
from time import time
import collections
import contextlib
import functools
import io
import re
import sys
import warnings


def deprecated(message):
    """A decorator generator to mark functions as deprecated and log a warning."""
    def decorator(func):
        @functools.wraps(func)
        def new_func(*args, **kwargs):
            warnings.warn("Call to deprecated function {}: {}".format(func.__name__,
                                                                      message),
                          category=DeprecationWarning,
                          stacklevel=2)
            return func(*args, **kwargs)
        return new_func
    return decorator


@contextlib.contextmanager
def log_time(operation_name, log_timings, indent=0):
    """A context manager that times the block and logs it to info level.

    Args:
      operation_name: A string, a label for the name of the operation.
      log_timings: A function to write log messages to. If left to None,
        no timings are written (this becomes a no-op).
      indent: An integer, the indentation level for the format of the timing
        line. This is useful if you're logging timing to a hierarchy of
        operations.
    Yields:
      The start time of the operation.
    """
    time1 = time()
    yield time1
    time2 = time()
    if log_timings:
        log_timings("Operation: {:48} Time: {}{:6.0f} ms".format(
            "'{}'".format(operation_name), '      '*indent, (time2 - time1) * 1000))


@contextlib.contextmanager
def box(name=None, file=None):
    """A context manager that prints out a box around a block.
    This is useful for printing out stuff from tests in a way that is readable.

    Args:
      name: A string, the name of the box to use.
      file: The file object to print to.
    Yields:
      None.
    """
    file = file or sys.stdout
    file.write('\n')
    if name:
        header = ',--------({})--------\n'.format(name)
        footer = '`{}\n'.format('-' * (len(header)-2))
    else:
        header = ',----------------\n'
        footer = '`----------------\n'

    file.write(header)
    yield
    file.write(footer)
    file.flush()


@contextlib.contextmanager
def swallow(*exception_types):
    """Catch and ignore certain exceptions.

    Args:
      exception_types: A tuple of exception classes to ignore.
    Yields:
      None.
    """
    try:
        yield
    except Exception as exc:
        if not isinstance(exc, exception_types):
            raise


def groupby(keyfun, elements):
    """Group the elements as a dict of lists, where the key is computed using the
    function 'keyfun'.

    Args:
      keyfun: A callable, used to obtain the group key from each element.
      elements: An iterable of the elements to group.
    Returns:
      A dict of key to list of sequences.
    """
    # Note: We could allow a custom aggregation function. Another option is
    # provide another method to reduce the list values of a dict, but that can
    # be accomplished using a dict comprehension.
    grouped = defaultdict(list)
    for element in elements:
        grouped[keyfun(element)].append(element)
    return grouped


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
    for element in seq:
        len_element = len(element)
        if len_element > length:
            longest, length = element, len_element
    return longest


def skipiter(iterable, num_skip):
    """Skip some elements from an iterator.

    Args:
      iterable: An iterator.
      num_skip: The number of elements in the period.
    Yields:
      Elements from the iterable, with num_skip elements skipped.
      For example, skipiter(range(10), 3) yields [0, 3, 6, 9].
    """
    assert num_skip > 0
    sit = iter(iterable)
    while 1:
        try:
            value = next(sit)
        except StopIteration:
            return
        yield value
        for _ in range(num_skip-1):
            try:
                next(sit)
            except StopIteration:
                return


def get_tuple_values(ntuple, predicate, memo=None):
    """Return all members referred to by this namedtuple instance that satisfy the
    given predicate. This function also works recursively on its members which
    are lists or tuples, and so it can be used for Transaction instances.

    Args:
      ntuple: A tuple or namedtuple.
      predicate: A predicate function that returns true if an attribute is to be
        output.
      memo: An optional memoizing dictionary. If a tuple has already been seen, the
        recursion will be avoided.
    Yields:
      Attributes of the tuple and its sub-elements if the predicate is true.
    """
    if memo is None:
        memo = set()
    id_ntuple = id(ntuple)
    if id_ntuple in memo:
        return
    memo.add(id_ntuple)

    if predicate(ntuple):
        yield
    for attribute in ntuple:
        if predicate(attribute):
            yield attribute
        if isinstance(attribute, (list, tuple)):
            for value in get_tuple_values(attribute, predicate, memo):
                yield value


def replace_namedtuple_values(ntuple, predicate, mapper, memo=None):
    """Recurse through all the members of namedtuples and lists, and for
    members that match the given predicate, run them through the given mapper.

    Args:
      ntuple: A namedtuple instance.
      predicate: A predicate function that returns true if an attribute is to be
        output.
      mapper: A callable, that will accept a single argument and return its
        replacement value.
      memo: An optional memoizing dictionary. If a tuple has already been seen, the
        recursion will be avoided.
    Yields:
      Attributes of the tuple and its sub-elements if the predicate is true.
    """
    if memo is None:
        memo = set()
    id_ntuple = id(ntuple)
    if id_ntuple in memo:
        return None
    memo.add(id_ntuple)

    # pylint: disable=unidiomatic-typecheck
    if not (type(ntuple) is not tuple and isinstance(ntuple, tuple)):
        return ntuple
    replacements = {}
    for attribute_name, attribute in zip(ntuple._fields, ntuple):
        if predicate(attribute):
            replacements[attribute_name] = mapper(attribute)
        elif type(attribute) is not tuple and isinstance(attribute, tuple):
            replacements[attribute_name] = replace_namedtuple_values(
                attribute, predicate, mapper, memo)
        elif type(attribute) in (list, tuple):
            replacements[attribute_name] = [
                replace_namedtuple_values(member, predicate, mapper, memo)
                for member in attribute]
    return ntuple._replace(**replacements)


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
            id_ = mre.sub(replacement, string)
            if id_ in seen:
                break  # Collision.
            seen.add(id_)
            idmap[id_] = string
        else:
            break
    else:
        return None # Could not find a unique mapping.

    return idmap


def escape_string(string):
    """Escape quotes and backslashes in payee and narration.

    Args:
      string: Any string.
    Returns.
      The input string, with offending characters replaced.
    """
    return string.replace('\\', r'\\')\
                 .replace('"', r'\"')


def idify(string):
    """Replace characters objectionable for a filename with underscores.

    Args:
      string: Any string.
    Returns:
      The input string, with offending characters replaced.
    """
    for sfrom, sto in [(r'[ \(\)]+', '_'),
                       (r'_*\._*', '.')]:
        string = re.sub(sfrom, sto, string)
    string = string.strip('_')
    return string


def dictmap(mdict, keyfun=None, valfun=None):
    """Map a dictionary's value.

    Args:
      mdict: A dict.
      key: A callable to apply to the keys.
      value: A callable to apply to the values.
    """
    if keyfun is None:
        keyfun = lambda x: x
    if valfun is None:
        valfun = lambda x: x
    return {keyfun(key): valfun(val) for key, val in mdict.items()}


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


def first_paragraph(docstring):
    """Return the first sentence of a docstring.
    The sentence has to be delimited by an empty line.

    Args:
      docstring: A doc string.
    Returns:
      A string with just the first sentence on a single line.
    """
    lines = []
    for line in docstring.strip().splitlines():
        if not line:
            break
        lines.append(line.rstrip())
    return ' '.join(lines)


def import_curses():
    """Try to import the 'curses' module.
    (This is used here in order to override for tests.)

    Returns:
      The curses module, if it was possible to import it.
    Raises:
      ImportError: If the module could not be imported.
    """
    # Note: There's a recipe for getting terminal size on Windows here, without
    # curses, I should probably implement that at some point:
    # https://stackoverflow.com/questions/263890/how-do-i-find-the-width-height-of-a-terminal-window
    # Also, consider just using 'blessings' instead, which provides this across
    # multiple platforms.
    # pylint: disable=import-outside-toplevel
    import curses
    return curses


def _get_screen_value(attrname, default=0):
    """Return the width or height of the terminal that runs this program."""
    try:
        curses = import_curses()
    except ImportError:
        value = default
    else:
        try:
            curses.setupterm()
            value = curses.tigetnum(attrname)
        except (io.UnsupportedOperation, curses.error):
            value = default
    return value


def get_screen_width():
    """Return the width of the terminal that runs this program.

    Returns:
      An integer, the number of characters the screen is wide.
      Return 0 if the terminal cannot be initialized.
    """
    return _get_screen_value('cols', 0)


def get_screen_height():
    """Return the height of the terminal that runs this program.

    Returns:
      An integer, the number of characters the screen is high.
      Return 0 if the terminal cannot be initialized.
    """
    return _get_screen_value('lines', 0)


class TypeComparable:
    """A base class whose equality comparison includes comparing the
    type of the instance itself.
    """
    def __eq__(self, other):
        return isinstance(other, type(self)) and super().__eq__(other)

def cmptuple(name, attributes):
    """Manufacture a comparable namedtuple class, similar to collections.namedtuple.

    A comparable named tuple is a tuple which compares to False if contents are
    equal but the data types are different. We define this to supplement
    collections.namedtuple because by default a namedtuple disregards the type
    and we want to make precise comparisons for tests.

    Args:
      name: The given name of the class.
      attributes: A string or tuple of strings, with the names of the
        attributes.
    Returns:
      A new namedtuple-derived type that compares False with other
      tuples with same contents.
    """
    base = collections.namedtuple('_{}'.format(name), attributes)
    return type(name, (TypeComparable, base,), {})


def uniquify(iterable, keyfunc=None, last=False):
    """Given a sequence of elements, remove duplicates of the given key. Keep either
    the first or the last element of a sequence of key-identical elements. Order
    is maintained as much as possible. This does maintain the ordering of the
    original elements, they are returned in the same order as the original
    elements.

    Args:
      iterable: An iterable sequence.
      keyfunc: A function that extracts from the elements the sort key
        to use and uniquify on. If left unspecified, the identify function
        is used and the uniquification occurs on the elements themselves.
      last: A boolean, True if we should keep the last item of the same keys.
        Otherwise keep the first.
    Yields:
      Elements from the iterable.
    """
    if keyfunc is None:
        keyfunc = lambda x: x
    seen = set()
    if last:
        unique_reversed_list = []
        for obj in reversed(iterable):
            key = keyfunc(obj)
            if key not in seen:
                seen.add(key)
                unique_reversed_list.append(obj)
        yield from reversed(unique_reversed_list)
    else:
        for obj in iterable:
            key = keyfunc(obj)
            if key not in seen:
                seen.add(key)
                yield obj


UNSET = object()

def sorted_uniquify(iterable, keyfunc=None, last=False):
    """Given a sequence of elements, sort and remove duplicates of the given key.
    Keep either the first or the last (by key) element of a sequence of
    key-identical elements. This does _not_ maintain the ordering of the
    original elements, they are returned sorted (by key) instead.

    Args:
      iterable: An iterable sequence.
      keyfunc: A function that extracts from the elements the sort key
        to use and uniquify on. If left unspecified, the identify function
        is used and the uniquification occurs on the elements themselves.
      last: A boolean, True if we should keep the last item of the same keys.
        Otherwise keep the first.
    Yields:
      Elements from the iterable.
    """
    if keyfunc is None:
        keyfunc = lambda x: x
    if last:
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
    else:
        prev_key = UNSET
        for obj in sorted(iterable, key=keyfunc):
            key = keyfunc(obj)
            if key != prev_key:
                yield obj
                prev_key = key


def is_sorted(iterable, key=lambda x: x, cmp=lambda x, y: x <= y):
    """Return true if the sequence is sorted.

    Args:
      iterable: An iterable sequence.
      key: A function to extract the quantity by which to sort.
      cmp: A function that compares two elements of a sequence.
    Returns:
      A boolean, true if the sequence is sorted.
    """
    iterator = map(key, iterable)
    prev = next(iterator)
    for element in iterator:
        if not cmp(prev, element):
            return False
        prev = element
    return True


class LineFileProxy:
    """A file object that will delegate writing full lines to another logging function.
    This may be used for writing data to a logging level without having to worry about
    lines.
    """
    def __init__(self, line_writer, prefix=None, write_newlines=False):
        """Construct a new line delegator file object proxy.

        Args:
          line_writer: A callable function, used to write to the delegated output.
          prefix: An optional string, the prefix to insert before every line.
          write_newlines: A boolean, true if we should output the newline characters.
        """
        self.line_writer = line_writer
        self.prefix = prefix
        self.write_newlines = write_newlines
        self.data = []

    def write(self, data):
        """Write some string data to the output.

        Args:
          data: A string, with or without newlines.
        """
        if '\n' in data:
            self.data.append(data)
            self.flush()
        else:
            self.data.append(data)

    def flush(self):
        """Flush the data to the line writer."""
        data = ''.join(self.data)
        if data:
            lines = data.splitlines()
            self.data = [lines.pop(-1)] if data[-1] != '\n' else []
            for line in lines:
                if self.prefix:
                    line = self.prefix + line
                if self.write_newlines:
                    line += '\n'
                self.line_writer(line)

    def close(self):
        """Close the line delegator."""
        self.flush()
