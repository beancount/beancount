"""
Generic utility packages and functions.
"""
import collections
import io
import re
import os
import sys
import subprocess
import logging
from time import time
import contextlib
from collections import defaultdict


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


DEFAULT_PAGER = 'more'

@contextlib.contextmanager
def pager(command=None):
    """Create a subprocess to write output to and wait for completion.

    This contextmanager is intended to be used to pipe output to a pager and
    wait on the pager to complete before continuing. Simply write to the file
    object and upon exit we close the file object. This also silences broken
    pipe errors triggered by the user exiting the sub-process, and recovers from
    a failing pager command by just using stdout.

    Args:
      command: A string, the shell command to run as a sub-process. This is run
        in a sub-shell with the full user environment. The intention is that you
        will use the PAGER environment variable.  If left to None, we initialize
        it to the PAGER.
    Yields:
      A file object to write to.
    """
    if command is None:
        command = os.environ.get('PAGER', DEFAULT_PAGER)
    if not command:
        command = DEFAULT_PAGER

    try:
        pipe = subprocess.Popen(command, shell=True, stdin=subprocess.PIPE)
    except OSError as exc:
        logging.error("Invalid pager: {}".format(exc))
        yield sys.stdout
        sys.stdout.flush()
    else:
        try:
            stdin_wrapper = io.TextIOWrapper(pipe.stdin, 'utf-8')
            yield stdin_wrapper
            stdin_wrapper.close()
            pipe.wait()
        except BrokenPipeError:
            pass


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


def get_tuple_values(ntuple, predicate, memo=None):
    """Return all members referred to by this namedtuple instance that satisfy the
    given predicate. This function also works recursively on its members which
    are lists or typles, and so it can be used for Transaction instances.

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
        return
    memo.add(id_ntuple)

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
        return # Could not find a unique mapping.

    return idmap


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


def get_screen_width():
    """Return the width of the terminal that runs this program.

    Returns:
      An integer, the number of characters the screen is wide.
      Return 0 if the terminal cannot be initialized.
    """
    import curses
    try:
        curses.setupterm()
    except io.UnsupportedOperation:
        return 0
    return curses.tigetnum('cols')


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
    UNSET = object()
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
