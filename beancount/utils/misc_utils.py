"""
Generic utility packages and functions.
"""

__copyright__ = "Copyright (C) 2014-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import contextlib
import io
from collections import defaultdict
from time import time


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
        log_timings(
            "Operation: {:48} Time: {}{:6.0f} ms".format(
                "'{}'".format(operation_name), "      " * indent, (time2 - time1) * 1000
            )
        )


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
        for _ in range(num_skip - 1):
            try:
                next(sit)
            except StopIteration:
                return


def escape_string(string):
    """Escape quotes and backslashes in payee and narration.

    Args:
      string: Any string.
    Returns.
      The input string, with offending characters replaced.
    """
    return string.replace("\\", r"\\").replace('"', r"\"")


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
    return _get_screen_value("cols", 0)


def get_screen_height():
    """Return the height of the terminal that runs this program.

    Returns:
      An integer, the number of characters the screen is high.
      Return 0 if the terminal cannot be initialized.
    """
    return _get_screen_value("lines", 0)


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
        if "\n" in data:
            self.data.append(data)
            self.flush()
        else:
            self.data.append(data)

    def flush(self):
        """Flush the data to the line writer."""
        data = "".join(self.data)
        if data:
            lines = data.splitlines()
            self.data = [lines.pop(-1)] if data[-1] != "\n" else []
            for line in lines:
                if self.prefix:
                    line = self.prefix + line
                if self.write_newlines:
                    line += "\n"
                self.line_writer(line)

    def close(self):
        """Close the line delegator."""
        self.flush()
