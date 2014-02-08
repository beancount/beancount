"""
Common utilities for processing and splitting data files.
"""

import itertools


def iter_sections(fileobj, separating_predicate=None):
    """For a given file object, yield file-like objects for each of the sections
    contained therein. A section is defined as a list of lines that don't match
    the predicate. For example, if you want to split by empty lines, provide a
    predicate that will be true given an empty line, which will cause a new
    section to be begun.

    Args:
      fileobj: A file object to read from.
      separating_predicate: A boolean predicate that is true on separating lines.
    Yields:
      A file-like object that you can use to read. EOF is signaled on an empty
      line.

    """
    if separating_predicate is None:
        separating_predicate = lambda line: bool(line.strip())

    lineiter = iter(fileobj)
    for line in lineiter:
        if separating_predicate(line):
            yield itertools.chain([line],
                                  iter_until_empty(lineiter,
                                                   separating_predicate))

def iter_until_empty(iterator, separating_predicate):
    """An iterator of lines that will stop at the first empty line.
    Args:
      iterator: An iterator of lines.
      separating_predicate: A boolean predicate that is true on separating lines.
    Yields:
      Non-empty lines. EOF when we hit an empty line.
    """
    for line in iterator:
        if not separating_predicate(line):
            break
        yield line


def get_sections(iterator, separating_predicate=None):
    """Consume and accumulate the results of a 2-level nested iterator.

    Args:
      iterator: An iterator of iterators.
      separating_predicate: A boolean predicate that is true on separating lines.
    Returns:
      A list of lists of objects.
    """
    return [''.join(subiter)
            for subiter in iter_sections(iterator, separating_predicate)]
