"""A file wrapper which acts as a cache for on-demand evaluation of conversions.

This object is used in lieu of a file in order to allow the various importers to
reuse each others' conversion results. Converting file contents, e.g. PDF to
text, can be expensive.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import codecs
from os import path

import chardet

from beancount.utils import file_type
from beancount.utils import defdict


# NOTE: See get_file() at the end of this file to create instances of FileMemo.


# Maximum number of bytes to read in order to detect the encoding of a file.
HEAD_DETECT_MAX_BYTES = 128 * 1024


class _FileMemo:
    """A file memoizer which acts as a cache for on-demand evaluation of conversions.

    Attributes:
      name: A string, the name of the underlying file.
    """

    def __init__(self, filename):
        self.name = filename

        # A cache of converter function to saved conversion value.
        self._cache = {}

    def __str__(self):
        return '<FileWrapper filename="{}">'.format(self.name)

    def convert(self, converter_func):
        """Registers a callable used to convert the file contents.
        Args:
          converter_func: A callable which accepts a filename and produces some
          derived version of the file contents.
        Returns:
          A bytes object, with the contents of the entire file.
        """
        try:
            result = self._cache[converter_func]
        except KeyError:
            # FIXME: Implement timing of conversions here. Store it for
            # reporting later.
            result = self._cache[converter_func] = converter_func(self.name)
        return result

    def mimetype(self):
        """Computes the MIME type of the file."""
        return self.convert(mimetype)

    def head(self, num_bytes=8192, encoding=None):
        """An alias for reading just the first bytes of a file."""
        return self.convert(head(num_bytes, encoding=encoding))

    def contents(self):
        """An alias for reading the entire contents of the file."""
        return self.convert(contents)


def mimetype(filename):
    """A converter that computes the MIME type of the file.

    Returns:
      A converter function.
    """
    return file_type.guess_file_type(filename)


def head(num_bytes=8192, encoding=None):
    """A converter that just reads the first bytes of a file.

    Note that the returned string may represent less bytes than
    specified if the encoded bytes read from the file terminate with
    an incomplete unicode character. This is likely to occur for
    variable width encodings suach as utf8.

    Args:
      num_bytes: The number of bytes to read.
    Returns:
      A converter function.

    """
    def head_reader(filename):
        with open(filename, 'rb') as fd:
            data = fd.read(num_bytes)
            enc = encoding or chardet.detect(data)['encoding']
            # A little trick to handle an encoded byte array that
            # terminates with an incomplete unicode character.
            decoder = codecs.iterdecode(iter([data]), enc)
            return next(decoder)
    return head_reader


def contents(filename):
    """A converter that just reads the entire contents of a file.

    Args:
      num_bytes: The number of bytes to read.
    Returns:
      A converter function.
    """
    # Attempt to detect the input encoding automatically, using chardet and a
    # decent amount of input.
    with open(filename, 'rb') as infile:
        rawdata = infile.read(HEAD_DETECT_MAX_BYTES)
    detected = chardet.detect(rawdata)
    encoding = detected['encoding']

    # Ignore encoding errors for reading the contents because input files
    # routinely break this assumption.
    errors = 'ignore'

    with open(filename, encoding=encoding, errors=errors) as file:
        return file.read()


def get_file(filename):
    """Create or reuse a globally registered instance of a FileMemo.

    Note: the FileMemo objects' lifetimes are reused for the duration of the
    process. This is usually the intended behavior. Always create them by
    calling this constructor.

    Args:
      filename: A path string, the absolute name of the file whose memo to create.
    Returns:
      A FileMemo instance.

    """
    assert path.isabs(filename), (
        "Path should be absolute in order to guarantee a single call.")
    return _CACHE[filename]

_CACHE = defdict.DefaultDictWithKey(_FileMemo)
