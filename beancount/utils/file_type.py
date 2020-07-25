"""Code that can guess a MIME type for a filename.

This attempts to identify the mime-type of a file suing

1. The built-in mimetypes library, then
2. python-magic (if available), and finally
3. some custom mappers for datatypes used in financial downloads, such as
   Quicken files.

"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import re
import warnings
import mimetypes

# Try to import python-magic. This is not strictly necessary--if you don't use
# any of the special file types you might be able to get away without it, but
# some file types may not be detected..
try:
    import magic
    # If 'filemagic' is installed, ignore it. We require the 'python-magic'
    # wrapper.
    if not hasattr(magic, 'from_file'):
        warnings.warn("You have installed 'filemagic' instead of 'python-magic'; "
                      "disabling.")
        magic = None # pylint: disable=invalid-name
except (ImportError, OSError):
    magic = None


# A mapping of regular expression to MIME types.
EXTRA_FILE_TYPES = [
    (re.compile(regexp, re.I), filetype)
    for regexp, filetype in (
        (r'.*\.qbo$', 'application/vnd.intu.qbo'),
        (r'.*\.(qfx|ofx)$', 'application/x-ofx'),
    )]


def guess_file_type(filename):
    """Attempt to guess the type of the input file.

    Args:
      filename: A string, the name of the file to guess the type for.
    Returns:
      A suitable mimetype string, or None if we could not guess.
    """

    # Try the standard mimetypes association.
    filetype, _ = mimetypes.guess_type(filename, False)
    if filetype:
        return filetype

    # Try out some extra types that only we know about.
    for regexp, mimetype in EXTRA_FILE_TYPES:
        if regexp.match(filename):
            return mimetype

    # Try out libmagic, if it is installed.
    if magic:
        filetype = magic.from_file(filename, mime=True)
        if isinstance(filetype, bytes):
            filetype = filetype.decode('utf8')
        return filetype
    else:
        raise ValueError(("Could not identify the type of file '{}'; "
                          "try installing python-magic").format(filename))
