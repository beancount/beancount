"""
Code that can guess a MIME type for a filename.
We've had to enrich what is provided by default by magic because of some oddball types.
"""
import re
import warnings
import mimetypes
try:
    import magic
    if not hasattr(magic, 'from_file'):
        warnings.warn("You have an old version of magic; disabling.")
        magic = None
except ImportError:
    magic = None


EXTRA_FILE_TYPES = [
    (re.compile(regexp, re.I), filetype)
    for regexp, filetype in (
            (r'.*\.qbo$', 'application/vnd.intu.qbo'),
            (r'.*\.(qfx|ofx)$', 'application/x-ofx'),
    )]


def guess_file_type(filename):
    """Attempt to guess the type of the input file.
    Return a suitable mimetype, or None if we don't know."""

    # Try the regular mimetypes association.
    filetype, _ = mimetypes.guess_type(filename, False)

    if filetype is None:
        # Try out some extra ones that we know about.
        for regexp, mimtype in EXTRA_FILE_TYPES:
            if regexp.match(filename):
                filetype = mimtype
                break

    if filetype is None:
        if not magic:
            logging.error("You need to install python-magic.")
        else:
            # Okay, we couldn't figure it out from the filename; use libmagic
            # (if installed).
            bfiletype = magic.from_file(filename, mime=True)
            filetype = bfiletype.decode()

    return filetype
