"""Compute a hash of the source files in order to warn when the source goes out of date.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import hashlib
import warnings
from os import path


# List of input source files we should check against.
#
# Note: Prefer not to include the generated source files because they will
# differ between platforms and versions of Flex/Bison.
PARSER_SOURCE_FILES = [
    'lexer.l',
    'grammar.y',
    'parser.h',
    'parser.c',
]

def hash_parser_source_files():
    """Compute a unique hash of the parser's Python code in order to bake that into
    the extension module. This is used at load-time to verify that the extension
    module and the corresponding Python codes match each other. If not, it
    issues a warning that you should rebuild your extension module.

    Returns:
      A string, the hexadecimal unique hash of relevant source code that should
      trigger a recompilation.
    """
    md5 = hashlib.md5()
    for filename in PARSER_SOURCE_FILES:
        fullname = path.join(path.dirname(__file__), filename)
        if not path.exists(fullname):
            return None
        with open(fullname, 'rb') as file:
            md5.update(file.read())
    # Note: Prepend a character in front of the hash because under Windows MSDEV
    # removes escapes, and if the hash starts with a number it fails to
    # recognize this is a string. A small compromise for portability.
    return md5.hexdigest()


def check_parser_source_files():
    """Check the extension module's source hash and issue a warning if the
    current source differs from that of the module.

    If the source files aren't located in the Python source directory, ignore
    the warning, we're probably running this from an installed based, in which
    case we don't need to check anything (this check is useful only for people
    running directly from source).
    """
    parser_source_hash = hash_parser_source_files()
    if parser_source_hash is None:
        return
    from . import _parser
    if _parser.SOURCE_HASH != parser_source_hash:
        warnings.warn("The Beancount parser C extension module is out-of-date. "
                      "You need to rebuild.")
