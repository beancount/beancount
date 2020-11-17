"""Text manipulation utilities.
"""
__copyright__ = "Copyright (C) 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"

import tempfile
import re


def replace_number(match):
    """Replace a single number matched from text into X'es.
    'match' is a MatchObject from a regular expressions match.
    (Use this with re.sub()).

    Args:
      match: A MatchObject.
    Returns:
      A replacement string, consisting only of X'es.
    """
    return re.sub('[0-9]', 'X', match.group(1)) + match.group(2)

def replace_numbers(text):
    """Replace all numbers found within text.

    Note that this is a heuristic used to filter out private numbers from web
    pages in incognito mode and thus may not be perfect. It should let through
    numbers which are part of URLs.

    Args:
      text: An input string object.
    Returns:
      A string, with relevant numbers hopefully replaced with X'es.

    """
    return re.sub(r'\b([0-9,]+(?:\.[0-9]*)?)\b([ \t<]+[^0-9,.]|$)', replace_number, text)


def entitize_ampersand(filename):
    """Convert unescaped ampersand characters (&) to XML entities.

    This is used to fix code that has been programmed by bad developers who
    didn't think about escaping the entities in their strings.
    file does not contain
    Args:
      filename: A string, the name of the file to convert.
    Returns:
      A self-destructing NamedTemporaryFile object that has been flushed and
      which you may read to obtain the fixed contents.
    """
    tidy_file = tempfile.NamedTemporaryFile(suffix='.xls', mode='w', delete=False)
    with open(filename) as infile:
        contents = infile.read()
    new_contents = re.sub('&([^;&]{12})', '&amp;\\1', contents)
    tidy_file.write(new_contents)
    tidy_file.flush()
    return tidy_file
