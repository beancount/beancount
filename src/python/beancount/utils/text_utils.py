"""Text manipulation utilities.
"""
__author__ = "Martin Blais <blais@furius.ca>"

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
    pages in incognito mode and thus may not be perfect.

    Args:
      text: An input string object.
    Returns:
      A string, with relevant numbers hopefully replaced with X'es.
    """
    return re.sub(r'\b([0-9,]+(?:\.[0-9]*)?)\b([ \t]+[^0-9,.]|$)', replace_number, text)
