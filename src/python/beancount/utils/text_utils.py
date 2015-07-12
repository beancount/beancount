"""Text manipulation utilities.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import re


def replace_number(match):
    """Replace a single number matched from text into X'es.
    'match' is a MatchObject from a regular expressions match.
    (Use this with re.sub())."""
    return re.sub('[0-9]', 'X', match.group(1)) + match.group(2)

def replace_numbers(text):
    """Replace all numbers found within text."""
    return re.sub(r'\b([0-9,]+\.[0-9]*)\b([^0-9,.]|$)', replace_number, text)
