"""Text manipulation utilities.
"""
import re


def replace_number(mo):
    """Replace a single number matched from text into X'es.
    'mo' is a MatchObject from a regular expressions match.
    (Use this with re.sub())."""
    return re.sub('[0-9]', 'X', mo.group(1)) + mo.group(2)

def replace_numbers(text):
    """Replace all numbers found within text."""
    return re.sub(r'\b([0-9,]+\.[0-9]*)\b([^0-9,.]|$)', replace_number, text)
