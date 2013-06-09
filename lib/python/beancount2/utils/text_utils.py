"""Text manipulation utilities.
"""
import re


class Matcher:
    """A convenience matcher to do regular expression matching in a conditional.
    This object acts like the regular expression module, but stores the
    MatchObject results as an attribute so you can access the last matched
    result. Use it like this:

       matcher = Matcher()
       ...
       if matcher.match('SOME (NAME)', text):
           matched_text = matcher.mo.group(1)

    """

    def match(self, *args, **kw):
        self.mo = re.match(*args, **kw)
        return self.mo

    def search(self, *args, **kw):
        self.mo = re.match(*args, **kw)
        return self.mo


def replace_number(mo):
    """Replace a single number matched from text into X'es.
    'mo' is a MatchObject from a regular expressions match.
    (Use this with re.sub())."""
    return re.sub('[0-9]', 'X', mo.group(1)) + mo.group(2)

def replace_numbers(text):
    """Replace all numbers found within text."""
    return re.sub(r'\b([0-9,]+\.[0-9]*)\b([^0-9,.]|$)', replace_number, text)
