#!/usr/bin/env python3
"""Generate code to be inserted into Python or Lex sources containing (parts of)
regular expressions matching unicode characters belonging to particular
categories.
"""

__copyright__ = "Copyright (C) 2018  Adrián Medraño Calvo"
__license__ = "GNU GPLv2"

import argparse
import sys
import unicodedata
from collections import defaultdict
from itertools import count
from itertools import groupby


def list_chunks(l, n):
    """Split list in chunks of size n."""
    for it in range(0, len(l), n):
        yield l[it : it + n]


def groupby_sequences(iterable, keyfunc):
    """
    Group items of iterable in groups such that the result of applying keyfunc
    to them is consecutive.
    """
    if keyfunc is None:
        keyfunc = lambda x: x
    return groupby(iterable, lambda i, c=count(): keyfunc(i) - next(c))


def categorize_unicode():
    """
    Return a dictionary mapping Unicode general category names to the characters
    that belong to them.
    """
    bycat = defaultdict(list)
    for c in map(chr, range(sys.maxunicode + 1)):
        bycat[unicodedata.category(c)].append(c)
    return bycat


def bytes_prefix(bs):
    """
    Return all but the last byte from a byte sequence.
    """
    return bs[0:-1]


def bytes_last(bs):
    """
    Return the last byte from a byte sequence.
    """
    return bs[-1]


def py_unicode_literal(c):
    """
    Convert a character into a Python unicode literal (e.g. \u0064).
    """
    codepoint = ord(c)
    if codepoint < 65536:
        return "\\u{:04x}".format(codepoint)
    else:
        return "\\U{:08x}".format(codepoint)


def py_unicode_range_literal(beg, end):
    """
    Convert a range of characters into an unicode range literal
    (e.g. \u0064-\u006f), to be used in a Python regular expression's bracket
    expression.
    """
    if beg == end:
        return py_unicode_literal(beg)
    else:
        return "{}-{}".format(py_unicode_literal(beg), py_unicode_literal(end))


def py_unicode_ranges(chars):
    """
    Convert a set of characters into an string to be used in a Python
    regular expression's bracket expression (e.g. \u0062\u0064-\u006f).
    """
    ranges = []
    for _, seq in groupby_sequences(chars, ord):
        seq = list(seq)
        beg = seq[0]
        end = seq[-1]
        ranges.append(py_unicode_range_literal(beg, end))
    return "r'" + "".join(ranges) + "'"


def lex_byte_literal(b):
    """
    Convert a byte into a byte literal, as supported by lex (e.g., "\x40").
    """
    return "\\x{:02x}".format(b)


def lex_byte_literals(bs):
    """
    Convert a sequence of bytes into a sequence of byte literals, as supported
    by lex (e.g., "\x40").
    """
    return "".join(["\\x{:02x}".format(b) for b in bs])


def lex_byte_range_literal(prefix, beg, end):
    pat = lex_byte_literals(prefix)
    if beg == end:
        pat += lex_byte_literal(beg)
    else:
        pat += "["
        pat += lex_byte_literal(beg)
        pat += "-"
        pat += lex_byte_literal(end)
        pat += "]"
    return pat


def lex_unicode_ranges(name, chars):
    """
    Convert a set of characters into a string to be used in a lex regular
    expression
    """
    res = ""
    bss = [c.encode("utf-8") for c in chars]
    pats = []
    for prefix, byprefix in groupby(bss, bytes_prefix):
        for _, bysuffix_sequence in groupby_sequences(byprefix, bytes_last):
            bysuffix_sequence = list(bysuffix_sequence)
            beg = bysuffix_sequence[0][-1]
            end = bysuffix_sequence[-1][-1]
            pat = lex_byte_range_literal(prefix, beg, end)
            pats.append(pat)
    partnames = []
    MAXPATS = 50
    if len(pats) > MAXPATS:
        i = 0
        for pats in list_chunks(pats, MAXPATS):
            partname = "{}-{}".format(name, i)
            res += "{}\t{}\n".format(partname, "|".join(pats))
            partnames.append(partname)
            i += 1
        res += "{}\t{{{}}}".format(name, "}|{".join(partnames))
    else:
        res += "{}\t{}".format(name, "|".join(pats))
    return res


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("--name", required=True, help="name to assign the result")
    parser.add_argument(
        "--categories",
        required=True,
        help="unicode categories to print, separated by commas (e.g. Lu,Ll)",
    )
    parser.add_argument(
        "--format", required=True, choices=["py", "lex"], help="output format"
    )
    args = parser.parse_args()

    bycategory = categorize_unicode()
    chars = []
    for cat in args.categories.split(","):
        chars += bycategory[cat]
    chars.sort()

    if args.format == "py":
        print(py_unicode_ranges(chars))
    elif args.format == "lex":
        print(lex_unicode_ranges(args.name, chars))


if __name__ == "__main__":
    main()
