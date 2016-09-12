#!/usr/bin/env python3
"""Align a beancount/ledger input file's numbers.

This reformats at beancount or ledger input file so that the amounts in the
postings are all aligned to the same column. The currency should match.

Note: this does not parse the Beancount ledger. It simply uses regular
expressions and text manipulations to do its work.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import collections
import io
import re
import sys


# A regular expression that matches an account name.
ACCOUNT_RE = '([A-Z][A-Za-z0-9\-]+)(:[A-Z][A-Za-z0-9\-]*)+'


def align_beancount(contents):
    """Reformat Beancount input to align all the numbers at the same column.

    Args:
      contents: A string, Beancount input syntax to reformat.
    Returns:
      A string, reformatted Beancount input with all the number aligned.
      No other changes than whitespace changes should be present between that
      return value and the input contents.
    """
    # Find all lines that have a number in them and calculate the maximum length
    # of the stripped prefix and the number.
    match_pairs = []
    for index, line in enumerate(contents.splitlines()):
        match = re.match(r'([^";]*?)\s+([-+]?\s*[\d,]+(?:\.\d*)?)\s+([A-Z0-9]+\b.*)', line)
        if match:
            prefix, number, rest = match.groups()
            match_pairs.append((prefix, number, rest))
        else:
            match_pairs.append((line, None, None))

    # Normalize whitespace before lines that has some indent and an account
    # name.
    norm_match_pairs = normalize_indent_whitespace(match_pairs)

    # Compute the maximum widths.
    filtered_pairs = [(prefix, number)
                      for prefix, number, _ in match_pairs
                      if number is not None]

    if filtered_pairs:
        max_prefix_width = max(len(prefix) for prefix, _ in filtered_pairs)
        max_num_width = max(len(number) for _, number in filtered_pairs)
    else:
        max_prefix_width = 0
        max_num_width = 0

    # Create a format that will admit the maximum width of all prefixes equally.
    line_format = '{{:<{prefix_width}}}  {{:>{num_width}}} {{}}'.format(
        prefix_width=max_prefix_width,
        num_width=max_num_width)

    # Process each line to an output buffer.
    output = io.StringIO()
    for prefix, number, rest in norm_match_pairs:
        if number is None:
            output.write(prefix)
        else:
            output.write(line_format.format(prefix.rstrip(), number, rest))
        output.write('\n')
    formatted_contents = output.getvalue()

    # Ensure that the file before and after have only whitespace differences.
    # This is a sanity check, to make really sure we never change anything but whitespace,
    # so it's safe.
    # open('/tmp/before', 'w').write(re.sub(r'[ \t]+', ' ', contents))
    # open('/tmp/after', 'w').write(re.sub(r'[ \t]+', ' ', formatted_contents))
    old_stripped = re.sub(r'[ \t\n]+', ' ', contents.rstrip())
    new_stripped = re.sub(r'[ \t\n]+', ' ', formatted_contents.rstrip())
    assert (old_stripped == new_stripped), (old_stripped, new_stripped)

    return formatted_contents


# Note: This is generic, could be moved to utils.
def compute_most_frequent(iterable):
    """Compute the frequencies of the given elements and return the most frequent.

    Args:
      iterable: A collection of hashable elements.
    Returns:
      The most frequent element. If there are no elements in the iterable,
      return None.
    """
    frequencies = collections.defaultdict(int)
    for element in iterable:
        frequencies[element] += 1
    if not frequencies:
        return None
    counts = sorted((count, element)
                    for element, count in frequencies.items())
    # Note: In case of a tie, this chooses the longest width.
    # We could eventually make this an option.
    return counts[-1][1]


def normalize_indent_whitespace(match_pairs):
    """Normalize whitespace before lines that has some indent and an account name.

    Args:
      match_pairs: A list of (prefix, number, rest) tuples.
    Returns:
      Another list of (prefix, number, rest) tuples, where prefix may have been
      adjusted with a different whitespace prefi.
    """
    # Compute most frequent account name prefix.
    match_posting = re.compile(r'([ \t]+)({})'.format(ACCOUNT_RE)).match
    width = compute_most_frequent(
        len(match.group(1))
        for match in (match_posting(prefix)
                      for prefix, _, _ in match_pairs)
        if match is not None)
    if width:
        norm_format = ' ' * width + '{}'

    # Make the necessary adjustments.
    adjusted_pairs = []
    for tup in match_pairs:
        prefix, number, rest = tup
        match = match_posting(prefix)
        if match is not None:
            tup = (norm_format.format(match.group(2)), number, rest)
        adjusted_pairs.append(tup)
    return adjusted_pairs


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename', nargs='?', help='Beancount filename')

    parser.add_argument('-o', '--output', action='store',
                        help="Output file (stdout if not specified)")

    opts = parser.parse_args()

    # Read the original contents.
    file = open(opts.filename) if opts.filename not in (None, '-') else sys.stdin
    contents = file.read()

    # Align the contents.
    formatted_contents = align_beancount(contents)

    # Make sure not to open the output file until we've passed out sanity
    # checks. We want to allow overwriting the input file, but want to avoid
    # losing it in case of errors!
    outfile = open(opts.output, 'w') if opts.output else sys.stdout
    outfile.write(formatted_contents)

    return 0


if __name__ == '__main__':
    main()
