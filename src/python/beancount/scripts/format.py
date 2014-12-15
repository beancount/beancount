#!/usr/bin/env python3
"""Align a beancount/ledger input file's numbers.

This reformats at beancount or ledger input file so that the amounts in the
postings are all aligned to the same column. The currency should match.

Note: this does not parse the Beancount ledger. It simply uses regular
expressions and text manipulations to do its work.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import io
import re
import sys


def align_beancount(contents):
    """Reformat Beancount input to align all the numbers at the same column.

    Args:
      contents: A string, Beancount input syntax to reforamt.
    Returns:
      A string, reformatted Beancount input with all the number aligned.
      No other changes than whitespace changes should be present between that
      return value and the input contents.
    """
    # Find all lines that have a number in them and calculate the maximum length
    # of the stripped prefix and the number.
    match_pairs = []
    max_prefix_width = 0
    max_num_width = 0
    for index, line in enumerate(contents.splitlines()):
        match = re.match(r'([^";]*?)\s+([-+]?\s*\d+(?:\.\d*)?)\s+([A-Z0-9]+\b.*)', line)
        if match:
            prefix, number, rest = match.groups()
            match_pairs.append((prefix, number, rest))
            max_prefix_width = max(max_prefix_width, len(prefix))
            max_num_width = max(max_num_width, len(number))
        else:
            match_pairs.append((line, None, None))

    # Create a format that will admit the maximum width of all prefixes equally.
    line_format = '{{:<{prefix_width}}}  {{:>{num_width}}} {{}}'.format(
        prefix_width=max_prefix_width,
        num_width=max_num_width)

    # Process each line to an output buffer.
    output = io.StringIO()
    for prefix, number, rest in match_pairs:
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
    assert re.sub(r'[ \t]+', ' ', contents) == re.sub(r'[ \t]+', ' ', formatted_contents)

    return formatted_contents


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('filename', help='Beancount filename')

    parser.add_argument('-o', '--output', action='store',
                        help="Output file (stdout if not specified)")

    opts = parser.parse_args()

    # Read the original contents.
    contents = open(opts.filename).read()

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
