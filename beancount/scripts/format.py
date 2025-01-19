__copyright__ = "Copyright (C) 2014-2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import io

import click
import regex

from beancount.core import account
from beancount.core import amount
from beancount.parser.version import VERSION

NUMBER_RE = r"[-+]?\s*[\d,]+(?:\.\d*)?"
PARENTHESIZED_BINARY_OP_RE = rf"\({NUMBER_RE}\s*[-+*/]\s*{NUMBER_RE}\)"


def align_beancount(contents, prefix_width=None, num_width=None, currency_column=None):
    """Reformat Beancount input to align all the numbers at the same column.

    Args:
      contents: A string, Beancount input syntax to reformat.
      prefix_width: An integer, the width in characters to render the account
        name to. If this is not specified, a good value is selected
        automatically from the contents of the file.
      num_width: An integer, the width to render each number. If this is not
        specified, a good value is selected automatically from the contents of
        the file.
      currency_column: An integer, the column at which to align the currencies.
        If given, this overrides the other options.
    Returns:
      A string, reformatted Beancount input with all the number aligned.
      No other changes than whitespace changes should be present between that
      return value and the input contents.

    """
    # Find all lines that have a number in them and calculate the maximum length
    # of the stripped prefix and the number.
    match_pairs = []
    for line in contents.splitlines():
        match = regex.match(
            rf'(^\d[^";]*?|\s+{account.ACCOUNT_RE})\s+'
            rf"({PARENTHESIZED_BINARY_OP_RE}|{NUMBER_RE})\s+"
            rf"((?:{amount.CURRENCY_RE})\b.*)",
            line,
        )
        if match:
            prefix, number, rest = match.groups()
            match_pairs.append((prefix, number, rest))
        else:
            match_pairs.append((line, None, None))

    # Normalize whitespace before lines that has some indent and an account
    # name.
    norm_match_pairs = normalize_indent_whitespace(match_pairs)

    if currency_column:
        output = io.StringIO()
        for prefix, number, rest in norm_match_pairs:
            if number is None:
                output.write(prefix)
            else:
                num_of_spaces = currency_column - len(prefix) - len(number) - 4
                spaces = " " * num_of_spaces
                output.write(prefix + spaces + "  " + number + " " + rest)
            output.write("\n")
        return output.getvalue()

    # Compute the maximum widths.
    filtered_pairs = [
        (prefix, number) for prefix, number, _ in match_pairs if number is not None
    ]

    if filtered_pairs:
        max_prefix_width = max(len(prefix) for prefix, _ in filtered_pairs)
        max_num_width = max(len(number) for _, number in filtered_pairs)
    else:
        max_prefix_width = 0
        max_num_width = 0

    # Use user-supplied overrides, if available
    if prefix_width:
        max_prefix_width = prefix_width
    if num_width:
        max_num_width = num_width

    # Create a format that will admit the maximum width of all prefixes equally.
    line_format = "{{:<{prefix_width}}}  {{:>{num_width}}} {{}}".format(
        prefix_width=max_prefix_width, num_width=max_num_width
    )

    # Process each line to an output buffer.
    output = io.StringIO()
    for prefix, number, rest in norm_match_pairs:
        if number is None:
            output.write(prefix)
        else:
            output.write(line_format.format(prefix.rstrip(), number, rest))
        output.write("\n")
    formatted_contents = output.getvalue()

    # Ensure that the file before and after have only whitespace differences.
    # This is a sanity check, to make really sure we never change anything but whitespace,
    # so it's safe.
    # open('/tmp/before', 'w').write(regex.sub(r'[ \t]+', ' ', contents))
    # open('/tmp/after', 'w').write(regex.sub(r'[ \t]+', ' ', formatted_contents))
    old_stripped = regex.sub(r"[ \t\n]+", " ", contents.rstrip())
    new_stripped = regex.sub(r"[ \t\n]+", " ", formatted_contents.rstrip())
    assert old_stripped == new_stripped, (old_stripped, new_stripped)

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
    frequencies = collections.Counter(iterable)
    if not frequencies:
        return None
    counts = sorted((count, element) for element, count in frequencies.items())
    # Note: In case of a tie, this chooses the longest width.
    # We could eventually make this an option.
    return counts[-1][1]


def normalize_indent_whitespace(match_pairs):
    """Normalize whitespace before lines that has some indent and an account name.

    Args:
      match_pairs: A list of (prefix, number, rest) tuples.
    Returns:
      Another list of (prefix, number, rest) tuples, where prefix may have been
      adjusted with a different whitespace prefix.
    """
    # Compute most frequent account name prefix.
    match_posting = regex.compile(r"([ \t]+)({}.*)".format(account.ACCOUNT_RE)).match
    width = compute_most_frequent(
        len(match.group(1))
        for match in (match_posting(prefix) for prefix, _, _ in match_pairs)
        if match is not None
    )
    norm_format = " " * (width or 0) + "{}"

    # Make the necessary adjustments.
    adjusted_pairs = []
    for tup in match_pairs:
        prefix, number, rest = tup
        match = match_posting(prefix)
        if match is not None:
            tup = (norm_format.format(match.group(2)), number, rest)
        adjusted_pairs.append(tup)
    return adjusted_pairs


@click.command()
@click.argument("filename", type=click.File("r", encoding="utf-8"))
@click.option(
    "--output",
    "-o",
    type=click.File("w", encoding="utf-8"),
    default="-",
    help="Output file.",
)
@click.option("--prefix-width", "-w", type=int, help="Force fixed prefix width.")
@click.option("--num-width", "-W", type=int, help="Force fixed numbers width.")
@click.option("--currency-column", "-c", type=int, help="Align curreencies to this column.")
@click.version_option(message=VERSION)
def main(filename, output, prefix_width, num_width, currency_column):
    """Automatically format a Beancount ledger.

    This reformats at beancount or ledger input file so that the
    amounts in the postings are all aligned to the same column.

    If the alignment is not specified with the --currency-column
    option, it is determined automatically from the maximum lenght of
    the account names and of the numerical part of the transaction
    amounts in the input file. The automatically determined widths can
    be overridden with the --prefix-width and --num-width options
    respectively.

    Note: this tool does not parse the Beancount ledger. It simply
    uses regular expressions and text manipulations to do its work.

    """
    contents = filename.read()

    formatted_contents = align_beancount(contents, prefix_width, num_width, currency_column)

    # Click opens files for writing in lazy mode. This prevents
    # truncating the input file untill it has been processed and
    # validated, avoid data loss in case of errors.
    output.write(formatted_contents)


if __name__ == "__main__":
    main()
