#!/usr/bin/env python3
"""Identify a column of text that contains hierarchical id and treeify that column.

This script will inspect a text file and attempt to find a vertically
left-aligned column of text that contains identifiers with multiple components,
such as "Assets:US:Bank:Checking", and replace those by a tree-like structure rendered
in ASCII, inserting new empty lines where necessary to create the tree.

Note: If your paths have spaces in them, this will not work. Space is used as a
delimiter to detect the end of a column. You can customize the delimiter with an
option.
"""
__copyright__ = "Copyright (C) 2013-2017  Martin Blais"
__license__ = "GNU GPLv2"

# IMPORTANT: This should be usable as a standalone script. Do not depend on Beancount.

import argparse
import collections
import re
import sys


# Default regular expressions used for splitting.
DEFAULT_PATTERN = (r"(Assets|Liabilities|Equity|Income|Expenses)"
                   r"(:[A-Z][A-Za-z0-9-_']*)*")
DEFAULT_DELIMITER = "[ \t]+"
DEFAULT_SPLITTER = ":"

# Regular expressions used for loose accounts.
LOOSE_PATTERN = r"\b([A-Za-z0-9-_']+)(:[A-Za-z0-9-_']+)+\b"
LOOSE_SPLITTER = r":"

# Regular expressions used for filenames matching.
FILENAME_PATTERN = r"([^ \t]*)(/[^ \t]*)+"
FILENAME_SPLITTER = r"/"


def find_column(lines, pattern, delimiter):
    """Find a valid column with hierarchical data in the text lines.

    Args:
      lines: A list of strings, the contents of the input.
      pattern: A regular expression for the hierarchical entries.
      delimiter: A regular expression that dictates how we detect the
        end of a column. Normally this is a single space. If the patterns
        contain spaces, you will need to increase this.
    Returns:
      A tuple of
        matches: A list of (line-number, name) tuples where 'name' is the
          hierarchical string to treeify and line-number is an integer, the
          line number where this applies.
        left: An integer, the leftmost column.
        right: An integer, the rightmost column.
      Note that not all line numbers may be present, so you may need to
      skip some. However, they are in guaranteed in sorted order.
    """
    # A mapping of the line beginning position to its match object.
    beginnings = collections.defaultdict(list)
    pattern_and_whitespace = "({})(?P<ws>{}.|$)".format(pattern, delimiter)
    for no, line in enumerate(lines):
        for match in re.finditer(pattern_and_whitespace, line):
            beginnings[match.start()].append((no, line, match))

    # For each potential column found, verify that it is valid. A valid column
    # will have the maximum of its content text not overlap with any of the
    # following text. We assume that a column will have been formatted to full
    # width and that no text following the line overlap with the column, even in
    # its trailing whitespace.
    #
    # In other words, the following example is a violation because "10,990.74"
    # overlaps with the end of "Insurance" and so this would not be recognized
    # as a valid column:
    #
    # Expenses:Food:Restaurant     10,990.74 USD
    # Expenses:Health:Dental:Insurance   208.80 USD
    #
    for leftmost_column, column_matches in sorted(beginnings.items()):

        # Compute the location of the rightmost column of text.
        rightmost_column = max(match.end(1) for _, _, match in column_matches)

        # Compute the leftmost location of the content following the column text
        # and past its whitespace.
        following_column = min(match.end() if match.group('ws') else 10000
                               for _, _, match in column_matches)

        if rightmost_column < following_column:
            # We process only the very first match.
            return_matches = [(no, match.group(1).rstrip())
                              for no, _, match in column_matches]
            return return_matches, leftmost_column, rightmost_column



class Node(list):
    """A node with a name attribute, a list of line numbers and a list of children
    (from its parent class).
    """
    def __init__(self, name):
        list.__init__(self)
        self.name = name
        self.nos = []

    def __str__(self):
        return '<Node {} {}>'.format(self.name, [node.name for node in self])

    __repr__ = __str__


def dump_tree(node, file=sys.stdout, prefix=''):
    """Render a tree as a tree.

    Args:
      node: An instance of Node.
      file: A file object to write to.
      prefix: A prefix string for each of the lines of the children.
    """
    file.write(prefix)
    file.write(node.name)
    file.write('\n')
    for child in node:
        dump_tree(child, file, prefix + '... ')


def create_tree(column_matches, regexp_split):
    """Build up a tree from a list of matches.

    Args:
      column_matches: A list of (line-number, name) pairs.
      regexp_split: A regular expression string, to use for splitting the names
        of components.
    Returns:
      An instance of Node, the root node of the created tree.
    """
    root = Node('')
    for no, name in column_matches:
        parts = re.split(regexp_split, name)
        node = root
        for part in parts:
            last_node = node[-1] if node else None
            if last_node is None or last_node.name != part:
                last_node = Node(part)
                node.append(last_node)
            node = last_node
        node.nos.append(no)
    return root


PREFIX_CHILD_1 = '|-- '
PREFIX_CHILD_C = '|   '
PREFIX_LEAF_1 = '`-- '
PREFIX_LEAF_C = '    '

def render_tree(root):
    """Render a tree of nodes.

    Returns:
      A list of tuples of (first_line, continuation_line, node) where
        first_line: A string, the first line to render, which includes the
          account name.
        continuation_line: A string, further line to render if necessary.
        node: The Node instance which corresponds to this line.
      and an integer, the width of the new columns.
    """
    # Compute all the lines ahead of time in order to calculate the width.
    lines = []

    # Start with the root node. We push the constant prefix before this node,
    # the account name, and the RealAccount instance. We will maintain a stack
    # of children nodes to render.
    stack = [('', root.name, root, True)]
    while stack:
        prefix, name, node, is_last = stack.pop(-1)

        if node is root:
            # For the root node, we don't want to render any prefix.
            first = cont = ''
        else:
            # Compute the string that precedes the name directly and the one below
            # that for the continuation lines.
            #  |
            #  @@@ Bank1    <----------------
            #  @@@ |
            #  |   |-- Checking
            if is_last:
                first = prefix + PREFIX_LEAF_1
                cont = prefix + PREFIX_LEAF_C
            else:
                first = prefix + PREFIX_CHILD_1
                cont = prefix + PREFIX_CHILD_C

        # Compute the name to render for continuation lines.
        #  |
        #  |-- Bank1
        #  |   @@@       <----------------
        #  |   |-- Checking
        if len(node) > 0:
            cont_name = PREFIX_CHILD_C
        else:
            cont_name = PREFIX_LEAF_C

        # Add a line for this account.
        if not (node is root and not name):
            lines.append((first + name,
                          cont + cont_name,
                          node))

        # Push the children onto the stack, being careful with ordering and
        # marking the last node as such.
        if node:
            child_items = reversed(node)
            child_iter = iter(child_items)
            child_node = next(child_iter)
            stack.append((cont, child_node.name, child_node, True))
            for child_node in child_iter:
                stack.append((cont, child_node.name, child_node, False))

    if not lines:
        return lines

    # Compute the maximum width of the lines and convert all of them to the same
    # maximal width. This makes it easy on the client.
    max_width = max(len(first_line) for first_line, _, __ in lines)
    line_format = '{{:{width}}}'.format(width=max_width)
    return [(line_format.format(first_line),
             line_format.format(cont_line),
             node)
            for (first_line, cont_line, node) in lines], max_width


def enum_tree_by_input_line_num(tree_lines):
    """Accumulate the lines of a tree until a line number is found.

    Args:
      tree_lines: A list of lines as returned by render_tree.
    Yields:
      Pairs of (line number, list of (line, node)).
    """
    pending = []
    for first_line, cont_line, node in tree_lines:
        if not node.nos:
            pending.append((first_line, node))
        else:
            line = first_line
            for no in node.nos:
                pending.append((line, node))
                line = cont_line
                yield (no, pending)
                pending = []
    if pending:
        yield (None, pending)


def _main():
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument('input', nargs='?', action='store',
                        help='Name of the file to process (default: stdin)')

    parser.add_argument('-o', '--output', action='store',
                        help='Name of the file to write (default: stdout)')

    parser.add_argument('-r', '--pattern', action='store',
                        default=None,
                        help=("Pattern for repeatable components "
                              "(default: \"{}\")".format(DEFAULT_PATTERN)))

    parser.add_argument('-d', '--delimiter', action='store',
                        default=DEFAULT_DELIMITER,
                        help=("Delimiter pattern to detect the end of a column text. "
                              "If your pattens contain strings, you may want to set this "
                              "to a longer string, like ' {{2,}}' "
                              "(default: \"{}\")").format(DEFAULT_DELIMITER))

    parser.add_argument('-s', '--split', action='store',
                        default=DEFAULT_SPLITTER,
                        help="Pattern splitting into components (default: \"{}\")".format(
                            DEFAULT_SPLITTER))

    parser.add_argument('-F', '--filenames', action='store_true',
                        help="Use pattern and split suitable for filenames")

    parser.add_argument('-A', '--loose-accounts', action='store_true',
                        help="Use pattern and split suitable for loose account names")

    parser.add_argument('--filler', action='store',
                        default=' ',
                        help="Filler string for new lines inserted for formatting")

    args = parser.parse_args()

    if sum([1 if expr else 0 for expr in (args.filenames,
                                          args.loose_accounts,
                                          args.pattern)]) > 1:
        parser.error("Conflicted pattern options")

    if args.pattern is None:
        args.pattern = DEFAULT_PATTERN

    if args.filenames:
        # Note: you could spit an error if the other options are set: "you may
        # not use --filenames and specify a pattern or split" or somesuch.
        args.pattern = FILENAME_PATTERN
        args.split = FILENAME_SPLITTER

    elif args.loose_accounts:
        args.pattern = LOOSE_PATTERN
        args.split = LOOSE_SPLITTER

    # Open input and output files.
    input_file = open(args.input, 'r') if args.input else sys.stdin
    output_file = open(args.output, 'w') if args.output else sys.stdout
    lines = list(input_file)

    # Find a column in the file. If not found, this will return None.
    result = find_column(lines, args.pattern, args.delimiter)
    if result is None:
        print("WARNING: Could not find any valid column in input",
              file=sys.stderr)
        for line in lines:
            output_file.write(line)
        output_file.close()
        return -1
    column_matches, left, right = result

    # Process the input lines again, this time with the column.
    root = create_tree(column_matches, args.split)

    # Render the tree we just inferred from the list of names.
    tree_lines, new_column_width = render_tree(root)

    # Compute minimum width of the resulting tree. It should not be less than
    # the original width of the column.
    old_column_width = right - left
    line_format = "{{:{}}}".format(max(old_column_width, new_column_width))

    # Iterate the tree by input line number. This is done so that we can can
    # render the new hierarchy lines as closely as possible to real input
    # lines... we delay rendering those until we have a match with a real line
    # to render.
    tree_iter = enum_tree_by_input_line_num(tree_lines)
    no, next_tree_lines = next(tree_iter)

    # Iterate over the input lines, rendering the tree lines only when
    # necessary.
    input_lines_iter = iter(enumerate(lines))
    for input_no, input_line in input_lines_iter:
        if input_no < no:
            # Catch up to the next line we need to render for the tree by
            # forwarding just the input line.
            output_file.write(input_line)
        else:
            assert input_no == no, (input_no, no)
            for line, node in next_tree_lines:
                if not node.nos:
                    # Render new lines, inserted just for the hierarchy.
                    prefix_string = args.filler * (left//len(args.filler)+1)
                    output_file.write(prefix_string[:left])
                    output_file.write(line.rstrip())
                    output_file.write('\n')
                else:
                    # Render lines that replace previous lines, with prefix and
                    # suffix.
                    prefix = input_line[:left]
                    suffix = input_line[right:].rstrip('\r\n')
                    out_line = prefix + line_format.format(line) + suffix
                    output_file.write(out_line.rstrip())
                    output_file.write('\n')
            try:
                no, next_tree_lines = next(tree_iter)
            except StopIteration:
                break

    # Render the rest of the input lines after the tree is done rendering.
    for _, input_line in input_lines_iter:
        output_file.write(input_line)


def main():
    try:
        _main()
    except BrokenPipeError:
        pass


if __name__ == '__main__':
    main()
