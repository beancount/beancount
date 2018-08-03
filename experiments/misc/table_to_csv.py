"""Snippets of code able to convert a text rendered table to a CSV file.

This was part of some old (working) code.
I'll probably reuse this eventually.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import re


def get_columns_from_underlines(line):
    index = 0;
    columns = []
    for field in re.findall(r'\-+', line):
        end = index + len(field)
        columns.append((index, end))
        index = end + 1
    return columns


def table_to_csv(table_string):
    """Convert a formatted table to CSV rows."""
    lines = iter(table_string.splitlines())
    next(lines)
    columns = get_columns_from_underlines(next(lines))
    rows = []
    for line in lines:
        row = []
        for start, end in columns:
            field = line[start:end]
            row.append(field.strip())
        rows.append(row)
    return rows
