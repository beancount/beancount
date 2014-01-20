"""
Utilities for reading and writing CSV files.
"""
from collections import namedtuple
import csv
import re


def csv_dict_reader(fileobj, **kw):
    "Read a CSV file yielding normalized dictionary fields."
    reader = csv.DictReader(fileobj, **kw)
    reader.fieldnames = [re.sub('[^a-z]', '_', x.lower()).strip(' _') for x in reader.fieldnames]
    return reader


def csv_tuple_reader(fileobj, **kw):
    """Read a CSV file yielding namedtuple instances. The CSV file must have a header line."""
    reader = csv.reader(fileobj, **kw)
    ireader = iter(reader)
    Tuple = csv_parse_header(next(ireader))
    for row in ireader:
        try:
            yield Tuple(*row)
        except TypeError:
            # If there's an error, it's usually from a line that has a 'END OF
            # LINE' marker at the end, or some comment line.
            assert len(row) == 1


def csv_split_sections(rows):
    """Given a list of rows, split the list where rows have only a single entry.
    This is useful for CSV files with multiple sections, where the separator is
    a title. We use this to separate the multiple tables within the CSV files."""

    sections = {}

    section_name = None
    section_rows = []
    for row in rows:
        if len(row) == 1:
            if section_name:
                sections[section_name] = section_rows
            section_name = row[0]
            section_rows = []
        elif row:
            section_rows.append(row)

    return sections


def csv_parse_header(header_row):
    """Create a new class for the following rows from the header line.
    This both normalizes hte header line and assign"""

    fieldnames = []
    for index, column in enumerate(header_row):
        field = column.lower()
        field = re.sub(r'\bp/l\b', 'pnl', field)
        field = re.sub('[^a-z0-9]', '_', field)
        field = field.strip(' _')
        if not field:
            field = 'col{}'.format(index)
        assert field not in fieldnames, field
        fieldnames.append(field)

    return namedtuple('Row', fieldnames)
