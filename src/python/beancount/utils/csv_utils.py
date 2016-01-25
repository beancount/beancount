"""
Utilities for reading and writing CSV files.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import csv
import re
import io
import textwrap


def as_rows(string):
    """Split a string as rows of a CSV file.

    Args:
      string: A string to be split, the contents of a CSV file.
    Returns:
      Lists of lists of strings.
    """
    return list(csv.reader(io.StringIO(textwrap.dedent(string))))


def csv_clean_header(header_row):
    """Create a new class for the following rows from the header line.
    This both normalizes hte header line and assign

    Args:
      header_row: A list of strings, the row with header titles.
    Returns:
      A list of strings, with possibly modified (cleaned) row titles, of the
      same lengths.
    """
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
    return fieldnames


def csv_dict_reader(fileobj, **kw):
    """Read a CSV file yielding normalized dictionary fields.

    This is basically an alternative constructor for csv.DictReader that
    normalized the field names.

    Args:
      fileobj: A file object to be read.
      **kw: Optional arguments forwarded to csv.DictReader.
    Returns:
      A csv.DictReader object.
    """
    reader = csv.DictReader(fileobj, **kw)
    reader.fieldnames = csv_clean_header(reader.fieldnames)
    return reader


def csv_tuple_reader(fileobj, **kw):
    """Read a CSV file yielding namedtuple instances. The CSV file must have a
    header line.

    Args:
      fileobj: A file object to be read.
      **kw: Optional arguments forwarded to csv.DictReader.
    Yields:
      Nametuple instances, one for each row.
    """
    reader = csv.reader(fileobj, **kw)
    ireader = iter(reader)
    fieldnames = csv_clean_header(next(ireader))
    Tuple = collections.namedtuple('Row', fieldnames)
    for row in ireader:
        try:
            yield Tuple(*row)
        except TypeError as exc:
            # If there's an error, it's usually from a line that has a 'END OF
            # LINE' marker at the end, or some comment line.
            assert len(row) in (0, 1)


def csv_split_sections(rows):
    """Given rows, split them in at empty lines.
    This is useful for structured CSV files with multiple sections.

    Args:
      rows: A list of rows, which are themselves lists of strings.
    Returns:
      A list of sections, which are lists of rows, which are lists of strings.
    """
    sections = []
    current_section = []
    for row in rows:
        if row:
            current_section.append(row)
        else:
            sections.append(current_section)
            current_section = []
    if current_section:
        sections.append(current_section)
    return sections


def csv_split_sections_with_titles(rows):
    """Given a list of rows, split their sections. If the sections have single
    column titles, consume those lines as their names and return a mapping of
    section names


    This is useful for CSV files with multiple sections, where the separator is
    a title. We use this to separate the multiple tables within the CSV files.

    Args:
      rows: A list of rows (list-of-strings).
    Returns:
     A list of lists of rows (list-of-strings).

    """
    sections_map = {}
    for index, section in enumerate(csv_split_sections(rows)):
        # Skip too short sections, cannot possibly be a title.
        if len(section) < 2:
            continue
        if len(section[0]) == 1 and len(section[1]) != 1:
            name = section[0][0]
            section = section[1:]
        else:
            name = 'Section {}'.format(index)
        sections_map[name] = section
    return sections_map
