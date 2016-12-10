"""Table rendering.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import csv
import collections
import io
import itertools


# An unrendered table data structure. This is a table-like report data
# structure--a two-dimensional array of cells--that is used as an intermediate
# state between the internal data structures of Beancount and format-specific
# reports.
#
# Attributes:
#   columns: A sequence of strings, names for each column.
#   header: A sequence of strings, a header to be rendered for each column.
#   rows: A list of rows, each of which is a sequence of strings, the
#     contents of all the cells of the table body.
Table = collections.namedtuple('Table', 'columns header body')


def attribute_to_title(fieldname):
    """Convert programming id into readable field name.

    Args:
      fieldname: A string, a programming ids, such as 'book_value'.
    Returns:
      A readable string, such as 'Book Value.'
   """
    return fieldname.replace('_', ' ').title()


def create_table(rows, field_spec=None):
    """Convert a list of tuples to an table report object.

    Args:
      rows: A list of tuples.
      field_spec: A list of strings, or a list of
        (FIELDNAME-OR-INDEX, HEADER, FORMATTER-FUNCTION)
        triplets, that selects a subset of the fields is to be rendered as well
        as their ordering. If this is a dict, the values are functions to call
        on the fields to render them. If a function is set to None, we will just
        call str() on the field.
    Returns:
      A Table instance.
    """
    # Normalize field_spec to a dict.
    if field_spec is None:
        namedtuple_class = type(rows[0])
        field_spec = [(field, None, None)
                      for field in namedtuple_class._fields]

    elif isinstance(field_spec, (list, tuple)):
        new_field_spec = []
        for field in field_spec:
            if isinstance(field, tuple):
                assert len(field) <= 3, field
                if len(field) == 1:
                    field, = field
                    new_field_spec.append((field, None, None))
                elif len(field) == 2:
                    field, header = field
                    new_field_spec.append((field, header, None))
                elif len(field) == 3:
                    new_field_spec.append(field)
            else:
                if isinstance(field, str):
                    title = attribute_to_title(field)
                elif isinstance(field, int):
                    title = "Field {}".format(field)
                else:
                    raise ValueError("Invalid type for column name")
                new_field_spec.append((field, title, None))

        field_spec = new_field_spec

    # Ensure a nicely formatted header.
    field_spec = [((name, attribute_to_title(name), formatter)
                   if header_ is None
                   else (name, header_, formatter))
                  for (name, header_, formatter) in field_spec]

    assert isinstance(field_spec, list), field_spec
    assert all(len(x) == 3 for x in field_spec), field_spec

    # Compute the column names.
    columns = [name for (name, _, __) in field_spec]

    # Compute the table header.
    header = [header_column for (_, header_column, __) in field_spec]

    # Compute the table body.
    body = []
    for row in rows:
        body_row = []
        for name, _, formatter in field_spec:
            if isinstance(name, str):
                value = getattr(row, name)
            elif isinstance(name, int):
                value = row[name]
            else:
                raise ValueError("Invalid type for column name")
            if value is not None:
                if formatter is not None:
                    value = formatter(value)
                else:
                    value = str(value)
            else:
                value = ''
            body_row.append(value)
        body.append(body_row)

    return Table(columns, header, body)


def table_to_html(table, classes=None, file=None):
    """Render a Table to HTML.

    Args:
      table: An instance of a Table.
      classes: A list of string, CSS classes to set on the table.
      file: A file object to write to. If no object is provided, this
        function returns a string.
    Returns:
      A string, the rendered table, or None, if a file object is provided
      to write to.
    """
    # Initialize file.
    oss = io.StringIO() if file is None else file
    oss.write('<table class="{}">\n'.format(' '.join(classes or [])))

    # Render header.
    if table.header:
        oss.write('  <thead>\n')
        oss.write('    <tr>\n')
        for header in table.header:
            oss.write('      <th>{}</th>\n'.format(header))
        oss.write('    </tr>\n')
        oss.write('  </thead>\n')

    # Render body.
    oss.write('  <tbody>\n')
    for row in table.body:
        oss.write('    <tr>\n')
        for cell in row:
            oss.write('      <td>{}</td>\n'.format(cell))
        oss.write('    </tr>\n')
    oss.write('  </tbody>\n')

    # Render footer.
    oss.write('</table>\n')
    if file is None:
        return oss.getvalue()


def table_to_text(table,
                  column_interspace=" ",
                  formats=None):
    """Render a Table to ASCII text.

    Args:
      table: An instance of a Table.
      column_interspace: A string to render between the columns as spacer.
      formats: An optional dict of column name to a format character that gets
        inserted in a format string specified, like this (where '<char>' is):
        {:<char><width>}. A key of '*' will provide a default value, like
        this, for example: (... formats={'*': '>'}).
    Returns:
      A string, the rendered text table.
    """
    column_widths = compute_table_widths(itertools.chain([table.header],
                                                         table.body))

    # Insert column format chars and compute line formatting string.
    column_formats = []
    if formats:
        default_format = formats.get('*', None)
    for column, width in zip(table.columns, column_widths):
        if column and formats:
            format_ = formats.get(column, default_format)
            if format_:
                column_formats.append("{{:{}{:d}}}".format(format_, width))
            else:
                column_formats.append("{{:{:d}}}".format(width))
        else:
            column_formats.append("{{:{:d}}}".format(width))

    line_format = column_interspace.join(column_formats) + "\n"
    separator = line_format.format(*[('-' * width) for width in column_widths])

    # Render the header.
    oss = io.StringIO()
    if table.header:
        oss.write(line_format.format(*table.header))

    # Render the body.
    oss.write(separator)
    for row in table.body:
        oss.write(line_format.format(*row))
    oss.write(separator)

    return oss.getvalue()


def table_to_csv(table, file=None, **kwargs):
    """Render a Table to a CSV file.

    Args:
      table: An instance of a Table.
      file: A file object to write to. If no object is provided, this
        function returns a string.
      **kwargs: Optional arguments forwarded to csv.writer().
    Returns:
      A string, the rendered table, or None, if a file object is provided
      to write to.
    """
    output_file = file or io.StringIO()

    writer = csv.writer(output_file, **kwargs)
    if table.header:
        writer.writerow(table.header)
    writer.writerows(table.body)

    if not file:
        return output_file.getvalue()


def compute_table_widths(rows):
    """Compute the max character widths of a list of rows.

    Args:
      rows: A list of rows, which are sequences of strings.
    Returns:
      A list of integers, the maximum widths required to render the columns of
      this table.
    Raises:
      IndexError: If the rows are of different lengths.
    """
    row_iter = iter(rows)
    first_row = next(row_iter)
    num_columns = len(first_row)
    column_widths = [len(cell) for cell in first_row]
    for row in row_iter:
        for i, cell in enumerate(row):
            if not isinstance(cell, str):
                cell = str(cell)
            cell_len = len(cell)
            if cell_len > column_widths[i]:
                column_widths[i] = cell_len
        if i+1 != num_columns:
            raise IndexError("Invalid number of rows")
    return column_widths


def render_table(table_, output, output_format, css_id=None, css_class=None):
    """Render the given table to the output file object in the requested format.

    The table gets written out to the 'output' file.

    Args:
      table_: An instance of Table.
      output: A file object you can write to.
      output_format: A string, the format to write the table to,
        either 'csv', 'txt' or 'html'.
      css_id: A string, an optional CSS id for the table object (only used for HTML).
      css_class: A string, an optional CSS class for the table object (only used for HTML).
    """
    if output_format in ('txt', 'text'):
        text = table_to_text(table_, "  ", formats={'*': '>', 'account': '<'})
        output.write(text)

    elif output_format in ('csv',):
        table_to_csv(table_, file=output)

    elif output_format in ('htmldiv', 'html'):

        if output_format == 'html':
            output.write('<html>\n')
            output.write('<body>\n')

        output.write('<div id="{}">\n'.format(css_id) if css_id else '<div>\n')
        classes = [css_class] if css_class else None
        table_to_html(table_, file=output, classes=classes)
        output.write('</div>\n')

        if output_format == 'html':
            output.write('</body>\n')
            output.write('</html>\n')

    else:
        raise NotImplementedError("Unsupported format: {}".format(output_format))
