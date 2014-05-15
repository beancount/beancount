"""Web rendering utilities.
"""
import io


def attribute_to_title(fieldname):
    """Convert programming id into readable field name.

    Args:
      fieldname: A string, a programming ids, such as 'book_value'.
    Returns:
      A readable string, such as 'Book Value.'
   """
    return fieldname.replace('_', ' ').title()


def render_tuples_to_html_table(rows,
                                field_spec=None,
                                classes=None,
                                file=None):
    """Convert a list of tuples to an HTML table.

    Args:
      rows: A list of tuples.
      field_spec: A list of strings, or a list of (strings, header,
        formatter-function) triplets, that selects a subset of the fields is to
        be rendered as well as their ordering. If this is a dict, the values are
        functions to call on the fields to render them. If a function is set to
        None, we will just call str() on the field.
      classes: A list of string, CSS classes to set on the table.
      file: A file object to write to. If no object is provided, this
        function returns a string.
    Returns:
      A string, the rendered table, or None, if a file object is provided
      to write to.
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
                assert isinstance(field, str), field
                new_field_spec.append((field, attribute_to_title(field), None))
        field_spec = new_field_spec

    # Ensure a string header.
    field_spec = [((name, attribute_to_title(name), formatter)
                   if header is None
                   else (name, header, formatter))
                  for (name, header, formatter) in field_spec]

    assert isinstance(field_spec, list), field_spec
    assert all(len(x) == 3 for x in field_spec), field_spec

    # Initialize file.
    oss = io.StringIO() if file is None else file
    oss.write('<table class="{}">\n'.format(' '.join(classes or [])))

    # Render header.
    oss.write('  <thead>\n')
    oss.write('    <tr>\n')
    for name, header, _ in field_spec:
        oss.write('      <th>{}</th>\n'.format(header))
    oss.write('    </tr>\n')
    oss.write('  </thead>\n')

    # Render body.
    oss.write('  <tbody>\n')
    for row in rows:
        oss.write('    <tr>\n')
        for name, _, formatter in field_spec:
            value = getattr(row, name)
            if value is not None:
                if formatter is not None:
                    value = formatter(value)
            else:
                value = ''
            oss.write('      <td>{}</td>\n'.format(value))
        oss.write('    </tr>\n')
    oss.write('  </tbody>\n')

    # Render footer.
    oss.write('</table>\n')
    if file is None:
        return oss.getvalue()
