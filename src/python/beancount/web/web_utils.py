"""Web rendering utilities.
"""
import io


def clean_attributes(fieldnames):
    """Convert programming ids into readable field names.

    Args:
      fieldnames: A list of strings, programming ids, such as
        'book_value'.
    Returns:
      A modifeid list of readable strings, such as 'Book Value.'
    """
    return [name.replace('_', ' ').title()
            for name in fieldnames]


# implement formatters parameter.
def render_tuples_to_html_table(rows, header=None, classes=None, file=None):
    """Convert a list of tuples to an HTML table.

    Args:
      rows: A list of tuples.
      header: A list of strings, the names of the fields. If this is
        left as None, no header is rendered.
      file: A file object to write to. If no object is provided, this
        function returns a string.
    Returns:
      A string, the rendered table, or None, if a file object is provided
      to write to.
    """
    oss = io.StringIO() if file is None else file
    oss.write('<table class="{}">\n'.format(' '.join(classes or [])))

    if header:
        oss.write('  <thead>\n')
        oss.write('    <tr>\n')
        for field in header:
            oss.write('      <th>{}</th>\n'.format(field))
        oss.write('    </tr>\n')
        oss.write('  </thead>\n')

    oss.write('  <tbody>\n')
    for row in rows:
        oss.write('    <tr>\n')
        for element in row:
            oss.write('      <td>{}</td>\n'.format(element))
        oss.write('    </tr>\n')
    oss.write('  </tbody>\n')
    oss.write('</table>\n')
    if file is None:
        return oss.getvalue()
