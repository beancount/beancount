"""
Support for creating Google gviz timeline charts.
"""
__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import io


def gviz_timeline(time_array, data_array_map, css_id='chart'):
    """Create a HTML rendering of the given arrays.

    Args:
      time_array: A sequence of datetime objects.
      data_array_map: A dict or list of items of name to
                      sequence of data points.
      css_id: A string, the CSS id attribute of the target node.
    Returns:
      Javascript code for rendering the chart. (It's up to you to
      insert the a div with the correct CSS id in your accompanying
      HTML page.)
    """
    # Set the order of the data to be output.
    if isinstance(data_array_map, dict):
        data_array_map = list(data_array_map.items())

    # Write preamble.
    oss = io.StringIO()

    oss.write('<script src="https://www.google.com/jsapi" type="text/javascript">'
              '</script>\n')
    oss.write('<script type="text/javascript">\n')

    oss.write("""\
      google.load('visualization', '1', {packages: ['annotatedtimeline']});
      function draw() {
        var data = new google.visualization.DataTable();
      """)

    # Declare columns.
    oss.write("data.addColumn('{}', '{}');\n".format('datetime', 'Time'))
    for name, _ in data_array_map:
        oss.write("data.addColumn('{}', '{}');\n".format('number', name))

    # Render the rows.
    oss.write('data.addRows([\n')

    datalists = [x[1] for x in data_array_map]

    for dtime, datas in zip(time_array, zip(*datalists)):
        js_datetime = ('Date({0.year}, {0.month}, {0.day})').format(dtime)
        oss.write('  [new {}, {}],\n'.format(js_datetime, ', '.join(map(str, datas))))
    oss.write(']);\n')

    oss.write("""
        var annotatedtimeline = new google.visualization.AnnotatedTimeLine(
            document.getElementById('{css_id}')
        );

        var options = {{
          'legendPosition'    : 'newRow',
          'displayAnnotations': true,
        }};

        annotatedtimeline.draw(data, options);
      }}

      google.setOnLoadCallback(draw);
    """.format(css_id=css_id))

    oss.write('</script>\n')

    return oss.getvalue()
