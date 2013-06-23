"""
Support for creating Google gviz timeline charts.
"""
import io
import datetime
import random



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
    for dtime, datas in zip(time_array, zip(x[1] for x in data_array_map)):
        js_datetime = ('Date({0.year}, {0.month}, {0.day}, '
                       '{0.hour}, {0.minute}, {0.second})').format(dtime)
        # oss.write('  [new {}, {}],\n'.format(js_datetime,
        #                                      ', '.join(map(..., series))))
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

    return oss.getvalue()


times = [datetime.datetime(2013, 1, day) for day in range(1, 11)]
data1 = [random.random() for _ in range(10)]
data2 = [random.random() for _ in range(10)]
datas = {'A': data1,
         'B': data2}

javascript = gviz_timeline(times, datas)
print(javascript)
