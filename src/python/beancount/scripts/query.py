"""Print out a list of current holdings, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import sys
import textwrap
import io

from beancount import load
from beancount.reports import table
from beancount.utils import file_utils
from beancount.reports import rselect


def get_list_report_string():
    """Return a formatted string for the list of supported reports.

    Returns:
      A help string.
    """
    oss = io.StringIO()
    oss.write("Available Reports:\n")
    for name, args, rclass, formats, description in rselect.get_report_types():
        synopsys = ('{}:{}'.format(name, ','.join(arg.upper() for arg in args))
                    if args
                    else name)
        oss.write("  '{}' [{}]:\n".format(synopsys, ', '.join(formats)))
        oss.write(textwrap.fill(description,
                                initial_indent="    ",
                                subsequent_indent="    "
                            ))
        oss.write("\n\n")
    return oss.getvalue()


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter,
                                     epilog=get_list_report_string())

    parser.add_argument('filename', help='Filename.')
    parser.add_argument('report', help='Name/specification of the desired report.')

    parser.add_argument('-f', '--format', default=None,
                           choices=['text', 'csv', 'html', 'beancount'],
                           help="Output format.")

    parser.add_argument('-o', '--output', action='store',
                           help=("Output filename. If not specified, the output goes "
                                 "to stdout. The filename is inspected to select a "
                                 "sensible default format, if one is not requested."))

    opts = parser.parse_args()

    outfile = open(opts.output, 'w') if opts.output else sys.stdout

    opts.format = opts.format or file_utils.guess_file_format(opts.output)

    # Parse the input file.
    entries, errors, options_map = load(opts.filename, quiet=True)

    # Dispatch on which report to generate.
    report_function = rselect.get_report_generator(opts.report)
    if report_function is None:
        parser.error("Unknown report.")

    # Create holdings list.
    result = report_function(entries, options_map)

    if isinstance(result, str):
        outfile.write(result)

    elif isinstance(result, table.TableReport):
        # Create the table report.
        table.render_table(result, outfile, opts.format)


if __name__ == '__main__':
    main()
