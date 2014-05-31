"""Print out a list of current holdings, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import sys

from beancount import load
from beancount.reports import table
from beancount.utils import file_utils
from beancount.reports import rselect


def main():
    import argparse
    parser = argparse.ArgumentParser(__doc__)

    parser.add_argument('filename', help='Filename.')
    parser.add_argument('report', help='Name of the desired report.')

    parser.add_argument('-f', '--format', default=None,
                           choices=['txt', 'csv', 'html'],
                           help="Output format.")

    parser.add_argument('-o', '--output', action='store',
                           help="Output filename. If not specified, output goes to stdout.")

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
    table_ = report_function(entries, options_map)

    # Create the table report.
    table.render_table(table_, outfile, opts.format)


if __name__ == '__main__':
    main()
