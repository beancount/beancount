"""Print out a list of current holdings, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import argparse
import sys
import textwrap
import io
import logging
import re

from beancount import loader
from beancount.ops import validation
from beancount.reports import rselect
from beancount.reports import table
from beancount.utils import file_utils
from beancount.utils import misc_utils


def get_list_report_string(only_report=None):
    """Return a formatted string for the list of supported reports.

    Args:
      only_report: A string, the name of a single report to produce the help
        for. If not specified, list all the available reports.
    Returns:
      A help string, or None, if 'only_report' was provided and is not a valid
      report name.
    """
    oss = io.StringIO()
    num_reports = 0
    for report_class in rselect.REPORTS:
        # Filter the name
        if only_report and only_report not in report_class.names:
            continue
        name = report_class.names[0]

        # Get the texttual description.
        description = textwrap.fill(
            re.sub(' +', ' ', ' '.join(report_class.__doc__.splitlines())),
            initial_indent="    ",
            subsequent_indent="    ",
            width=80)

        # Get the report's arguments.
        report = report_class()
        parser = argparse.ArgumentParser()
        report.add_args(parser)
        args_str = parser.format_help()

        # Get the list of supported formats.
        formats = report.get_supported_formats()

        oss.write('{}:\n'.format(','.join(report.names)))
        oss.write('  Formats: {}\n'.format(','.join(formats)))
        oss.write('  Description:\n')
        oss.write(description)
        oss.write('\n\n')
        num_reports += 1

    if not num_reports:
        return None
    return oss.getvalue()


class ListReportsAction(argparse.Action):
    """An argparse action that just prints the list of reports and exits."""

    def __call__(self, parser, namespace, values, option_string=None):
        help_string = get_list_report_string(values)
        if values and help_string is None:
            sys.stderr.write("Error: Invalid report name '{}'\n".format(values))
            sys.exit(1)
        else:
            print(help_string)
            sys.exit(0)


def main():
    parser = argparse.ArgumentParser(description=__doc__)

    parser.add_argument('--help-reports', '--list-reports',
                        nargs='?',
                        default=None,
                        action=ListReportsAction,
                        help="Special: Print the full list of supported reports and exit.")

    parser.add_argument('filename',
                        help='The Beancout input filename to load.')

    parser.add_argument('report',
                        help='Name/specification of the desired report.')

    parser.add_argument('-f', '--format', default=None,
                        choices=['text', 'csv', 'html', 'beancount'],
                        help="Output format.")

    parser.add_argument('-o', '--output', action='store',
                        help=("Output filename. If not specified, the output goes "
                              "to stdout. The filename is inspected to select a "
                              "sensible default format, if one is not requested."))

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Print timings.')

    opts, rest_args = parser.parse_known_args()

    # Handle special commands.
    if opts.help_reports:
        print(get_list_report_string())
        return

    # Open output file and guess file format.
    outfile = open(opts.output, 'w') if opts.output else sys.stdout
    opts.format = opts.format or file_utils.guess_file_format(opts.output)




    report = rselect.get_report(opts.report)
    filter_args = report.parse_args(rest_args)
    print('FILTER_ARGS', filter_args)
## FIXME: continue here




    # Dispatch on which report to generate.
    report_function = rselect.get_report_generator(opts.report)
    if report_function is None:
        parser.error("Unknown report.")
    is_check = report_function is rselect.report_validate

    # Force hardcore validations, just for check.
    if is_check:
        validation.VALIDATIONS.extend(validation.HARDCORE_VALIDATIONS)

    if opts.verbose:
        logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    # Parse the input file.
    with misc_utils.log_time('beancount.loader (total)', logging.info):
        entries, errors, options_map = loader.load(opts.filename,
                                                   log_timings=logging.info,
                                                   log_errors=sys.stderr)

    # Create holdings list.
    result = report_function(entries, options_map)

    if isinstance(result, str):
        outfile.write(result)

    elif isinstance(result, table.TableReport):
        # Create the table report.
        table.render_table(result, outfile, opts.format)


if __name__ == '__main__':
    main()
